# Install required packages if not already installed
if (!require("plotly", quietly = TRUE)) {
  install.packages("plotly", repos = "https://cloud.r-project.org")
}
if (!require("shinycssloaders", quietly = TRUE)) {
  install.packages("shinycssloaders", repos = "https://cloud.r-project.org")
}
if (!require("shinyjs", quietly = TRUE)) {
  install.packages("shinyjs", repos = "https://cloud.r-project.org")
}

library(shiny)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(shinycssloaders)
library(shinyjs)

## ---------- Read JSON: region-level data ----------

data_raw <- fromJSON("Data/education_data.json")
data <- as_tibble(data_raw$regions)

# Prepare data for second visualization
regions <- data |>
  select(
    id,
    name,
    color,
    learningPoverty,
    spendingGDP
  )

# Color mapping
region_colors <- setNames(regions$color, regions$name)

## ---------- Shiny UI: Tabbed interface for multiple visualizations ----------

ui <- navbarPage(
  title = "Education Data Visualization",
  
  # Add custom CSS for animations and JavaScript
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }
      .fade-in {
        animation: fadeIn 0.6s ease-in;
      }
      #region_plot {
        transition: opacity 0.3s ease-in;
      }
      #difference_display {
        font-size: 14px;
        color: #333;
      }
    ")),
    tags$script(HTML("
      // Custom JavaScript for bar interaction
      $(document).on('shiny:connected', function() {
        // Handled by plotly's built-in click events
      });
    "))
  ),
  
  # Tab 1: Access vs Completion (Li Qi)
  tabPanel(
    "Access vs Completion",
  titlePanel("Access vs Completion"),
  p("Do higher out-of-school rates coincide with lower completion? How strong is the relationship?"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      checkboxGroupInput("incomeLevel", "Filter by Income Level:",
                         choices = unique(data$incomeLevel),
                         selected = unique(data$incomeLevel)),
      sliderInput("spendingRange", "Spending per Pupil ($):",
                  min = 0, max = 15000,
                  value = c(0, 15000), step = 500,
                  pre = "$", sep = ","),
      checkboxInput("showRegression", "Show Regression Line", value = TRUE),
      checkboxInput("showLabels", "Show Region Labels", value = TRUE)
    ),
    
    mainPanel(
      width = 9,
      plotOutput("scatterPlot", height = "500px")
    )
  )
  ),
  
  # Tab 2: Spending vs Learning Poverty
  tabPanel(
    "Spending vs Learning Poverty",
    div(
      class = "fade-in",
      titlePanel("Comparing education spending vs learning poverty by region"),
      sidebarLayout(
        sidebarPanel(
          helpText("Tick regions to compare. Bar width encodes spending (% of GDP); bar height encodes learning poverty."),
          checkboxGroupInput(
            "regions",
            "Regions:",
            choices  = regions$name,
            selected = regions$name[1:3]
          )
        ),
        mainPanel(
          div(
            style = "overflow-x: auto;",
            withSpinner(
              plotlyOutput("region_plot", height = "450px"),
              type = 4,
              color = "#4361ee",
              size = 1.5
            )
          ),
          br(),
          div(
            id = "comparison_display",
            style = "padding: 15px; background-color: #f8f9fa; border-radius: 8px; border: 2px solid #dee2e6; min-height: 60px;",
            htmlOutput("bar_comparison", inline = TRUE)
          )
        )
      )
    )
  )
)

## ---------- Shiny server ----------

server <- function(input, output, session) {

  # ========== Tab 1: Access vs Completion (Li Qi) ==========
  filtered_data <- reactive({
    d <- data[data$incomeLevel %in% input$incomeLevel, ]
    d <- d[d$spendingPerPupil >= input$spendingRange[1] & 
           d$spendingPerPupil <= input$spendingRange[2], ]
    d
  })

  output$scatterPlot <- renderPlot({
    d <- filtered_data()
    if (nrow(d) < 2) return(NULL)
    
    model <- lm(completionRate ~ outOfSchool, data = d)
    r_squared <- summary(model)$r.squared
    slope <- coef(model)[2]
    intercept <- coef(model)[1]
    
    p <- ggplot(d, aes(x = outOfSchool, y = completionRate))
    
    if (input$showRegression) {
      p <- p + geom_smooth(method = "lm", color = "#e63946", linetype = "dashed",
                           fill = "#e63946", alpha = 0.15, se = TRUE)
    }
    
    p <- p +
      geom_point(aes(size = spendingPerPupil, color = color), alpha = 0.8) +
      scale_color_identity() +
      scale_size_continuous(range = c(4, 20),
                            breaks = c(500, 5000, 10000),
                            labels = c("$500", "$5.0k", "$10.0k"),
                            name = "Spending per pupil:") +
      scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 25)) +
      scale_y_continuous(labels = function(y) paste0(y, "%"), limits = c(50, 100)) +
      labs(x = "Out-of-school Rate (% of primary-age children)",
           y = "Primary Completion Rate (%)")
    
    if (input$showLabels) {
      p <- p + geom_text(aes(label = id), vjust = -1.5, size = 3, fontface = "bold")
    }
    
    if (input$showRegression) {
      p <- p +
        annotate("text", x = 18, y = 98,
                 label = paste0("y = ", round(slope, 2), "x + ", round(intercept, 1)),
                 color = "#e63946", fontface = "italic", hjust = 0, size = 4) +
        annotate("text", x = 18, y = 95,
                 label = paste0("R² = ", round(r_squared, 3)),
                 color = "#e63946", fontface = "italic", hjust = 0, size = 4) +
        annotate("label", x = 0.5, y = 54,
                 label = paste0("Strong Correlation:\nEach 1pp increase in out-of-school\nrate associates with ",
                                abs(round(slope, 1)), "pp decrease in\ncompletion (R²=", round(r_squared, 2), ")."),
                 fill = "#fff3cd", color = "#856404", size = 3, hjust = 0, label.size = 0.5)
    }
    
    p + theme_minimal() +
      theme(axis.title = element_text(size = 11), legend.position = "right")
  })
  
  # ========== Tab 2: Spending vs Learning Poverty ==========
  selected_data <- reactive({
    req(input$regions)
    regions |>
      filter(name %in% input$regions) |>
      arrange(learningPoverty) |>
      mutate(region_index = row_number())
  })
  
  # Track selected bars for comparison - store region names instead of indices
  selected_bars <- reactiveValues(bar1_name = NULL, bar2_name = NULL)
  
  # Create a reactive to track clicks more reliably
  click_data <- reactive({
    event_data("plotly_click", source = "region_plot")
  })
  
  # Handle bar clicks with more reliable event handling
  observeEvent(click_data(), {
    click <- click_data()
    
    # Safely check if click data is valid
    if (is.null(click)) {
      return()
    }
    
    # Check if it's a data frame
    if (!inherits(click, "data.frame")) {
      return()
    }
    if (nrow(click) == 0) {
      return()
    }
    
    # Try to get curveNumber and pointNumber
    curve_num <- NULL
    point_num <- NULL
    
    if ("curveNumber" %in% names(click)) {
      curve_num <- click$curveNumber[1]
    }
    if ("pointNumber" %in% names(click)) {
      point_num <- click$pointNumber[1]
    }
    
    # Use curveNumber if available, otherwise use pointNumber
    bar_index <- NULL
    if (!is.null(curve_num) && !is.na(curve_num)) {
      bar_index <- as.integer(curve_num) + 1L  # curveNumber is 0-indexed
    } else if (!is.null(point_num) && !is.na(point_num)) {
      bar_index <- as.integer(point_num) + 1L  # pointNumber is 0-indexed
    }
    
    if (is.null(bar_index) || is.na(bar_index)) {
      return()
    }
    
    dat <- selected_data()
    if (is.null(dat) || nrow(dat) == 0) {
      return()
    }
    
    # Validate bar_index
    if (is.na(bar_index) || bar_index < 1L || bar_index > nrow(dat)) {
      return()
    }
    
    # Get the clicked region name
    clicked_region_name <- dat$name[bar_index]
    
    # If clicking an already selected bar, deselect it
    if (!is.null(selected_bars$bar1_name) && clicked_region_name == selected_bars$bar1_name) {
      # Clicking bar1: remove it, move bar2 to bar1 if it exists
      if (!is.null(selected_bars$bar2_name)) {
        selected_bars$bar1_name <- selected_bars$bar2_name
        selected_bars$bar2_name <- NULL
      } else {
        selected_bars$bar1_name <- NULL
      }
    } else if (!is.null(selected_bars$bar2_name) && clicked_region_name == selected_bars$bar2_name) {
      # Clicking bar2: just remove it
      selected_bars$bar2_name <- NULL
    } else {
      # Clicking a new bar: add it
      if (!is.null(selected_bars$bar1_name) && !is.null(selected_bars$bar2_name)) {
        # Already have two bars: replace the first one
        selected_bars$bar1_name <- selected_bars$bar2_name
        selected_bars$bar2_name <- clicked_region_name
      } else if (!is.null(selected_bars$bar1_name)) {
        # Have one bar: add as second
        selected_bars$bar2_name <- clicked_region_name
      } else {
        # No bars selected: select the first
        selected_bars$bar1_name <- clicked_region_name
      }
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  output$region_plot <- renderPlotly({
    dat <- selected_data()
    req(nrow(dat) > 0)

    # Map spendingGDP to bar width (0.3~0.9)
    dat <- dat |>
      mutate(width = rescale(spendingGDP, to = c(0.3, 0.9)))

    rects <- dat |>
      mutate(
        xmin = region_index - width / 2,
        xmax = region_index + width / 2,
        ymin = 0,
        ymax = learningPoverty
      )

    # Tooltip text: region name + learning poverty + spending%
    rects <- rects |>
      mutate(
        tooltip = paste0(
          "Region: ", name,
          "<br>Learning poverty: ", round(learningPoverty, 1), "%", 
          "<br>Spending: ", round(spendingGDP, 1), "% of GDP",
          "<br><i>Click to select for comparison</i>"
        )
      )

    # Get the range of learning poverty for color scale
    lp_min <- min(rects$learningPoverty)
    lp_max <- max(rects$learningPoverty)
    
    # Add selection status to rects - use region names for matching
    selected_names <- c()
    if (!is.null(selected_bars$bar1_name)) selected_names <- c(selected_names, selected_bars$bar1_name)
    if (!is.null(selected_bars$bar2_name)) selected_names <- c(selected_names, selected_bars$bar2_name)
    
    rects <- rects |>
      mutate(
        is_selected = name %in% selected_names
      )

    # Create plot using plotly native API
    # Color function for learning poverty
    get_color <- function(lp) {
      ratio <- (lp - lp_min) / (lp_max - lp_min)
      ratio <- max(0, min(1, ratio))  # Clamp between 0 and 1
      r <- round(58 + (230 - 58) * ratio)   # 58 (0x3a) to 230 (0xe6)
      g <- round(134 + (57 - 134) * ratio)  # 134 (0x86) to 57 (0x39)
      b <- round(255 + (70 - 255) * ratio)  # 255 (0xff) to 70 (0x46)
      sprintf("rgb(%d,%d,%d)", r, g, b)
    }
    
    # Create plotly chart using bar chart - each bar as separate trace
    p_plotly <- plot_ly(source = "region_plot")
    
    # Add each bar as a separate bar trace
    for (i in seq_len(nrow(rects))) {
      rect <- rects[i, ]
      bar_color <- get_color(rect$learningPoverty)
      border_color <- if (rect$is_selected) "#ffc107" else "white"  # 更亮的金黄色
      border_width <- if (rect$is_selected) 6 else 1  # 更粗的边框
      
      p_plotly <- p_plotly |>
        add_trace(
          x = rect$region_index,
          y = rect$learningPoverty,
          width = rect$width,
          type = "bar",
          base = 0,
          marker = list(
            color = bar_color,
            line = list(
              color = border_color,
              width = border_width
            ),
            opacity = 0.9
          ),
          text = "",  # No text on bars
          textposition = "none",
          hovertext = rect$tooltip,
          hoverinfo = "text",
          name = rect$name,
          showlegend = FALSE
        )
    }
    
    # Configure layout - no dragmode
    p_plotly <- p_plotly |>
      layout(
        xaxis = list(
          title = "Selected regions",
          tickmode = "array",
          tickvals = rects$region_index,
          ticktext = rects$name
        ),
        yaxis = list(
          title = "Learning poverty (%)",
          tickformat = ".0f"
        ),
        barmode = "overlay",
        showlegend = FALSE,
        clickmode = "event",
        dragmode = FALSE,
        transition = list(
          duration = 800,
          easing = "cubic-in-out"
        ),
        annotations = list(
          list(
            text = "Bar width encodes education spending as % of GDP; bar height encodes learning poverty. Color indicates severity of learning poverty. Click bars to compare.",
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = -0.12,
            xanchor = "center",
            yanchor = "top",
            font = list(size = 10)
          )
        )
      ) |>
      config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c("lasso2d", "select2d", "pan2d", "zoom2d")
      )
    
    # Add colorbar by creating a dummy trace with color scale
    p_plotly <- p_plotly |>
      add_trace(
        x = rep(max(rects$region_index) + 0.5, 2),
        y = c(lp_min, lp_max),
        type = "scatter",
        mode = "markers",
        marker = list(
          size = 0,
          color = c(lp_min, lp_max),
          colorscale = list(
            list(0, "#3a86ff"),
            list(1, "#e63946")
          ),
          cmin = lp_min,
          cmax = lp_max,
          showscale = TRUE,
          colorbar = list(
            title = "Learning Poverty<br>Severity",
            len = 0.5,
            y = 0.75,
            tickformat = ".0f"
          )
        ),
        showlegend = FALSE,
        hoverinfo = "skip"
      )
    
    p_plotly
  })
  
  # Display comparison between selected bars
  output$bar_comparison <- renderText({
    if (is.null(selected_bars$bar1_name)) {
      return("<em style='color: #6c757d;'>Click on two bars to compare their learning poverty values.</em>")
    }
    
    dat <- selected_data()
    req(nrow(dat) > 0)
    
    # Find bar1 by name
    bar1_data <- dat[dat$name == selected_bars$bar1_name, ]
    if (nrow(bar1_data) == 0) {
      return("<em style='color: #6c757d;'>Invalid selection. Please click on a bar again.</em>")
    }
    
    bar1_name <- bar1_data$name
    bar1_lp <- bar1_data$learningPoverty
    bar1_spending <- bar1_data$spendingGDP
    
    if (is.null(selected_bars$bar2_name)) {
      return(paste0(
        "<div style='display: flex; align-items: center; gap: 10px;'>",
        "<div style='flex: 1; padding: 10px; background-color: #e8f5e9; border-radius: 5px; border-left: 4px solid #4caf50;'>",
        "<strong style='color: #2e7d32;'>Selected:</strong> ", bar1_name, "<br>",
        "<span style='font-size: 18px; font-weight: bold; color: #2e7d32;'>", round(bar1_lp, 1), "%</span> Learning Poverty<br>",
        "<span style='color: #666;'>Spending: ", round(bar1_spending, 1), "% of GDP</span>",
        "</div>",
        "<div style='color: #6c757d; font-style: italic;'>Click another bar to compare →</div>",
        "</div>"
      ))
    }
    
    # Find bar2 by name
    bar2_data <- dat[dat$name == selected_bars$bar2_name, ]
    if (nrow(bar2_data) == 0) {
      return(paste0(
        "<div style='display: flex; align-items: center; gap: 10px;'>",
        "<div style='flex: 1; padding: 10px; background-color: #e8f5e9; border-radius: 5px; border-left: 4px solid #4caf50;'>",
        "<strong style='color: #2e7d32;'>Selected:</strong> ", bar1_name, "<br>",
        "<span style='font-size: 18px; font-weight: bold; color: #2e7d32;'>", round(bar1_lp, 1), "%</span> Learning Poverty<br>",
        "<span style='color: #666;'>Spending: ", round(bar1_spending, 1), "% of GDP</span>",
        "</div>",
        "<div style='color: #6c757d; font-style: italic;'>Click another bar to compare →</div>",
        "</div>"
      ))
    }
    bar2_name <- bar2_data$name
    bar2_lp <- bar2_data$learningPoverty
    bar2_spending <- bar2_data$spendingGDP
    
    difference <- abs(bar1_lp - bar2_lp)
    direction <- ifelse(bar2_lp > bar1_lp, "higher", "lower")
    spending_diff <- abs(bar1_spending - bar2_spending)
    
    # Determine which bar has higher learning poverty
    higher_bar <- ifelse(bar2_lp > bar1_lp, bar2_name, bar1_name)
    lower_bar <- ifelse(bar2_lp > bar1_lp, bar1_name, bar2_name)
    
    # Get color function (same as in the plot)
    lp_min <- min(dat$learningPoverty)
    lp_max <- max(dat$learningPoverty)
    get_color <- function(lp) {
      ratio <- (lp - lp_min) / (lp_max - lp_min)
      ratio <- max(0, min(1, ratio))
      r <- round(58 + (230 - 58) * ratio)
      g <- round(134 + (57 - 134) * ratio)
      b <- round(255 + (70 - 255) * ratio)
      sprintf("rgb(%d,%d,%d)", r, g, b)
    }
    
    # Get colors for both bars
    bar1_color <- get_color(bar1_lp)
    bar2_color <- get_color(bar2_lp)
    
    # Use very light background with colored border for better text readability
    # Background is mostly white with a slight tint of the bar color
    rgb_to_light_bg <- function(rgb_str) {
      # Extract RGB values
      rgb_vals <- as.numeric(strsplit(gsub("rgb\\(|\\)", "", rgb_str), ",")[[1]])
      # Very light background (30% original + 70% white) for maximum readability
      r <- round(rgb_vals[1] * 0.3 + 255 * 0.7)
      g <- round(rgb_vals[2] * 0.3 + 255 * 0.7)
      b <- round(rgb_vals[3] * 0.3 + 255 * 0.7)
      sprintf("rgb(%d,%d,%d)", r, g, b)
    }
    
    bar1_bg_color <- rgb_to_light_bg(bar1_color)
    bar2_bg_color <- rgb_to_light_bg(bar2_color)
    
    result <- paste0(
      "<div style='display: flex; gap: 15px; align-items: stretch;'>",
      # Bar 1 - left box with left border (using bar color)
      "<div style='flex: 1; padding: 12px; background-color: ", bar1_bg_color, "; border-radius: 5px; border-left: 4px solid ", bar1_color, ";'>",
      "<strong style='color: #000; font-size: 15px; font-weight: bold;'>", bar1_name, "</strong><br>",
      "<span style='font-size: 22px; font-weight: bold; color: ", bar1_color, "; text-shadow: 0 0 1px rgba(0,0,0,0.1);'>", round(bar1_lp, 1), "%</span><br>",
      "<span style='color: #000; font-size: 12px; font-weight: 500;'>Learning Poverty</span><br>",
      "<span style='color: #333; font-size: 11px;'>Spending: ", round(bar1_spending, 1), "% GDP</span>",
      "</div>",
      # Comparison
      "<div style='flex: 0 0 auto; display: flex; flex-direction: column; justify-content: center; align-items: center; padding: 0 10px;'>",
      "<div style='font-size: 24px; font-weight: bold; color: #e63946;'>", round(difference, 1), "</div>",
      "<div style='font-size: 11px; color: #666; text-align: center;'>percentage<br>points<br>difference</div>",
      "</div>",
      # Bar 2 - right box with right border (using bar color)
      "<div style='flex: 1; padding: 12px; background-color: ", bar2_bg_color, "; border-radius: 5px; border-right: 4px solid ", bar2_color, ";'>",
      "<strong style='color: #000; font-size: 15px; font-weight: bold;'>", bar2_name, "</strong><br>",
      "<span style='font-size: 22px; font-weight: bold; color: ", bar2_color, "; text-shadow: 0 0 1px rgba(0,0,0,0.1);'>", round(bar2_lp, 1), "%</span><br>",
      "<span style='color: #000; font-size: 12px; font-weight: 500;'>Learning Poverty</span><br>",
      "<span style='color: #333; font-size: 11px;'>Spending: ", round(bar2_spending, 1), "% GDP</span>",
      "</div>",
      "</div>",
      "<div style='margin-top: 10px; padding: 8px; background-color: #fff3cd; border-radius: 4px; border-left: 3px solid #ffc107;'>",
      "<strong>", higher_bar, "</strong> has <strong>", round(difference, 1), " percentage points</strong> ",
      "<strong style='color: #e63946;'>", direction, "</strong> learning poverty than <strong>", lower_bar, "</strong>",
      if (spending_diff > 0.1) {
        paste0(" (Spending difference: ", round(spending_diff, 1), "% of GDP)")
      } else {
        ""
      },
      "</div>"
    )
    
    return(result)
  })
  
}

shinyApp(ui = ui, server = server)
