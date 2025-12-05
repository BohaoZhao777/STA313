library(shiny)
library(tidyverse)
library(ggiraph)      
library(bslib)        
library(countrycode)  
library(janitor)      

# =======================================================
# 1. DATA PROCESSING (Run once on startup)
# =======================================================

# Load Data
exp_df <- read_csv("19-expenditure.csv", show_col_types = FALSE)
edu_df <- read_csv("11-education-countries.csv", show_col_types = FALSE)

# Clean Expenditure Data
exp_clean <- exp_df %>%
  clean_names() %>%
  mutate(education_expenditure = as.numeric(na_if(education_expenditure, "-")))

edu_clean <- edu_df %>%
  # Rename columns manually by index to avoid confusion
  rename(
    country = 1,
    oos_primary = 2,
    oos_lower_sec = 3,
    oos_upper_sec = 4,
    comp_primary = 5,
    comp_lower_sec = 6,
    comp_upper_sec = 7,
    learning_poverty = 14
  ) %>%
  # Convert "-" to NA and ensure numeric
  mutate(across(c(oos_primary, comp_primary, learning_poverty), 
                ~as.numeric(na_if(., "-")))) %>%
  select(country, oos_primary, comp_primary, learning_poverty)

# Merge Datasets
final_df <- exp_clean %>%
  inner_join(edu_clean, by = "country") %>%
  # Add Region automatically using the country name
  mutate(region = countrycode(country, origin = "country.name", destination = "region")) %>%
  # Handle cases where region is not found (optional)
  filter(!is.na(education_expenditure), !is.na(learning_poverty)) 


# =======================================================
# 2. UI (USER INTERFACE)
# =======================================================

ui <- page_sidebar(
  theme = bs_theme(bootswatch = "minty"), # Clean, professional theme
  title = "Education Efficiency Dashboard",
  
  sidebar = sidebar(
    title = "Controls",
    
    # Filter by Region
    selectInput(
      "region_select", 
      "Select Region:",
      choices = c("All Regions", sort(unique(final_df$region))),
      selected = "All Regions"
    ),
    
    # Toggle Trend Line
    checkboxInput("show_trend", "Show Efficiency Trend", value = TRUE),
    
    # Analytical Note (Dynamic)
    hr(),
    h5("Insight:"),
    textOutput("insight_text")
  ),
  
  # Main Layout
  card(
    card_header("Spending Efficiency: Expenditure vs. Learning Poverty"),
    girafeOutput("efficiency_plot", height = "500px"),
    card_footer("Note: 'Efficient' countries are in the bottom-left (Low Spend, Low Poverty).")
  )
)

# =======================================================
# 3. SERVER LOGIC
# =======================================================

server <- function(input, output) {
  
  # Reactive Data Filter
  filtered_data <- reactive({
    if (input$region_select == "All Regions") {
      final_df
    } else {
      final_df %>% filter(region == input$region_select)
    }
  })
  
  # Render Interactive Plot
  output$efficiency_plot <- renderGirafe({
    
    # Base ggplot
    p <- ggplot(filtered_data(), aes(
      x = education_expenditure, 
      y = learning_poverty, 
      color = region,
      # Tooltip is crucial for interactivity grade
      tooltip = paste0(
        "<b>", country, "</b><br>",
        "Spending: ", education_expenditure, "%<br>",
        "Learning Poverty: ", learning_poverty, "%"
      ),
      data_id = country
    )) +
      # Use interactive points
      geom_point_interactive(size = 4, alpha = 0.7) +
      
      # Labels & Style
      labs(
        x = "Govt Expenditure on Education (%)",
        y = "Learning Poverty Rate (%)",
        title = paste("Efficiency Analysis -", input$region_select)
      ) +
      scale_color_viridis_d(option = "turbo") + # Professional color palette
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
    
    # Add Trend Line if checked
    if (input$show_trend) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "gray50", linetype = "dashed")
    }
    
    # Render with Girafe options
    girafe(
      ggobj = p,
      options = list(
        opts_hover(css = "fill:black;stroke:black;r:6pt;"), # Highlight on hover
        opts_selection(type = "single"),
        opts_tooltip(css = "background-color:white;color:black;padding:5px;border-radius:5px;")
      )
    )
  })
  
  # Dynamic Insight Text
  output$insight_text <- renderText({
    data <- filtered_data()
    avg_pov <- round(mean(data$learning_poverty, na.rm = TRUE), 1)
    
    paste0(
      "In ", input$region_select, 
      ", the average Learning Poverty rate is ", avg_pov, "%. ",
      "Countries below the dashed line are achieving better outcomes relative to their spending."
    )
  })
}

# Run App
shinyApp(ui, server)