library(shiny)
library(tidyverse)
library(ggiraph)
library(bslib)
library(countrycode)
library(janitor)

options(shiny.sanitize.errors = FALSE)

# =======================================================
# 1. ROBUST DATA PROCESSING
# =======================================================

clean_numeric <- function(x) {
  as.numeric(gsub("[^0-9.-]", "", as.character(x)))
}

# Load Data
exp_df <- read_csv("19-expenditure.csv", na = c("", "-", "..", "n/a"), show_col_types = FALSE)
edu_df <- read_csv("11-education-countries.csv", na = c("", "-", "..", "n/a"), show_col_types = FALSE)

# --- Process Expenditure Data ---
exp_clean <- exp_df %>%
  clean_names() %>%
  rename(country_raw = 1) %>% 
  mutate(
    # Create standardized ISO3 code (e.g., "USA", "CHN")
    iso_code = countrycode(country_raw, origin = "country.name", destination = "iso3c"),
    education_expenditure = clean_numeric(education_expenditure)
  ) %>%
  filter(!is.na(iso_code), !is.na(education_expenditure))

# --- Process Education Data ---
edu_clean <- edu_df %>%
  # 
  select(
    country_raw = 1,
    oos_primary = starts_with("Primary education"), 
    learning_poverty = contains("Learning_Poverty_Total")
  ) %>%
  mutate(
    iso_code = countrycode(country_raw, origin = "country.name", destination = "iso3c"),
    learning_poverty = clean_numeric(learning_poverty)
  ) %>%
  filter(!is.na(iso_code))

# --- Merge and Create Final Dataset ---
final_df <- exp_clean %>%
  # Inner Join on ISO CODE, not name. This fixes spelling mismatches.
  inner_join(edu_clean, by = "iso_code") %>%
  mutate(
    # Get a clean country name and region from the standardized code
    country = countrycode(iso_code, origin = "iso3c", destination = "country.name"),
    region = countrycode(iso_code, origin = "iso3c", destination = "region")
  ) %>%
  filter(!is.na(learning_poverty), !is.na(region))

# --- CONSOLE REPORT (Look at your RStudio Console) ---
cat("\n================ DATA REPORT ================\n")
cat("Countries in Expenditure File: ", nrow(exp_clean), "\n")
cat("Countries in Education File:   ", nrow(edu_clean), "\n")
cat("Countries Successfully Merged: ", nrow(final_df), "\n")
cat("Regions Found: ", paste(unique(final_df$region), collapse = ", "), "\n")
cat("=============================================\n")

# =======================================================
# 2. UI
# =======================================================

ui <- page_sidebar(
  theme = bs_theme(bootswatch = "minty"), 
  title = "Global Education Efficiency",
  
  sidebar = sidebar(
    selectInput("region_select", "Select Region:",
                choices = c("All Regions", sort(unique(final_df$region))),
                selected = "All Regions"),
    checkboxInput("show_trend", "Show Trend Line", value = TRUE),
    hr(),
    helpText("Hover over points to see country details.")
  ),
  
  card(
    card_header("Government Expenditure vs. Learning Poverty"),
    girafeOutput("efficiency_plot", height = "550px")
  )
)

# =======================================================
# 3. SERVER
# =======================================================

server <- function(input, output) {
  
  # Reactive Data Filter
  filtered_data <- reactive({
    req(input$region_select)
    if (input$region_select == "All Regions") {
      final_df
    } else {
      final_df %>% filter(region == input$region_select)
    }
  })
  
  output$efficiency_plot <- renderGirafe({
    dat <- filtered_data()
    
    # Validation to prevent crashes if a region is empty
    validate(
      need(nrow(dat) > 0, "No data available for this region.")
    )
    
    # Dynamic Title
    plot_title <- if(input$region_select == "All Regions") "Global Overview" else input$region_select
    
    p <- ggplot(dat, aes(
      x = education_expenditure, 
      y = learning_poverty, 
      color = region,
      tooltip = paste0("<b>", country, "</b><br>",
                       "Region: ", region, "<br>",
                       "Spend: ", round(education_expenditure, 1), "%<br>",
                       "Poverty: ", round(learning_poverty, 1), "%"),
      data_id = iso_code
    )) +
      geom_point_interactive(size = 4, alpha = 0.8) +
      scale_color_viridis_d(option = "turbo") +
      labs(
        title = plot_title,
        subtitle = "Does spending more reduce learning poverty?",
        x = "Govt Expenditure (% of GDP)",
        y = "Learning Poverty Rate (%)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
    
    if (input$show_trend && nrow(dat) > 2) {
      p <- p + geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", alpha=0.5)
    }
    
    girafe(
      ggobj = p,
      options = list(
        opts_hover(css = "fill:black;stroke:black;r:6pt;"),
        opts_selection(type = "single")
      )
    )
  })
}

shinyApp(ui, server)
