library(shiny)
library(ggplot2)
library(jsonlite)

data <- fromJSON("Data/education_data.json")$regions

ui <- fluidPage(
  
  # Li Qi: Access vs Completion
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
  # End Li Qi
  
)

server <- function(input, output, session) {

  # Li Qi: Access vs Completion
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
  # End Li Qi
  
}

shinyApp(ui = ui, server = server)
