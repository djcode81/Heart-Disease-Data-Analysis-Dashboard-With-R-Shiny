library(shiny)
library(ggplot2)


ui <- fluidPage(
  titlePanel("Heart Disease Data Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Variable:", 
                  choices = c("Age" = "age", "Cholesterol" = "chol"))  # Remove Analyze button
    ),
    mainPanel(
      plotOutput("histPlot"),
      plotOutput("correlationPlot"),  
      verbatimTextOutput("summaryTable") 
    )
  )
)

# Server
server <- function(input, output) {
  
  data <- read.csv("heart.csv")
  
  
  output$histPlot <- renderPlot({
    hist(data[[input$variable]], 
         main = paste("Histogram of", input$variable),
         xlab = input$variable)
  })
  
 
  output$correlationPlot <- renderPlot({
    clean_data <- data[!is.na(data$trestbps) & !is.na(data$num), ]  
    
    ggplot(clean_data, aes(x = trestbps, y = factor(num))) +
      geom_jitter(width = 0.2, aes(color = factor(num))) +  
      geom_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x) +  
      labs(x = "Resting Blood Pressure", y = "Heart Disease Status (0 = No, 1 = Yes)", 
           title = "Correlation Between Resting Blood Pressure and Heart Disease") +
      theme_minimal()
  })
  
  
  output$summaryTable <- renderPrint({
    var_data <- data[[input$variable]]  
    
    if (is.numeric(var_data)) {
      summary(var_data)  
    } else if (is.factor(var_data) || is.character(var_data)) {
      table(var_data)  
    } else {
      
    }
  })
}


shinyApp(ui = ui, server = server)
