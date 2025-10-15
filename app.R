library(shiny)
library(ggplot2)
ui <- fluidPage(
  sidebarPanel(
    sliderInput("mean", label="Mean (μ):", min=-10, max=10, value=0),
    sliderInput("sd", label="Standard Deviation (σ):", min=0.1, max=5, value=1),
    numericInput("rangemin", label="Range Min:", value=-2),
    numericInput("rangemax", label="Range Max:", value=2),
  ),
  
  mainPanel(
  plotOutput("graph"),
  verbatimTextOutput("probability")
  )
)
server <- function(input, output, session) {
  prob <- reactive({
    pmin = pnorm(input$rangemin, mean = input$mean, sd = input$sd)
    pmax = pnorm(input$rangemax, mean = input$mean, sd = input$sd)
    paste("Probability between", input$rangemin, "and", input$rangemax, ":",
          (pmin - pmax))
  })
  
  graph <- reactive({
    x <- seq(-20, 20, length.out = 1000)
    y <- dnorm(x, mean = input$mean, sd = input$sd)
    df <- data.frame(x, y)
    
    ggplot(df, aes(x, y)) +
      geom_line(color = "blue", linewidth=1) +
      geom_area(data = subset(df, x >= input$rangemin & x <= input$rangemax),
                aes(y = y), fill = "lightblue", alpha = 0.5) +
      labs(title = "Normal Distribution PDF",
           x = "x", y = "Density") +
      theme_minimal()
  })
  
  output$probability <- renderText({
    prob()
  })
  
  output$graph <- renderPlot({
    graph()
  })
}
shinyApp(ui, server)
