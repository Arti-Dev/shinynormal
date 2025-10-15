library(shiny)
library(ggplot2)
ui <- fluidPage(
  sliderInput("mean", label="Mean (μ):", min=-10, max=10, value=0),
  sliderInput("sd", label="Standard Deviation (σ):", min=0.1, max=5, value=1),
  numericInput("rangemin", label="Range Min:", value=-2),
  numericInput("rangemax", label="Range Max:", value=2),
  
  verbatimTextOutput("probability")
)
server <- function(input, output, session) {
  prob <- reactive({
    pmin = pnorm(input$rangemin, mean = input$mean, sd = input$sd)
    pmax = pnorm(input$rangemax, mean = input$mean, sd = input$sd)
    paste("Probability between", input$rangemin, "and", input$rangemax, ":",
          (pmin - pmax))
  })
  
  output$probability <- renderText({
    prob()
  })
}
shinyApp(ui, server)
