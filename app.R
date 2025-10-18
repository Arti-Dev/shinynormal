library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Normal PDF with Optional Confidence Intervals"),
  sidebarLayout(
    sidebarPanel(
      numericInput("mean", label = "Mean (μ):", value = 0),
      numericInput("sd", label = "Standard Deviation (σ):", value = 1),
      numericInput("rangemin", label = "Range Min:", value = -2),
      numericInput("rangemax", label = "Range Max:", value = 2),
      checkboxGroupInput(
        "cis",
        label = "Show confidence intervals",
        choices = c("90%" = "ci90", "95%" = "ci95"),
        selected = NULL
      )
    ),
    mainPanel(
      plotOutput("graph"),
      verbatimTextOutput("probability"),
      verbatimTextOutput("ci_text")
    )
  )
)

server <- function(input, output, session) {

  prob <- reactive({
    pmin <- pnorm(input$rangemin, mean = input$mean, sd = input$sd)
    pmax <- pnorm(input$rangemax, mean = input$mean, sd = input$sd)
    paste(
      "Probability between", input$rangemin, "and", input$rangemax, ":",
      round(pmax - pmin, 6)
    )
  })

  ci_bounds <- reactive({
    m <- input$mean; s <- input$sd
    list(
      ci90 = c(lower = m - qnorm(0.95)  * s, upper = m + qnorm(0.95)  * s),
      ci95 = c(lower = m - qnorm(0.975) * s, upper = m + qnorm(0.975) * s)
    )
  })

  graph <- reactive({
    x <- seq(input$mean - 6*input$sd, input$mean + 6*input$sd, length.out = 1000)
    y <- dnorm(x, mean = input$mean, sd = input$sd)
    df <- data.frame(x, y)
    bounds <- ci_bounds()

    # Base plot — original blue color
    p <- ggplot(df, aes(x, y)) +
      geom_line(color = "blue", linewidth = 1) +
      geom_area(
        data = subset(df, x >= input$rangemin & x <= input$rangemax),
        aes(y = y), fill = "lightblue", alpha = 0.5
      ) +
      labs(title = "Normal Distribution PDF",
           x = "x", y = "Density") +
      theme_minimal()

    # Add 90% CI (green band + lines)
    if ("ci90" %in% input$cis) {
      ci90_df <- subset(df, x >= bounds$ci90["lower"] & x <= bounds$ci90["upper"])
      p <- p +
        geom_area(data = ci90_df, aes(y = y), fill = "lightgreen", alpha = 0.3) +
        geom_vline(xintercept = bounds$ci90, color = "darkgreen", linetype = "dashed", linewidth = 0.8)
    }

    # Add 95% CI (orange band + lines)
    if ("ci95" %in% input$cis) {
      ci95_df <- subset(df, x >= bounds$ci95["lower"] & x <= bounds$ci95["upper"])
      p <- p +
        geom_area(data = ci95_df, aes(y = y), fill = "orange", alpha = 0.2) +
        geom_vline(xintercept = bounds$ci95, color = "darkorange", linetype = "dotdash", linewidth = 0.8)
    }

    p
  })

  output$probability <- renderText({ prob() })

  output$ci_text <- renderText({
    b <- ci_bounds()
    pieces <- character()
    if ("ci90" %in% input$cis)
      pieces <- c(pieces, sprintf("90%% CI: [%.4f, %.4f]", b$ci90["lower"], b$ci90["upper"]))
    if ("ci95" %in% input$cis)
      pieces <- c(pieces, sprintf("95%% CI: [%.4f, %.4f]", b$ci95["lower"], b$ci95["upper"]))
    if (length(pieces) == 0) "" else paste(pieces, collapse = "   |   ")
  })

  output$graph <- renderPlot({ graph() })
}

shinyApp(ui, server)
