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
  
  z_of <- function(x, m, s) (x - m) / s
  
  prob <- reactive({
    if (input$sd <= 0) return("σ must be > 0.")
    pmin <- pnorm(input$rangemin, mean = input$mean, sd = input$sd)
    pmax <- pnorm(input$rangemax, mean = input$mean, sd = input$sd)
    zmin <- z_of(input$rangemin, input$mean, input$sd)  # NEW
    zmax <- z_of(input$rangemax, input$mean, input$sd)  # NEW
    paste0(
      "P(", input$rangemin, " ≤ X ≤ ", input$rangemax, ") = ",
      round(pmax - pmin, 6),
      "   |   z-range: [", round(zmin, 4), ", ", round(zmax, 4), "]" # NEW
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
    if (input$sd <= 0) {
      return(ggplot() + theme_void() + ggtitle("σ must be > 0")) # NEW
    }
    x <- seq(input$mean - 6*input$sd, input$mean + 6*input$sd, length.out = 1000)
    y <- dnorm(x, mean = input$mean, sd = input$sd)
    df <- data.frame(x, y)
    bounds <- ci_bounds()
    
    p <- ggplot(df, aes(x, y)) +
      geom_line(color = "blue", linewidth = 1) +
      geom_area(
        data = subset(df, x >= input$rangemin & x <= input$rangemax),
        aes(y = y), fill = "lightblue", alpha = 0.5
      ) +
      labs(title = "Normal Distribution PDF", x = "x", y = "Density") +
      scale_x_continuous(
        sec.axis = sec_axis(~ (. - input$mean) / input$sd, name = "Z = (x - μ) / σ")
      ) +
      theme_minimal()
    
    if ("ci90" %in% input$cis) {
      ci90_df <- subset(df, x >= bounds$ci90["lower"] & x <= bounds$ci90["upper"])
      p <- p +
        geom_area(data = ci90_df, aes(y = y), fill = "lightgreen", alpha = 0.3) +
        geom_vline(xintercept = bounds$ci90, color = "darkgreen",
                   linetype = "dashed", linewidth = 0.8)
    }
    
    if ("ci95" %in% input$cis) {
      ci95_df <- subset(df, x >= bounds$ci95["lower"] & x <= bounds$ci95["upper"])
      p <- p +
        geom_area(data = ci95_df, aes(y = y), fill = "orange", alpha = 0.2) +
        geom_vline(xintercept = bounds$ci95, color = "darkorange",
                   linetype = "dotdash", linewidth = 0.8)
    }
    
    p
  })
  
  output$probability <- renderText({ prob() })
  
  output$ci_text <- renderText({
    if (input$sd <= 0) return("")
    b <- ci_bounds()
    parts <- character()
    if ("ci90" %in% input$cis) {
      parts <- c(parts, sprintf(
        "90%% CI (x): [%.4f, %.4f]   |   90%% CI (z): [%.4f, %.4f]",
        b$ci90["lower"], b$ci90["upper"],
        z_of(b$ci90["lower"], input$mean, input$sd),
        z_of(b$ci90["upper"], input$mean, input$sd)
      ))
    }
    if ("ci95" %in% input$cis) {
      parts <- c(parts, sprintf(
        "95%% CI (x): [%.4f, %.4f]   |   95%% CI (z): [%.4f, %.4f]",
        b$ci95["lower"], b$ci95["upper"],
        z_of(b$ci95["lower"], input$mean, input$sd),
        z_of(b$ci95["upper"], input$mean, input$sd)
      ))
    }
    if (length(parts) == 0) "" else paste(parts, collapse = "   |   ")
  })
  
  output$graph <- renderPlot({ graph() })
}

shinyApp(ui, server)
