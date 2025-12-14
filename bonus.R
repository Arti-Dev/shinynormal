library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Barebones One-Sample t-Test App"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "test_type",
        label = "Choose Hypothesis Test:",
        choices = c("One-Sample t-Test" = "one_t"),
        selected = "one_t"
      ),
      
      textInput(
        "sample_data",
        label = "Enter Sample Data (comma-separated):",
        value = "12, 15, 14, 16, 13, 14, 15"
      ),
      
      numericInput("mu0", label = "Null Mean (μ0):", value = 14),
      
      numericInput("alpha", label = "Significance Level (α):", value = 0.05, min = 0.0001, max = 0.5, step = 0.01),
      
      actionButton("run", "Run Test")
    ),
    
    mainPanel(
      h3("Results"),
      verbatimTextOutput("results_text"),
      plotOutput("dist_plot")
    )
  )
)

server <- function(input, output, session) {
  
  # Parse comma-separated numeric input safely
  parse_sample <- function(txt) {
    parts <- unlist(strsplit(txt, ","))
    parts <- trimws(parts)
    nums <- suppressWarnings(as.numeric(parts))
    nums <- nums[!is.na(nums)]
    nums
  }
  
  # Only run computations when user clicks "Run Test"
  sample_data <- eventReactive(input$run, {
    parse_sample(input$sample_data)
  })
  
  test_out <- eventReactive(input$run, {
    x <- sample_data()
    
    # Minimal input checks
    if (length(x) < 2) {
      return(list(error = "Please enter at least 2 numeric values."))
    }
    
    # Two-sided one-sample t-test (barebones)
    tt <- t.test(x, mu = input$mu0, alternative = "two.sided", conf.level = 1 - input$alpha)
    
    list(
      error = NULL,
      n = length(x),
      mean = mean(x),
      sd = sd(x),
      t = unname(tt$statistic),
      df = unname(tt$parameter),
      p = unname(tt$p.value),
      ci = unname(tt$conf.int),
      reject = (unname(tt$p.value) < input$alpha),
      mu0 = input$mu0,
      alpha = input$alpha,
      x = x
    )
  })
  
  output$results_text <- renderText({
    out <- test_out()
    
    if (!is.null(out$error)) return(out$error)
    
    decision <- if (out$reject) "REJECT H0" else "FAIL TO REJECT H0"
    
    sprintf(
      paste0(
        "One-Sample t-test (two-sided)\n",
        "H0: mu = %.4f\n",
        "alpha = %.4f\n\n",
        "n = %d\n",
        "sample mean = %.6f\n",
        "sample sd   = %.6f\n\n",
        "t = %.6f, df = %.0f, p-value = %.6f\n",
        "(1 - alpha) CI for mu: [%.6f, %.6f]\n\n",
        "Decision: %s"
      ),
      out$mu0, out$alpha,
      out$n,
      out$mean, out$sd,
      out$t, out$df, out$p,
      out$ci[1], out$ci[2],
      decision
    )
  })
  
  output$dist_plot <- renderPlot({
    out <- test_out()
    if (is.null(out) || !is.null(out$error)) return(NULL)
    
    df <- data.frame(x = out$x)
    
    ggplot(df, aes(x)) +
      geom_histogram(bins = 10) +
      geom_vline(xintercept = out$mu0, linetype = "dashed", linewidth = 1) +
      labs(
        title = "Sample Data Distribution",
        x = "Values",
        y = "Frequency"
      ) +
      theme_minimal()
  })
}

shinyApp(ui, server)
