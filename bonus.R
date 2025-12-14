library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Interactive Hypothesis Testing App"),
  tabsetPanel(
    tabPanel(
      "Results",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "test_type",
            "Choose Hypothesis Test:",
            choices = c(
              "One-Sample t-Test" = "one_t",
              "Two-Sample t-Test" = "two_t",
              "Proportion Test"   = "prop"
            ),
            selected = "one_t"
          ),
          
          conditionalPanel(
            condition = "input.test_type == 'one_t' || input.test_type == 'two_t'",
            textInput(
              "sample_data1",
              "Sample 1 Data (comma-separated):",
              value = "12, 15, 14, 16, 13, 14, 15"
            )
          ),
          
          conditionalPanel(
            condition = "input.test_type == 'two_t'",
            textInput(
              "sample_data2",
              "Sample 2 Data (comma-separated):",
              value = "11, 12, 10, 14, 13, 12"
            ),
            checkboxInput("welch", "Use Welch's t-test (unequal variances)", value = TRUE)
          ),
          
          conditionalPanel(
            condition = "input.test_type == 'one_t'",
            numericInput("mu0", "Null Mean (μ0):", value = 14)
          ),
          
          conditionalPanel(
            condition = "input.test_type == 'two_t'",
            numericInput("diff0", "Null Difference (μ1 − μ2):", value = 0)
          ),
          
          conditionalPanel(
            condition = "input.test_type == 'prop'",
            numericInput("x_success", "Number of successes (x):", value = 40, min = 0, step = 1),
            numericInput("n_trials", "Number of trials (n):", value = 50, min = 1, step = 1),
            numericInput("p0", "Null proportion (p0):", value = 0.75, min = 0, max = 1, step = 0.01)
          ),
          
          numericInput(
            "alpha",
            "Significance Level (α):",
            value = 0.05,
            min = 0.0001,
            max = 0.5,
            step = 0.01
          ),
          
          actionButton("run", "Run Test")
        ),
        
        mainPanel(
          h3("Test Results"),
          verbatimTextOutput("results_text"),
          plotOutput("plot_out")
        )
      )
    ),
    
    tabPanel(
      "Help",
      fluidRow(
        column(
          10,
          h3("What is this app?"),
          p("This app runs common hypothesis tests and displays basic results."),
          h4("Supported tests"),
          tags$ul(
            tags$li(strong("One-Sample t-Test:"), " compares a sample mean to a hypothesized mean μ0."),
            tags$li(strong("Two-Sample t-Test:"), " compares two independent sample means."),
            tags$li(strong("Proportion Test:"), " compares an observed proportion to a hypothesized proportion.")
          ),
          h4("How to use"),
          tags$ol(
            tags$li("Pick a test type."),
            tags$li("Enter inputs."),
            tags$li("Choose α and click Run Test."),
            tags$li("Interpret the results.")
          ),
          h4("Interpretation"),
          p("If p-value < α, reject H0. Otherwise, fail to reject H0.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  parse_sample <- function(txt) {
    parts <- unlist(strsplit(txt, ","))
    parts <- trimws(parts)
    nums <- suppressWarnings(as.numeric(parts))
    nums[!is.na(nums)]
  }
  
  test_out <- eventReactive(input$run, {
    alpha <- input$alpha
    
    if (input$test_type == "one_t") {
      x <- parse_sample(input$sample_data1)
      if (length(x) < 2) return(list(error = "Please enter at least 2 numeric values."))
      
      tt <- t.test(x, mu = input$mu0, conf.level = 1 - alpha)
      
      return(list(
        type = "one_t",
        x = x,
        mu0 = input$mu0,
        alpha = alpha,
        n = length(x),
        mean = mean(x),
        sd = sd(x),
        t = unname(tt$statistic),
        df = unname(tt$parameter),
        p = unname(tt$p.value),
        ci = unname(tt$conf.int),
        reject = tt$p.value < alpha
      ))
    }
    
    if (input$test_type == "two_t") {
      x1 <- parse_sample(input$sample_data1)
      x2 <- parse_sample(input$sample_data2)
      if (length(x1) < 2 || length(x2) < 2) return(list(error = "Please enter valid data for both samples."))
      
      tt <- t.test(
        x1, x2,
        mu = input$diff0,
        var.equal = !input$welch,
        conf.level = 1 - alpha
      )
      
      return(list(
        type = "two_t",
        x1 = x1,
        x2 = x2,
        diff0 = input$diff0,
        alpha = alpha,
        n1 = length(x1),
        n2 = length(x2),
        mean1 = mean(x1),
        mean2 = mean(x2),
        t = unname(tt$statistic),
        df = unname(tt$parameter),
        p = unname(tt$p.value),
        ci = unname(tt$conf.int),
        reject = tt$p.value < alpha,
        welch = input$welch
      ))
    }
    
    if (input$test_type == "prop") {
      x <- as.integer(input$x_success)
      n <- as.integer(input$n_trials)
      p0 <- input$p0
      
      phat <- x / n
      se <- sqrt(p0 * (1 - p0) / n)
      z <- (phat - p0) / se
      pval <- 2 * (1 - pnorm(abs(z)))
      
      zcrit <- qnorm(1 - alpha / 2)
      se_ci <- sqrt(phat * (1 - phat) / n)
      ci <- pmax(0, pmin(1, c(phat - zcrit * se_ci, phat + zcrit * se_ci)))
      
      return(list(
        type = "prop",
        x = x,
        n = n,
        p0 = p0,
        alpha = alpha,
        phat = phat,
        z = z,
        p = pval,
        ci = ci,
        reject = pval < alpha
      ))
    }
    
    list(error = "Invalid test.")
  })
  
  output$results_text <- renderText({
    out <- test_out()
    if (is.null(out) || !is.null(out$error)) return(out$error)
    
    decision <- if (out$reject) "REJECT H0" else "FAIL TO REJECT H0"
    
    if (out$type == "one_t") {
      sprintf(
        "One-Sample t-test\nH0: mu = %.4f\nalpha = %.4f\n\nn = %d\nmean = %.6f\nsd = %.6f\n\nt = %.6f, df = %.0f, p = %.6f\nCI: [%.6f, %.6f]\n\nDecision: %s",
        out$mu0, out$alpha, out$n, out$mean, out$sd,
        out$t, out$df, out$p, out$ci[1], out$ci[2], decision
      )
    } else if (out$type == "two_t") {
      sprintf(
        "Two-Sample t-test\nH0: mu1 - mu2 = %.4f\nalpha = %.4f\n\nmean1 = %.6f\nmean2 = %.6f\n\nt = %.6f, df = %.4f, p = %.6f\nCI: [%.6f, %.6f]\n\nDecision: %s",
        out$diff0, out$alpha, out$mean1, out$mean2,
        out$t, out$df, out$p, out$ci[1], out$ci[2], decision
      )
    } else {
      sprintf(
        "Proportion z-test\nH0: p = %.4f\nalpha = %.4f\n\nphat = %.6f\nz = %.6f, p = %.6f\nCI: [%.6f, %.6f]\n\nDecision: %s",
        out$p0, out$alpha, out$phat, out$z, out$p,
        out$ci[1], out$ci[2], decision
      )
    }
  })
  
  output$plot_out <- renderPlot({
    out <- test_out()
    if (is.null(out) || !is.null(out$error)) return(NULL)
    
    if (out$type == "one_t") {
      ggplot(data.frame(x = out$x), aes(x)) +
        geom_histogram(bins = 10) +
        geom_vline(xintercept = out$mu0, linetype = "dashed") +
        theme_minimal()
    } else if (out$type == "two_t") {
      df <- data.frame(
        value = c(out$x1, out$x2),
        group = factor(c(rep("Sample 1", length(out$x1)), rep("Sample 2", length(out$x2))))
      )
      ggplot(df, aes(value)) +
        geom_histogram(bins = 10) +
        facet_wrap(~group, scales = "free_y") +
        theme_minimal()
    } else {
      df <- data.frame(
        label = factor(c("Observed (p̂)", "Null (p0)")),
        value = c(out$phat, out$p0)
      )
      ggplot(df, aes(label, value)) +
        geom_col(width = 0.6) +
        coord_cartesian(ylim = c(0, 1)) +
        theme_minimal()
    }
  })
}

shinyApp(ui, server)
