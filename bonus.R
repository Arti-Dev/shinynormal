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
            label = "Choose Hypothesis Test:",
            choices = c(
              "One-Sample t-Test" = "one_t",
              "Two-Sample t-Test" = "two_t",
              "Proportion Test"   = "prop"
            ),
            selected = "one_t"
          ),
          
          # ---------- One-sample / Two-sample inputs ----------
          conditionalPanel(
            condition = "input.test_type == 'one_t' || input.test_type == 'two_t'",
            textInput(
              "sample_data1",
              label = "Sample 1 Data (comma-separated):",
              value = "12, 15, 14, 16, 13, 14, 15"
            )
          ),
          
          conditionalPanel(
            condition = "input.test_type == 'two_t'",
            textInput(
              "sample_data2",
              label = "Sample 2 Data (comma-separated):",
              value = "11, 12, 10, 14, 13, 12"
            ),
            checkboxInput("welch", "Use Welch's t-test (unequal variances)", value = TRUE)
          ),
          
          conditionalPanel(
            condition = "input.test_type == 'one_t'",
            numericInput("mu0", label = "Null Mean (μ0):", value = 14)
          ),
          
          conditionalPanel(
            condition = "input.test_type == 'two_t'",
            numericInput("diff0", label = "Null Difference (μ1 − μ2):", value = 0)
          ),
          
          # ---------- Proportion test inputs ----------
          conditionalPanel(
            condition = "input.test_type == 'prop'",
            numericInput("x_success", label = "Number of successes (x):", value = 40, min = 0, step = 1),
            numericInput("n_trials",  label = "Number of trials (n):",   value = 50, min = 1, step = 1),
            numericInput("p0",        label = "Null proportion (p0):",   value = 0.75, min = 0, max = 1, step = 0.01)
          ),
          
          numericInput(
            "alpha",
            label = "Significance Level (α):",
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
          p("This app runs common hypothesis tests and displays basic results (test statistic, p-value, confidence interval, and a simple plot)."),
          
          h4("Supported tests"),
          tags$ul(
            tags$li(strong("One-Sample t-Test:"), " compares a sample mean to a hypothesized mean μ0."),
            tags$li(strong("Two-Sample t-Test:"), " compares two independent sample means (μ1 − μ2)."),
            tags$li(strong("Proportion Test:"), " compares an observed proportion x/n to a hypothesized proportion p0 (normal approximation).")
          ),
          
          h4("How to use"),
          tags$ol(
            tags$li("Pick a test type in the sidebar."),
            tags$li("Enter the required inputs (data or x/n and p0)."),
            tags$li("Choose α and click ", strong("Run Test"), "."),
            tags$li("Read the output and decision (reject vs fail to reject).")
          ),
          
          h4("Interpretation (quick)"),
          p("If p-value < α, reject H0. Otherwise, fail to reject H0."),
          p("Confidence intervals are shown at level (1 − α).")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ---------------- Helpers ----------------
  parse_sample <- function(txt) {
    parts <- unlist(strsplit(txt, ","))
    parts <- trimws(parts)
    nums <- suppressWarnings(as.numeric(parts))
    nums <- nums[!is.na(nums)]
    nums
  }
  
  # ---------------- Core computation ----------------
  test_out <- eventReactive(input$run, {
    alpha <- input$alpha
    
    if (input$test_type == "one_t") {
      x <- parse_sample(input$sample_data1)
      if (length(x) < 2) return(list(error = "Please enter at least 2 numeric values for Sample 1."))
      
      tt <- t.test(x, mu = input$mu0, alternative = "two.sided", conf.level = 1 - alpha)
      
      return(list(
        type = "one_t",
        error = NULL,
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
        reject = (unname(tt$p.value) < alpha)
      ))
    }
    
    if (input$test_type == "two_t") {
      x1 <- parse_sample(input$sample_data1)
      x2 <- parse_sample(input$sample_data2)
      if (length(x1) < 2 || length(x2) < 2) {
        return(list(error = "Please enter at least 2 numeric values for BOTH Sample 1 and Sample 2."))
      }
      
      # var.equal is FALSE for Welch; TRUE for pooled
      var_equal <- !isTRUE(input$welch)
      
      tt <- t.test(
        x1, x2,
        alternative = "two.sided",
        mu = input$diff0,
        var.equal = var_equal,
        conf.level = 1 - alpha
      )
      
      return(list(
        type = "two_t",
        error = NULL,
        x1 = x1, x2 = x2,
        diff0 = input$diff0,
        alpha = alpha,
        n1 = length(x1), n2 = length(x2),
        mean1 = mean(x1), mean2 = mean(x2),
        t = unname(tt$statistic),
        df = unname(tt$parameter),
        p = unname(tt$p.value),
        ci = unname(tt$conf.int),
        reject = (unname(tt$p.value) < alpha),
        welch = isTRUE(input$welch)
      ))
    }
    
    if (input$test_type == "prop") {
      x <- as.integer(input$x_success)
      n <- as.integer(input$n_trials)
      p0 <- input$p0
      
      if (is.na(x) || is.na(n) || n <= 0) return(list(error = "Please enter valid integers for x and n (n > 0)."))
      if (x < 0 || x > n) return(list(error = "x must be between 0 and n."))
      if (p0 <= 0 || p0 >= 1) return(list(error = "p0 must be strictly between 0 and 1 for the z-test."))
      
      phat <- x / n
      se <- sqrt(p0 * (1 - p0) / n)
      if (se == 0) return(list(error = "Standard error is 0; check inputs."))
      
      z <- (phat - p0) / se
      pval <- 2 * (1 - pnorm(abs(z)))
      
      # (1 - alpha) CI for p using normal approx with phat
      zcrit <- qnorm(1 - alpha / 2)
      se_ci <- sqrt(phat * (1 - phat) / n)
      ci <- c(phat - zcrit * se_ci, phat + zcrit * se_ci)
      ci <- pmax(0, pmin(1, ci))  # clamp to [0,1] for display
      
      return(list(
        type = "prop",
        error = NULL,
        x = x, n = n, p0 = p0,
        alpha = alpha,
        phat = phat,
        z = z,
        p = pval,
        ci = ci,
        reject = (pval < alpha)
      ))
    }
    
    list(error = "Unknown test type.")
  })
  
  # ---------------- Output text ----------------
  output$results_text <- renderText({
    out <- test_out()
    if (is.null(out)) return("")
    if (!is.null(out$error)) return(out$error)
    
    decision <- if (out$reject) "REJECT H0" else "FAIL TO REJECT H0"
    
    if (out$type == "one_t") {
      return(sprintf(
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
        out$n, out$mean, out$sd,
        out$t, out$df, out$p,
        out$ci[1], out$ci[2],
        decision
      ))
    }
    
    if (out$type == "two_t") {
      test_name <- if (out$welch) "Two-Sample t-test (Welch)" else "Two-Sample t-test (pooled variances)"
      return(sprintf(
        paste0(
          "%s (two-sided)\n",
          "H0: (mu1 - mu2) = %.4f\n",
          "alpha = %.4f\n\n",
          "n1 = %d, mean1 = %.6f\n",
          "n2 = %d, mean2 = %.6f\n\n",
          "t = %.6f, df = %.4f, p-value = %.6f\n",
          "(1 - alpha) CI for (mu1 - mu2): [%.6f, %.6f]\n\n",
          "Decision: %s"
        ),
        test_name,
        out$diff0, out$alpha,
        out$n1, out$mean1,
        out$n2, out$mean2,
        out$t, out$df, out$p,
        out$ci[1], out$ci[2],
        decision
      ))
    }
    
    if (out$type == "prop") {
      return(sprintf(
        paste0(
          "One-Sample Proportion z-test (two-sided)\n",
          "H0: p = %.4f\n",
          "alpha = %.4f\n\n",
          "x = %d successes, n = %d trials\n",
          "phat = x/n = %.6f\n\n",
          "z = %.6f, p-value = %.6f\n",
          "(1 - alpha) CI for p (normal approx): [%.6f, %.6f]\n\n",
          "Decision: %s"
        ),
        out$p0, out$alpha,
        out$x, out$n,
        out$phat,
        out$z, out$p,
        out$ci[1], out$ci[2],
        decision
      ))
    }
    
    "No output."
  })
  
  # ---------------- Plot ----------------
  output$plot_out <- renderPlot({
    out <- test_out()
    if (is.null(out) || !is.null(out$error)) return(NULL)
    
    if (out$type == "one_t") {
      df <- data.frame(x = out$x)
      ggplot(df, aes(x)) +
        geom_histogram(bins = 10) +
        geom_vline(xintercept = out$mu0, linetype = "dashed", linewidth = 1) +
        labs(title = "Sample 1 Distribution", x = "Values", y = "Frequency") +
        theme_minimal()
    } else if (out$type == "two_t") {
      df <- data.frame(
        value = c(out$x1, out$x2),
        group = factor(c(rep("Sample 1", length(out$x1)), rep("Sample 2", length(out$x2))))
      )
      ggplot(df, aes(value)) +
        geom_histogram(bins = 10) +
        facet_wrap(~group, ncol = 1, scales = "free_y") +
        labs(title = "Sample Distributions", x = "Values", y = "Frequency") +
        theme_minimal()
    } else if (out$type == "prop") {
      df <- data.frame(p = out$phat)
      ggplot(df, aes(p)) +
        geom_histogram(bins = 10) +
        geom_vline(xintercept = out$p0, linetype = "dashed", linewidth = 1) +
        xlim(0, 1) +
        labs(title = "Observed Proportion (phat) with p0", x = "Proportion", y = "Frequency") +
        theme_minimal()
    } else {
      NULL
    }
  })
}

shinyApp(ui, server)
