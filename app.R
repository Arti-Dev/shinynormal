library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Normal PDF with Optional Confidence Intervals"),
  sidebarLayout(
    sidebarPanel(
      numericInput("mean", label = "Mean (μ):", value = 0),
      numericInput("sd", label = "Standard Deviation (σ):", value = 1, min = 0.0001, step = 0.1),
      
      selectInput(
        "mode",
        label = "Probability region",
        choices = c("Less than a value" = "lt",
                    "Greater than a value" = "gt",
                    "Between two values" = "between",
                    "Outside two values" = "outside"),
        selected = "between"
      ),
      
      numericInput("rangemin", label = "Range Min:", value = -2),
      
      conditionalPanel(
        condition = "input.mode == 'between' || input.mode == 'outside'",
        numericInput("rangemax", label = "Range Max:", value = 2)
      ),
      
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
      
      conditionalPanel(
        condition = "input.cis && input.cis.indexOf('ci90') > -1",
        wellPanel(
          h4("90% Confidence Interval"),
          verbatimTextOutput("ci90_text")
        )
      ),
      conditionalPanel(
        condition = "input.cis && input.cis.indexOf('ci95') > -1",
        wellPanel(
          h4("95% Confidence Interval"),
          verbatimTextOutput("ci95_text")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$mode, {
    if (input$mode == "lt") {
      updateNumericInput(session, "rangemin", label = "Cutoff (x0): P(X < x0)")
    } else if (input$mode == "gt") {
      updateNumericInput(session, "rangemin", label = "Cutoff (x0): P(X > x0)")
    } else {
      updateNumericInput(session, "rangemin", label = "Range Min:")
      updateNumericInput(session, "rangemax", label = "Range Max:")
    }
  }, ignoreInit = TRUE)
  
  z_of <- function(x, m, s) (x - m) / s
  tail_kind <- reactive({
    if (input$mode %in% c("lt", "gt")) "one" else "two"
  })
  range_pair <- reactive({
    a <- input$rangemin
    b <- if (!is.null(input$rangemax)) input$rangemax else a
    if (a <= b) c(a, b) else c(b, a)
  })
  
  prob <- reactive({
    if (input$sd <= 0) return("σ must be > 0.")
    m <- input$mean; s <- input$sd
    a <- input$rangemin
    ab <- range_pair(); lo <- ab[1]; hi <- ab[2]
    
    switch(
      input$mode,
      lt = {
        p <- pnorm(a, mean = m, sd = s)
        sprintf("P(X < %.4f) = %.6f   |   z = %.4f", a, p, z_of(a, m, s))
      },
      gt = {
        p <- 1 - pnorm(a, mean = m, sd = s)
        sprintf("P(X > %.4f) = %.6f   |   z = %.4f", a, p, z_of(a, m, s))
      },
      between = {
        p <- pnorm(hi, m, s) - pnorm(lo, m, s)
        sprintf("P(%.4f < X < %.4f) = %.6f   |   z-range: [%.4f, %.4f]",
                lo, hi, p, z_of(lo, m, s), z_of(hi, m, s))
      },
      outside = {
        p <- pnorm(lo, m, s) + (1 - pnorm(hi, m, s))
        sprintf("P(X < %.4f or X > %.4f) = %.6f   |   z-cuts: [%.4f, %.4f]",
                lo, hi, p, z_of(lo, m, s), z_of(hi, m, s))
      }
    )
  })
  
  ci_bounds <- reactive({
    m <- input$mean; s <- input$sd
    mode <- input$mode
    tk <- tail_kind()
    
    build_two_sided <- function(level) {
      z <- qnorm((1 + level) / 2)
      list(lower = m - z * s, upper = m + z * s, z = z, kind = "two")
    }
    build_one_sided <- function(level, side) {
      z <- qnorm(level)
      if (side == "upper") {
        list(lower = -Inf, upper = m + z * s, z = z, kind = "one-upper")
      } else {
        list(lower = m - z * s, upper = Inf, z = z, kind = "one-lower")
      }
    }
    
    side <- if (mode == "lt") "upper" else if (mode == "gt") "lower" else "two"
    
    if (tk == "two") {
      list(ci90 = build_two_sided(0.90),
           ci95 = build_two_sided(0.95))
    } else {
      list(ci90 = build_one_sided(0.90, side),
           ci95 = build_one_sided(0.95, side))
    }
  })
  
  graph <- reactive({
    if (input$sd <= 0) {
      return(ggplot() + theme_void() + ggtitle("σ must be > 0"))
    }
    m <- input$mean; s <- input$sd
    x <- seq(m - 6 * s, m + 6 * s, length.out = 2000)
    y <- dnorm(x, mean = m, sd = s)
    df <- data.frame(x, y)
    
    mode <- input$mode
    ab <- range_pair(); lo <- ab[1]; hi <- ab[2]
    a <- input$rangemin
    
    p <- ggplot(df, aes(x, y)) +
      geom_line(color = "blue", linewidth = 1) +
      labs(title = "Normal Distribution PDF", x = "x", y = "Density") +
      scale_x_continuous(sec.axis = sec_axis(~ (. - m) / s, name = "Z = (x - μ) / σ")) +
      theme_minimal()
    
    if (mode == "lt") {
      p <- p + geom_area(data = subset(df, x <= a), aes(y = y), fill = "lightblue", alpha = 0.5)
    } else if (mode == "gt") {
      p <- p + geom_area(data = subset(df, x >= a), aes(y = y), fill = "lightblue", alpha = 0.5)
    } else if (mode == "between") {
      p <- p + geom_area(data = subset(df, x >= lo & x <= hi), aes(y = y), fill = "lightblue", alpha = 0.5)
    } else {
      p <- p +
        geom_area(data = subset(df, x <= lo), aes(y = y), fill = "lightblue", alpha = 0.5) +
        geom_area(data = subset(df, x >= hi), aes(y = y), fill = "lightblue", alpha = 0.5)
    }
    
    bounds <- ci_bounds()
    
    add_two_sided <- function(ci, fill_col, line_col, lty) {
      band <- subset(df, x >= ci$lower & x <= ci$upper)
      p + geom_area(data = band, aes(y = y), fill = fill_col, alpha = 0.22) +
        geom_vline(xintercept = c(ci$lower, ci$upper), color = line_col, linetype = lty, linewidth = 0.9)
    }
    add_one_sided <- function(ci, fill_col, line_col, lty) {
      if (is.infinite(ci$lower)) {
        band <- subset(df, x <= ci$upper)
        p + geom_area(data = band, aes(y = y), fill = fill_col, alpha = 0.22) +
          geom_vline(xintercept = ci$upper, color = line_col, linetype = lty, linewidth = 0.9)
      } else {
        band <- subset(df, x >= ci$lower)
        p + geom_area(data = band, aes(y = y), fill = fill_col, alpha = 0.22) +
          geom_vline(xintercept = ci$lower, color = line_col, linetype = lty, linewidth = 0.9)
      }
    }
    
    if ("ci90" %in% input$cis) {
      ci <- bounds$ci90
      p <- if (tail_kind() == "two") add_two_sided(ci, "lightgreen", "darkgreen", "dashed")
      else add_one_sided(ci, "lightgreen", "darkgreen", "dashed")
    }
    if ("ci95" %in% input$cis) {
      ci <- bounds$ci95
      p <- if (tail_kind() == "two") add_two_sided(ci, "orange", "darkorange", "dotdash")
      else add_one_sided(ci, "orange", "darkorange", "dotdash")
    }
    
    if (mode %in% c("lt", "gt")) {
      p <- p + geom_vline(xintercept = a, color = "blue", linewidth = 0.8)
    } else {
      p <- p + geom_vline(xintercept = c(lo, hi), color = "blue", linewidth = 0.8)
    }
    
    p
  })
  
  output$probability <- renderText({ prob() })
  
  output$ci90_text <- renderText({
    if (input$sd <= 0 || !"ci90" %in% input$cis) return("")
    b <- ci_bounds()$ci90
    if (tail_kind() == "two") {
      sprintf("90%% CI (two-sided)\n x: [%.4f, %.4f]\n z: ±%.4f", b$lower, b$upper, b$z)
    } else {
      if (is.infinite(b$lower))
        sprintf("90%% CI (one-sided, upper)\n x: (-Inf, %.4f]\n z: %.4f", b$upper, b$z)
      else
        sprintf("90%% CI (one-sided, lower)\n x: [%.4f, Inf)\n z: %.4f", b$lower, b$z)
    }
  })
  
  output$ci95_text <- renderText({
    if (input$sd <= 0 || !"ci95" %in% input$cis) return("")
    b <- ci_bounds()$ci95
    if (tail_kind() == "two") {
      sprintf("95%% CI (two-sided)\n x: [%.4f, %.4f]\n z: ±%.4f", b$lower, b$upper, b$z)
    } else {
      if (is.infinite(b$lower))
        sprintf("95%% CI (one-sided, upper)\n x: (-Inf, %.4f]\n z: %.4f", b$upper, b$z)
      else
        sprintf("95%% CI (one-sided, lower)\n x: [%.4f, Inf)\n z: %.4f", b$lower, b$z)
    }
  })
  
  output$graph <- renderPlot({ graph() })
}

shinyApp(ui, server)
