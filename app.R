library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Normal PDF with Optional Confidence Intervals"),
  sidebarLayout(
    sidebarPanel(
      numericInput("mean", label = "Mean (μ):", value = 0),
      numericInput("sd", label = "Standard Deviation (σ):", value = 1, min = 0.0001, step = 0.1),
      
      # Probability mode selector
      selectInput(
        "mode",
        label = "Probability region",
        choices = c("Less than a value" = "lt",
                    "Greater than a value" = "gt",
                    "Between two values" = "between",
                    "Outside two values" = "outside"),
        selected = "between"
      ),
      
      # Always show the first input; we'll relabel it dynamically
      numericInput("rangemin", label = "Range Min:", value = -2),
      
      # Show Range Max only when needed
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
      verbatimTextOutput("ci_text")
    )
  )
)

server <- function(input, output, session) {
  
  # Relabel Range Min depending on mode
  observeEvent(input$mode, {
    if (input$mode == "lt") {
      updateNumericInput(session, "rangemin", label = "Cutoff (x0): P(X < x0)")
    } else if (input$mode == "gt") {
      updateNumericInput(session, "rangemin", label = "Cutoff (x0): P(X > x0)")
    } else {
      updateNumericInput(session, "rangemin", label = "Range Min:")
      # If rangemax exists (only visible in UI for between/outside), keep its standard label
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
  
  # Confidence intervals respect one- vs two-tailed choice
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
      scale_x_continuous(sec.axis = sec_axis(~ (. - m) / s, name = "Values")) +
      theme_minimal()
    
    # Probability region shading (original light blue)
    if (mode == "lt") {
      p <- p + geom_area(data = subset(df, x <= a), aes(y = y), fill = "lightblue", alpha = 0.5)
    } else if (mode == "gt") {
      p <- p + geom_area(data = subset(df, x >= a), aes(y = y), fill = "lightblue", alpha = 0.5)
    } else if (mode == "between") {
      p <- p + geom_area(data = subset(df, x >= lo & x <= hi), aes(y = y), fill = "lightblue", alpha = 0.5)
    } else { # outside
      p <- p +
        geom_area(data = subset(df, x <= lo), aes(y = y), fill = "lightblue", alpha = 0.5) +
        geom_area(data = subset(df, x >= hi), aes(y = y), fill = "lightblue", alpha = 0.5)
    }
    
    # Confidence intervals
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
    
    # Draw user cut(s)
    if (mode %in% c("lt", "gt")) {
      p <- p + geom_vline(xintercept = a, color = "lightblue", linewidth = 0.8)
    } else {
      p <- p + geom_vline(xintercept = c(lo, hi), color = "lightblue", linewidth = 0.8)
    }
    
    p
  })
  
  output$probability <- renderText({ prob() })
  
  output$ci_text <- renderText({
    if (input$sd <= 0) return("")
    b <- ci_bounds()
    kind <- tail_kind()
    
    fmt_two <- function(label, ci) {
      sprintf("%s CI (two-sided): x=[%.4f, %.4f], z=±%.4f", label, ci$lower, ci$upper, ci$z)
    }
    fmt_one <- function(label, ci) {
      if (is.infinite(ci$lower)) {
        sprintf("%s CI (one-sided, upper): x=(-Inf, %.4f], z=%.4f", label, ci$upper, ci$z)
      } else {
        sprintf("%s CI (one-sided, lower): x=[%.4f, Inf), z=%.4f", label, ci$lower, ci$z)
      }
    }
    
    parts <- character()
    if ("ci90" %in% input$cis) parts <- c(parts, if (kind == "two") fmt_two("90%", b$ci90) else fmt_one("90%", b$ci90))
    if ("ci95" %in% input$cis) parts <- c(parts, if (kind == "two") fmt_two("95%", b$ci95) else fmt_one("95%", b$ci95))
    if (length(parts) == 0) "" else paste(parts, collapse = "   |   ")
  })
  
  output$graph <- renderPlot({ graph() })
}

shinyApp(ui, server)
