# ============================================================
# app.R - Portfolio Visualization Dashboard (BDA400 Assignment 6)
# ============================================================
# Fetches live stock data with quantmod, visualizes it with ggplot2 in a
# Shiny dashboard, overlays technical indicators, and applies a moving
# average crossover trading rule with buy/sell annotations.
#
# Design note on indicator overlays: Moving Averages are drawn directly on
# the price chart since they share the same price scale. RSI (0-100) and
# MACD (can be positive or negative, a different unit entirely) are NOT
# overlaid directly on the price chart - that would squash the price line
# unreadable. Instead they're drawn as their own stacked sub-panels below
# the price chart, which is standard practice in real trading platforms,
# and each panel only appears when its checkbox is ticked (satisfying the
# "turn the overlay on and off for each indicator" requirement).

library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)

source("core_logic.R")

portfolio_symbols <- c("AAPL", "MSFT", "NVDA", "AMZN", "TSLA")

# ---------------------------------------------------------------- UI ----
ui <- fluidPage(
  titlePanel("Portfolio Visualization Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("symbol", "Stock Symbol:", choices = portfolio_symbols),
      dateRangeInput("date_range", "Select Date Range:",
                     start = "2025-01-01", end = "2025-12-31"),
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly")),
      selectInput("chart_type", "Chart Type:",
                  choices = c("Line", "Candlestick", "Area")),
      checkboxGroupInput("indicators", "Technical Indicators:",
                         choices = c("Moving Averages", "RSI", "MACD")),
      conditionalPanel(
        condition = "input.indicators.includes('Moving Averages')",
        sliderInput("short_n", "Short MA period:", min = 5, max = 50, value = 20),
        sliderInput("long_n", "Long MA period:", min = 20, max = 200, value = 50)
      ),
      checkboxInput("show_signals", "Show Buy/Sell Trading Signals", value = TRUE)
    ),
    mainPanel(
      plotOutput("stock_chart", height = "400px"),
      conditionalPanel(
        condition = "input.indicators.includes('RSI')",
        plotOutput("rsi_chart", height = "180px")
      ),
      conditionalPanel(
        condition = "input.indicators.includes('MACD')",
        plotOutput("macd_chart", height = "180px")
      ),
      h4("Trading Signal Log"),
      tableOutput("signal_table")
    )
  )
)

# ------------------------------------------------------------ SERVER ----
server <- function(input, output, session) {

  # Fetch live data from Yahoo Finance via quantmod. auto.assign = FALSE
  # returns the xts object directly instead of creating a variable, so it
  # can be converted straight into a plain data frame.
  raw_data <- reactive({
    xts_data <- getSymbols(input$symbol, src = "yahoo",
                            from = "2025-01-01", to = "2025-12-31",
                            auto.assign = FALSE)
    df <- data.frame(Date = zoo::index(xts_data), zoo::coredata(xts_data))
    colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    df
  })

  # Aggregates to the selected time frame, then filters to the selected
  # date range.
  filtered_data <- reactive({
    df <- aggregate_timeframe(raw_data(), input$time_frame)
    df[df$Date >= input$date_range[1] & df$Date <= input$date_range[2], ]
  })

  # Adds Short_MA/Long_MA/Signal columns using the moving average
  # crossover trading rule.
  signal_data <- reactive({
    generate_signals(filtered_data(), input$short_n, input$long_n)
  })

  # Builds the main price chart: line, candlestick, or area, with the MA
  # overlay and buy/sell annotations added conditionally.
  output$stock_chart <- renderPlot({
    df <- if ("Moving Averages" %in% input$indicators) signal_data() else filtered_data()

    p <- switch(input$chart_type,
      "Line" = ggplot(df, aes(x = Date, y = Close)) +
                 geom_line(color = "steelblue", linewidth = 0.8),
      "Area" = ggplot(df, aes(x = Date, y = Close)) +
                 geom_area(fill = "steelblue", alpha = 0.3) +
                 geom_line(color = "steelblue", linewidth = 0.8),
      "Candlestick" = ggplot(df, aes(x = Date)) +
                 geom_segment(aes(y = Low, yend = High, xend = Date), color = "grey40") +
                 geom_rect(aes(xmin = Date - 0.3, xmax = Date + 0.3,
                               ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                               fill = Close >= Open)) +
                 scale_fill_manual(values = c(`TRUE` = "forestgreen", `FALSE` = "firebrick"),
                                   guide = "none")
    )

    if ("Moving Averages" %in% input$indicators) {
      p <- p +
        geom_line(aes(y = Short_MA), color = "orange", linewidth = 0.7, na.rm = TRUE) +
        geom_line(aes(y = Long_MA), color = "purple", linewidth = 0.7, na.rm = TRUE)
    }

    if (input$show_signals && "Moving Averages" %in% input$indicators) {
      changes <- get_signal_changes(signal_data())
      if (nrow(changes) > 0) {
        p <- p + geom_point(data = changes, aes(x = Date, y = Close,
                   shape = Signal, color = Signal), size = 3, inherit.aes = FALSE) +
          scale_shape_manual(values = c(Buy = 24, Sell = 25)) +
          scale_color_manual(values = c(Buy = "forestgreen", Sell = "firebrick"))
      }
    }

    p + labs(title = paste(input$symbol, "-", input$chart_type, "Chart"),
              x = "Date", y = "Price (USD)") +
      theme_minimal()
  })

  # RSI sub-panel - only rendered/shown when the RSI checkbox is ticked.
  output$rsi_chart <- renderPlot({
    df <- add_indicators(filtered_data())
    ggplot(df, aes(x = Date, y = RSI)) +
      geom_line(color = "darkblue", na.rm = TRUE) +
      geom_hline(yintercept = c(30, 70), linetype = "dashed", color = "grey50") +
      labs(title = "RSI (14)", y = "RSI", x = NULL) +
      ylim(0, 100) +
      theme_minimal()
  })

  # MACD sub-panel - only rendered/shown when the MACD checkbox is ticked.
  output$macd_chart <- renderPlot({
    df <- add_indicators(filtered_data())
    ggplot(df, aes(x = Date)) +
      geom_line(aes(y = MACD), color = "steelblue", na.rm = TRUE) +
      geom_line(aes(y = MACD_Signal), color = "darkorange", na.rm = TRUE) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      labs(title = "MACD (12, 26, 9)", y = "MACD", x = NULL) +
      theme_minimal()
  })

  # Table of the actual buy/sell crossover points found, for the signal log.
  output$signal_table <- renderTable({
    if (!("Moving Averages" %in% input$indicators)) {
      return(data.frame(Message = "Enable 'Moving Averages' to see trading signals."))
    }
    changes <- get_signal_changes(signal_data())
    if (nrow(changes) == 0) {
      return(data.frame(Message = "No Buy/Sell crossovers in this date range."))
    }
    data.frame(Date = as.character(changes$Date),
               Close = round(changes$Close, 2),
               Signal = changes$Signal)
  })
}

shinyApp(ui = ui, server = server)
