# stock_functions.R
# BDA400 Assignment 2 - Technical Analysis using R (Preliminary Stage)
# Marwah Mahsal Khan
#
# Utility functions for reading the portfolio, downloading stock data with
# quantmod, calculating basic statistics and displaying the results.
# These get sourced by technical_analysis.Rmd.

library(quantmod)
library(TTR)


# ---------------------------------------------------------------------------
# 1. Reading the portfolio file
# ---------------------------------------------------------------------------

# Reads portfolio.txt and returns a clean vector of stock symbols.
# Blank lines and extra spaces are removed, and everything is made uppercase
# so "aapl " and "AAPL" count as the same symbol.
read_portfolio <- function(file = "portfolio.txt") {
  if (!file.exists(file)) {
    stop("Can't find ", file, " - check your working directory.")
  }
  symbols <- readLines(file, warn = FALSE)
  symbols <- toupper(trimws(symbols))
  symbols <- symbols[symbols != ""]
  unique(symbols)
}


# ---------------------------------------------------------------------------
# 2. Importing the stock data
# ---------------------------------------------------------------------------

# Reads portfolio.txt, downloads daily prices for every symbol from Yahoo
# Finance using quantmod, and returns a named list with one data frame per
# stock (e.g. stocks$AAPL, stocks$MSFT).
#
# quantmod gives back an xts object (a time series), so each one is converted
# into a normal data frame with the date as its own column.
#
# Note: "to" is set to 2026-01-01 so the whole of 2025 is included. Jan 1 is
# a market holiday anyway so no extra day gets pulled in.
load_stock_data <- function(file = "portfolio.txt",
                            from = "2025-01-01",
                            to   = "2026-01-01") {
  symbols <- read_portfolio(file)
  stocks  <- list()

  for (sym in symbols) {
    message("Downloading ", sym, " ...")

    stock_df <- tryCatch({
      xts_data <- getSymbols(sym, src = "yahoo", from = from, to = to,
                             auto.assign = FALSE)
      df <- data.frame(Date = index(xts_data), coredata(xts_data))
      # quantmod names columns like "AAPL.Open" - simplify them
      colnames(df) <- c("Date", "Open", "High", "Low", "Close",
                        "Volume", "Adjusted")
      df
    }, error = function(e) {
      message("  Could not download ", sym, ": ", conditionMessage(e))
      NULL
    })

    if (!is.null(stock_df)) {
      stocks[[sym]] <- stock_df
    }
  }

  message("Loaded ", length(stocks), " of ", length(symbols), " symbols.")
  stocks
}


# ---------------------------------------------------------------------------
# 3. Statistics
# ---------------------------------------------------------------------------

# Base R doesn't have a function for the statistical mode (R's mode() tells
# you the storage type of an object, not the most common value).
# Closing prices almost never repeat to the exact cent, so the prices are
# rounded first (to the nearest dollar by default) and then the most common
# rounded value is returned. If there's a tie, the lowest value wins because
# table() sorts values in increasing order and which.max() takes the first.
calc_mode <- function(x, digits = 0) {
  x <- round(x[!is.na(x)], digits)
  counts <- table(x)
  as.numeric(names(counts)[which.max(counts)])
}

# Adds 20-day and 50-day simple moving average columns to a stock's data
# frame using SMA() from TTR. The first 19 (or 49) rows are NA because there
# aren't enough earlier days yet to average.
add_moving_averages <- function(stock_df, short_n = 20, long_n = 50) {
  stock_df$SMA_20 <- SMA(stock_df$Close, n = short_n)
  stock_df$SMA_50 <- SMA(stock_df$Close, n = long_n)
  stock_df
}

# Takes one stock's data frame and returns a one-row data frame of stats
# based on the daily closing price. The moving averages reported are the most
# recent values (last trading day in the data).
calculate_statistics <- function(stock_df, symbol = "") {
  close <- stock_df$Close
  sma20 <- SMA(close, n = 20)
  sma50 <- SMA(close, n = 50)

  data.frame(
    Symbol      = symbol,
    From        = min(stock_df$Date),
    To          = max(stock_df$Date),
    Days        = length(close),
    Mean        = round(mean(close, na.rm = TRUE), 2),
    Median      = round(median(close, na.rm = TRUE), 2),
    Mode        = calc_mode(close),
    SD          = round(sd(close, na.rm = TRUE), 2),
    Min         = round(min(close, na.rm = TRUE), 2),
    Max         = round(max(close, na.rm = TRUE), 2),
    SMA_20_Last = round(tail(sma20, 1), 2),
    SMA_50_Last = round(tail(sma50, 1), 2)
  )
}

# Runs calculate_statistics() on every stock in the list and stacks the
# results into one table (one row per stock).
calculate_all_statistics <- function(stocks) {
  rows <- lapply(names(stocks), function(sym) {
    calculate_statistics(stocks[[sym]], sym)
  })
  do.call(rbind, rows)
}


# ---------------------------------------------------------------------------
# 4. Display utilities
# ---------------------------------------------------------------------------

# Shows the most recent n rows of a stock, newest first, laid out the same
# way Yahoo Finance's "Historical Data" tab does it.
display_historical <- function(stock_df, symbol = "", n = 10) {
  recent <- tail(stock_df, n)
  recent <- recent[order(recent$Date, decreasing = TRUE), ]
  out <- data.frame(
    Date        = format(recent$Date, "%b %d, %Y"),
    Open        = sprintf("%.2f", recent$Open),
    High        = sprintf("%.2f", recent$High),
    Low         = sprintf("%.2f", recent$Low),
    Close       = sprintf("%.2f", recent$Close),
    `Adj Close` = sprintf("%.2f", recent$Adjusted),
    Volume      = formatC(recent$Volume, format = "d", big.mark = ","),
    check.names = FALSE
  )
  rownames(out) <- NULL
  cat("\n", symbol, "- last", n, "trading days\n")
  out
}

# A quick "quote summary" similar to the box at the top of a Yahoo Finance
# stock page: last close, previous close, day's range, range over the whole
# period, volume and average volume.
display_quote_summary <- function(stock_df, symbol = "") {
  last <- nrow(stock_df)
  data.frame(
    Item = c("Last Close", "Previous Close", "Open", "Day's Range",
             "Period Range", "Volume", "Avg. Volume"),
    Value = c(
      sprintf("%.2f", stock_df$Close[last]),
      sprintf("%.2f", stock_df$Close[last - 1]),
      sprintf("%.2f", stock_df$Open[last]),
      sprintf("%.2f - %.2f", stock_df$Low[last], stock_df$High[last]),
      sprintf("%.2f - %.2f", min(stock_df$Low), max(stock_df$High)),
      formatC(stock_df$Volume[last], format = "d", big.mark = ","),
      formatC(round(mean(stock_df$Volume)), format = "d", big.mark = ",")
    )
  )
}

# Line chart of the closing price with the 20 and 50 day moving averages.
plot_price_with_ma <- function(stock_df, symbol = "") {
  stock_df <- add_moving_averages(stock_df)
  plot(stock_df$Date, stock_df$Close, type = "l", col = "black",
       xlab = "Date", ylab = "Price (USD)",
       main = paste(symbol, "- Closing Price with Moving Averages"))
  lines(stock_df$Date, stock_df$SMA_20, col = "blue")
  lines(stock_df$Date, stock_df$SMA_50, col = "red")
  legend("topleft", legend = c("Close", "20-day SMA", "50-day SMA"),
         col = c("black", "blue", "red"), lty = 1, bty = "n", cex = 0.8)
  grid()
}

# Candlestick chart with volume using quantmod's own chartSeries().
# chartSeries needs an xts object, so the data frame gets converted back.
plot_candlestick <- function(stock_df, symbol = "") {
  x <- xts(round(stock_df[, c("Open", "High", "Low", "Close", "Volume")], 2),,
           order.by = stock_df$Date)
  chartSeries(x, name = symbol, theme = chartTheme("white"),
              TA = "addVo();addSMA(n = 20)")
}

# Puts every stock on one chart by rebasing each closing price to 100 on
# the first day. Makes it easy to compare performance even though the
# actual prices are very different (e.g. NVDA vs MSFT).
plot_normalized <- function(stocks) {
  norm <- lapply(stocks, function(df) 100 * df$Close / df$Close[1])
  y_range <- range(unlist(norm), na.rm = TRUE)
  cols <- seq_along(stocks)

  first <- stocks[[1]]
  plot(first$Date, norm[[1]], type = "l", col = cols[1], ylim = y_range,
       xlab = "Date", ylab = "Value (start = 100)",
       main = "Portfolio - Relative Performance")
  if (length(stocks) > 1) {
    for (i in 2:length(stocks)) {
      lines(stocks[[i]]$Date, norm[[i]], col = cols[i])
    }
  }
  abline(h = 100, lty = 2, col = "grey")
  legend("topleft", legend = names(stocks), col = cols, lty = 1,
         bty = "n", cex = 0.8)
}

# Saves each stock's data frame to a CSV in the output folder so the data
# can be opened in Excel or checked separately.
save_stock_data <- function(stocks, folder = "output") {
  if (!dir.exists(folder)) dir.create(folder)
  for (sym in names(stocks)) {
    write.csv(stocks[[sym]], file.path(folder, paste0(sym, ".csv")),
              row.names = FALSE)
  }
  invisible(file.path(folder, paste0(names(stocks), ".csv")))
}
