# core_logic.R
# The data-prep, aggregation, and trading-signal logic for the portfolio
# dashboard, pulled out of the Shiny app so it can be unit-tested against
# real data on its own (Shiny's reactive wrappers just call these).

library(TTR)
library(quantmod)

# Converts a daily OHLCV data frame (with a Date column) to weekly or
# monthly bars using quantmod's to.weekly()/to.monthly(). "Daily" returns
# the data unchanged.
aggregate_timeframe <- function(df, time_frame = "Daily") {
  if (time_frame == "Daily") {
    return(df)
  }
  xts_data <- xts::xts(df[, c("Open", "High", "Low", "Close", "Volume")],
                        order.by = as.Date(df$Date))
  agg <- if (time_frame == "Weekly") {
    xts::to.weekly(xts_data, drop.time = TRUE)
  } else if (time_frame == "Monthly") {
    xts::to.monthly(xts_data, drop.time = TRUE)
  } else {
    stop("time_frame must be 'Daily', 'Weekly', or 'Monthly'")
  }
  out <- data.frame(Date = zoo::index(agg), zoo::coredata(agg))
  colnames(out) <- c("Date", "Open", "High", "Low", "Close", "Volume")
  out
}

# Moving Average Crossover trading rule.
# Buy  when the short-term MA crosses above the long-term MA.
# Sell when the short-term MA crosses below the long-term MA.
# Hold otherwise.
generate_signals <- function(df, short_n = 20, long_n = 50) {
  short_ma <- SMA(df$Close, n = short_n)
  long_ma  <- SMA(df$Close, n = long_n)
  signals <- ifelse(short_ma > long_ma, "Buy",
              ifelse(short_ma < long_ma, "Sell", "Hold"))
  df$Short_MA <- short_ma
  df$Long_MA  <- long_ma
  df$Signal   <- signals
  df
}

# Picks out just the rows where the signal actually CHANGED from the
# previous row - these are the real buy/sell crossover points worth
# annotating on the chart (annotating every single day would be clutter).
get_signal_changes <- function(df) {
  df <- df[!is.na(df$Signal), ]
  changed <- c(TRUE, df$Signal[-1] != df$Signal[-nrow(df)])
  df[changed & df$Signal != "Hold", ]
}

# Adds RSI and MACD columns using TTR, for the indicator-overlay panels.
add_indicators <- function(df, rsi_n = 14, macd_fast = 12, macd_slow = 26, macd_signal = 9) {
  df$RSI <- RSI(df$Close, n = rsi_n)
  macd_vals <- MACD(df$Close, nFast = macd_fast, nSlow = macd_slow, nSig = macd_signal)
  df$MACD <- macd_vals[, "macd"]
  df$MACD_Signal <- macd_vals[, "signal"]
  df
}
