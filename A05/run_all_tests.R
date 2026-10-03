# run_all_tests.R
# Loads every indicator function and runs the hypothetical examples from the
# assignment PDF, plus real 2025 stock data from the A02 output/ folder, so
# an evaluator can confirm every function works end to end.
#
# Run this from inside the A05 folder (so the source() paths below resolve).

source("sma.R")
source("ema.R")
source("macd.R")
source("stdev.R")
source("linreg.R")
source("rsi.R")
source("stoch_rsi.R")
source("crossover.R")
source("crossunder.R")

cat("---- SMA ----\n")
print(sma(c(10, 12, 15, 20, 18, 22, 25, 24, 21), period = 3))

cat("\n---- EMA ----\n")
print(ema(c(10, 12, 15, 20, 18, 22, 25, 24, 21), period = 3))

cat("\n---- MACD ----\n")
print(macd(c(100, 105, 110, 115, 120, 125, 130), short_period = 3, long_period = 5, signal_period = 2))

cat("\n---- stdev ----\n")
print(stdev(c(10, 12, 15, 20, 18, 22, 25, 24, 21)))

cat("\n---- linreg ----\n")
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
print(linreg(data, regressionLength = length(data), regressionOffset = 0))

cat("\n---- RSI ----\n")
print(rsi(c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62), period = 5))

cat("\n---- StochRSI ----\n")
print(stoch_rsi(c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62), period = 5, k_period = 3, d_period = 3))

cat("\n---- Crossover / Crossunder ----\n")
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
print(crossover(arr1, arr2))
print(crossunder(arr1, arr2))

cat("\n---- Real stock data (2025, from A02) ----\n")
symbols <- c("AAPL", "MSFT", "NVDA", "AMZN", "TSLA")
for (sym in symbols) {
  df <- read.csv(paste0("../output/", sym, ".csv"))
  close <- df$Close
  cat(sym, "- Last SMA(20):", round(tail(sma(close, 20), 1), 2),
      "| Last RSI(14):", round(tail(rsi(close, 14), 1), 2),
      "| stdev:", round(stdev(close), 2), "\n")
}
