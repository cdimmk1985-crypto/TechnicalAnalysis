# Stochastic RSI (StochRSI)
# Template and naming required by the assignment - do not rename.
# Depends on rsi() and sma() - make sure both are sourced first.

stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)

  # Calculate the StochRSI
  min_rsi <- min(rsi_values, na.rm = TRUE)
  max_rsi <- max(rsi_values, na.rm = TRUE)
  k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)

  # Calculate the %K line (StochRSI)
  k_line <- sma(k_values[!is.na(k_values)], k_period)

  # Calculate the %D line (3-day simple moving average of %K)
  d_line <- sma(k_line, d_period)

  # Return the %K and %D lines as a list
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  return(result)
}
