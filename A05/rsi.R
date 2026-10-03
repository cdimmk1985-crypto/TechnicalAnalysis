# Relative Strength Index (RSI)
# Template and naming required by the assignment - do not rename.

rsi <- function(data, period) {
  # Calculate the differences between consecutive data points
  diff_values <- diff(data)

  # Initialize two vectors to store the gains and losses
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  # Calculate gains and losses
  for (i in 1:length(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }

  # Calculate the average gains and average losses for the first 'period' data points
  avg_gain <- mean(gains[1:period])
  avg_loss <- mean(losses[1:period])

  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA, length(data))

  # Calculate RSI values using the Wilder's smoothing method
  for (i in (period + 1):length(data)) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period

    rs <- avg_gain / avg_loss
    rsi_values[i] <- 100 - (100 / (1 + rs))
  }

  return(rsi_values)
}
