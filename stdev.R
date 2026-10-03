# Standard Deviation (stdev)
# Template and naming required by the assignment - do not rename.
# Note: this uses the population formula (divide by n) as given in the
# assignment's own formula (sum of squared diffs / n), not R's built-in
# sd() which divides by (n-1) (sample standard deviation).

stdev <- function(data) {
  # Calculate the mean of the data
  mean_value <- sum(data) / length(data)

  # Calculate the differences between the data points and the mean
  diff_values <- data - mean_value

  # Calculate the squared differences
  squared_diff <- diff_values^2

  # Calculate the variance (mean of squared differences)
  variance <- sum(squared_diff) / length(data)

  # Calculate the standard deviation (square root of the variance)
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}
