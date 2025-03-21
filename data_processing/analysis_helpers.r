TAE <- function(actual, expected) {
  sum(abs(actual - expected))
}
RE <- function(actual, expected, N) {
  TAE(actual, expected) / (N * length(actual))
}
RMSE <- function(actual, expected) {
  sqrt(sum((actual - expected) * (actual - expected)) / length(actual))
}
