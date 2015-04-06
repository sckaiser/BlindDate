true.mode <- function(x) {
  # Calculates the most frequent value in an vector.
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}