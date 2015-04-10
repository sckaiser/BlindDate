TrueMode <- function(x) {
  # Calculates the most frequent value in an vector.
  # Ref: https://stackoverflow.com/questions/2547402/standard-library-function-in-r-for-finding-the-mode
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}