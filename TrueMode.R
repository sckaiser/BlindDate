TrueMode <- function(x, na.rm = F) {
  # Calculates the most frequent value in an vector.
  # Ref: https://stackoverflow.com/questions/2547402/standard-library-function-in-r-for-finding-the-mode
  # Args:
  #  x, a vector
  #  na.rm, whether to remove NA values.
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  ux[WhichMax(tabulate(match(x, ux)))]
}