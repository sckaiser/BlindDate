GreplAny <- function(pattern, x, ...) {
  # Runs grepl() across multiple patterns.
  # Args:
  #   pattern, a character vector of patterns to search.
  #   x, a character vector to be searched.
  # Returns:
  #  a logical indicating whether x contains any of the entries in pattern.
  y <- sapply(pattern, grepl, x, ...)  # check for matches
  if(is.matrix(y)) {
    y <- apply(y, 1, any)  # consolidate results if length(x) > 1
  } else {
    y <- any(y)  # consolidate results if length(x) == 1
  }
  return(y)
}