GreplAny <- function(pattern, x, ...) {
  # Runs grepl() across multiple patterns.
  # Args:
  #   pattern, a character vector of patterns to search.
  #   x, a character vector to be searched.
  # Returns:
  #  a logical indicating whether x contains any of the entries in pattern.
  
  y  <- vapply(pattern, grepl, logical(length(x)), x, ...)  # check for matches
  if(is.matrix(y)) {
    apply(y, 1, any)  # return results if length(x) > 1
  } else {
    any(y)  # return results if length(x) == 1
  }
}