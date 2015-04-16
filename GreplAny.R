GreplAny <- function(pattern, x, ...) {
  #  Runs grepl functions across multiple patterns
  #  pattern, a character vector of patterns to search.
  #  x, a character vector to be searched.
  # Returns:
  #  y, a logical vector indicating whether any of the entries in pattern were found in x.
  y <- sapply(pattern, grepl, x, ...)  # check for matches
  apply(y, 1, any)  # consolidate results
}