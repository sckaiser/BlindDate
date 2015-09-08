is.empty <- function(x, ignore.nm = T) {
  # checks if a vector is in the family of "atomic(0)".
  # Args:
  #  x, a vector.
  #  ignore.nm, a logical indicating whether to ignore attributes.
  if (ignore.nm) {
    x <- as.vector(x)  # remove any attributes
  }
  identical(x, x[0]) 
}