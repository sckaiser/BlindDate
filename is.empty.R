is.empty <- function(x, ignore.nm = T) {
  # checks if a vector is in the family of "atomic(0)".
  # Args:
  #  x, a vector.
  #  ignore.nm, a logical indicating whether to ignore attributes.
  if (ignore.nm) {
    x <- as.vector(x)  # remove any attributes
  }
  identical(x, logical(0)) | identical(x, integer(0))   |
  identical(x, numeric(0)) | identical(x, character(0)) |
  identical(x, complex(0)) | identical(x, raw(0))        
}