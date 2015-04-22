UniqueOrder <- function(x) {
  # 
  # Args:
  #  x, a numeric vector (or a list of single-value numerics)
  # Returns:
  #  x, a numeric vector of unique, ordered values.
  x <- unlist(x)     # convert from list if necessary
  x <- as.vector(x)  # drop names
  x <- unique(x)     # remove duplicates
  x[order(x)]        # sort ascending & return
}