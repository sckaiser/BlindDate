UniqueOrder <- function(x) {
  # 
  # Args:
  #  x, a vector (or a list of length-1 vectors)
  # Returns:
  #  x, a vector of unique, ordered values.
  x <- unlist(x)     # convert from list if necessary
  x <- as.vector(x)  # drop names
  x <- unique(x)     # remove duplicates
  x[order(x)]        # sort ascending & return
}