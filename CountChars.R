CountChars <- function(x, y) {
  # Counts the number of times a given character is found
  # Args:
  #  x, a character vector,
  #  y, a character of length one.
  n.spaces <- gregexpr(y, x, fixed = T)  # get locations of the character
  n.spaces <- lapply(n.spaces, length)  # count number of occurances
  unlist(n.spaces)  # convert to vector
}
