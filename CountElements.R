CountElements <- function(x) {
  # Counts the number of elements in a character string. An element is defined
  # as a sequence of adjacent numbers or characters, exluduing punctuation and
  # spaces.
  # Args:
  #  x, a character vector
  # Returns:
  #  y, an integer: the most frequent number of elements found.
  
  x <- TokenizeDt(x)  # tokenize to a list of character vectors (elements)
  y <- mclapply(x, length)  # count elements in each x
  y <- unlist(y)
  TrueMode(y)  # return the most common.
}