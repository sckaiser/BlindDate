CountChars <- function(x, y) {
  # Counts the number of times a given character is found
  # Args:
  #  x, a character vector,
  #  y, a character of length one.
  len.y <- length(y)
  if (len.y != 1) {
    
    print(paste("The second argument was not of length 1. It was of length:", len.y))
    stop()
  } # need to add error checking for nchar(y) != 1
  which.char.y <- gregexpr(y, x, fixed = T)  # get locations of the character
  n.char.y <- lapply(which.char.y, length)  # count number of occurances
  unlist(n.char.y)  # convert to vector
}
