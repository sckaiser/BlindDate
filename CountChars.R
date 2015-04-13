CountChars <- function(x, y) {
  # Counts the number of times a given character is found
  # Args:
  #  x, a character vector,
  #  y, a length one character vector containing a single character.
  len.y <- length(y)
  if (len.y != 1) {
    print(paste("Errorr: The second argument must be length 1. It was length:", len.y))
    stop()
  }
  if (nchar(y[1]) != 1) {
    print("Error: The second argument must have exactly one character.")
    stop()
  }  
  which.char.y <- gregexpr(y, x, fixed = T)  # get locations of the character
  n.char.y <- lapply(which.char.y, length)  # count number of occurances
  unlist(n.char.y)  # convert to vector
}

# 2. Doesn't handle when gregexpr returns -1 (i.e., not found)