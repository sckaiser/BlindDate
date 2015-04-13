CountChars <- function(x, y) {
  # Counts how often a single character appears in another character vector.
  # Args:
  #  x, a character vector,
  #  y, a length one character vector containing a single character.
  # Returns:
  #  n.char.y, the number of times y is found in each string in x.
  
  len.y  <- length(y)
  nchars <- nchar(y)
  if (len.y  != 1) {
    print("Error: The second argument must be length 1.")
    print(paste("It was length:", len.y))
    stop()
  }
  if (nchars != 1) {
    print("Error: The second argument must have exactly one character.")
    print(paste("The argument provded had", nchars, "characters:", y))
    stop()
  }
  
  x.char.y  <- gregexpr(y, x, fixed = T)  # get y's locations in x
  n.char.y  <- lapply(x.char.y, length)  # count y's occurances
  n.char.y  <- unlist(n.char.y)  # convert to vector
  
  # Handle when gregexpr returns -1 (i.e., not found)
  first     <- function(z) { z[1] }  # helper function
  not.found <- lapply(x.char.y, first)  # get the first match
  not.found <- unlist(not.found)   # convert to vector
  not.found <- which(not.found == -1)  # -1 indicates no match
  n.char.y[not.found] <- 0  # set non-matches to zero.
  return(n.char.y)
}