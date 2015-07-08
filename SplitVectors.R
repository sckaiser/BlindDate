SplitVectors <- function(x, n) {
  # Splits a vector, or a list of vectors, into separate vectors. This can be
  # used in processing dates to convert, e.g. a d1-m1-y1, d2-m2-y2, ... 
  # structure into a d1, d2, ... a m1, m2, ... and a y1, y2, ... structure.
  # Args:
  #  x, a vector or a list of vectors.
  #  n, the intger number of new vectors into which x should be split.
  # Returns:
  #  a list of n vectors.
  
  if (n > 1) {
    seq1        <- seq(1, length(x) + 1 - n, by = n)  # first splitting sequence 
    vect.list   <- list(seq1)
    for (i in 2:n) {
      seqx      <- vect.list[[i - 1]] + 1  # create subsequent sequences
      vect.list <- list(vect.list, seqx)
      # TODO: actually split the vector
    }
  } else {
    vect.list   <- list(x)  # handle 'don't split' case
  }
  return(vect.list)
}

x <- c(12, 1, 2015, 11, 29, 2015, 10, 31, 2015)
n <- 3

x <- c(12, 2015, 11, 2015, 10, 2015)
n <- 2

x <- c(2015, 2014, 2016)
n <- 1
