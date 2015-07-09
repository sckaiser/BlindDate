SplitVectors <- function(x, n) {
  # Splits a vector, or a list of vectors, into separate vectors. This can be
  # used in processing dates to convert, e.g. a d1-m1-y1, d2-m2-y2, ... 
  # structure into a d1, d2, ... a m1, m2, ... and a y1, y2, ... structure.
  # Args:
  #  x, a vector or a list of vectors.
  #  n, the intger number of new vectors into which x should be split.
  # Returns:
  #  a length-n list of vectors.
  
  IdxHelper <- function(idx, x) x[idx]  # subsets a vector x using idx
  
  if (length(x) %% n != 0) {
    warning("input length was not a multiple of the number of splits")
  }
  
  if (n > 1) {
    seq1        <- seq(1, length(x) + 1 - n, by = n)  # first splitting sequence 
    seq.list    <- list(seq1)
    for (i in 2:n) {
      seqx      <- seq.list[[i - 1]] + 1  # create next sequence
      seq.list  <- c(seq.list, list(seqx))  # append it
      vect.list <- lapply(seq.list, IdxHelper, x)
    }
  } else {
    vect.list   <- list(x)  # handle 'don't split' case
  }
  return(vect.list)
}