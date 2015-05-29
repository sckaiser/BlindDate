WhichMax <- function(x) {
  # Modified version of which.is.max() from {nnet}
  # Like which.max(), but randomly breaks ties
  
  y <- seq_along(x)[x == max(x)]
  if(length(y) > 1) {
    sample(y, 1)
  } else {
    y
  }
}