is.useful <- function(FUN, x, thr = 1, sample.sz = min(5000, length(x)), ...) {
  # Runs a function against a sample of a vector to estimate if the function
  # will change the vector.
  # Args:
  #  FUN, a function which takes x as its first argument. FUN comes first to
  #       facilitate calling via lapply.
  #  x, a vector that is accepted by the function.
  #  thr, a threshold value between 0 and 1; 1 being the strictest.
  #  sample.sz, the number of values of x to sample.
  #  ..., additional arguments to FUN.
  # Returns:
  #  a logical indicating whether or not FUN changes x.
  
  stopifnot(thr >= 0 && thr <= 1 && sample.sz <= length(x))
  x     <- UnSample(x, sample.sz)
  x     <- x[!is.na(x)]
  match <- length(which(x == FUN(x, ...))) / length(x)
  (match < thr)
}