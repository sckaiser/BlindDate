UnSample <- function(x, sample.sz, end.sz = max(10, 0.005 * sample.sz)) {
  # Randomly samples most of a vector, but includes beginning and ending values.
  # This is useful when loading data where unusual values are found at the ends.
  # For example, for data logged sequentially, early and later values may differ
  # due to changes in measurement granularity or format at some point during the
  # life of the logging process.
  # Args:
  #  x, a vector.
  #  sample.sz, the number of samples to return, including those at the ends.
  #  end.sz, the number of samples from the beginning and end (combined).
  
  end.sz    <- round(end.sz)
  end.sz    <- end.sz + end.sz %% 2
  stopifnot(length(x) >= sample.sz, sample.sz > end.sz)
  if (sample.sz >= 15 && end.sz > 0) {
    end.sz  <- end.sz / 2
    ends    <- c(x[1:end.sz], x[(length(x) - end.sz):length(x)])
    middle  <- x[end.sz:(length(x) - end.sz)]
    rand.sz <- min(sample.sz - end.sz, length(middle))
    c(ends, middle[sample(length(middle), rand.sz)])
  } else {
    warning("The vector was simply sampled at random.")
    x[sample(length(x), sample.sz)]
  }
}