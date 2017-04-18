UnSample <- function(x, sample.sz, end.sz = -1L) {
  # Randomly samples most of a vector, but includes beginning and ending values.
  # This is useful when loading data where unusual values are found at the ends.
  # For example, for data logged sequentially, early and later values may differ
  # due to changes in measurement granularity or format at some point during the
  # life of the logging process.
  # Args:
  #  x, a vector.
  #  sample.sz, a length-one integer; the number of samples to return, including
  #             those at the ends.
  #  end.sz, a length-one integer; the number of samples from the beginning and
  #          from the end (combined).
  
  stopifnot(length(sample.sz) == 1, length(end.sz) == 1, length(x) >= sample.sz,
            sample.sz > end.sz)
  if (end.sz == -1L) {  # set the defaut
    if (sample.sz <= 3L) {
      end.sz <- 0L
    } else if (sample.sz <= 10L) {
      end.sz <- min(2L, sample.sz - 1L)
    } else {
      end.sz <- max(10, 0.005 * sample.sz)
      end.sz <- as.integer(round(end.sz))
    }
    end.sz <- end.sz + end.sz %% 2L
  }
    end.sz  <- end.sz / 2
    ends    <- c(x[1:end.sz], x[(length(x) - end.sz):length(x)])
    middle  <- x[end.sz:(length(x) - end.sz)]
    rand.sz <- min(sample.sz - end.sz, length(middle))
    c(ends, middle[sample(length(middle), rand.sz)])
}