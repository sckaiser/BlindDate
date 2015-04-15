SetPartition <- function(x, y) {
  # Completes the partition of one vector against a given set (if necessary)
  # In other words, if exaclty one value is missing, it will be supplied.
  # Args:
  #  x, a partially or fully partitioned set. NA indicates the missing value(s).
  #  y, the complete set.
  # Returns:
  #  x.complete, i.e., x with the missing value filled in.
  #  if x was already complete, x will be returned.
  # TODO: Decide what to return if x has >1 missing value.
  
  # Error Handling
  if (length(x) != length(y)) {
    print("The vectors are not the same length.")
    stop
  }
  if (class(x) != class(y)) {
    print("The vectors are not the same class.")
    stop
  }
  
  # Initialize
  x.complete <- x 
  
  # If the set is not fully partitioned, find & fill in the NA
  # If it is fully partitioned, just return x.complete
  if (!identical(x[order(x)], y[order(y)])) {
    missing.idx <- which(is.na(x))
    missing.value <- setdiff(y, x)
    x.complete[missing.idx] <- missing.value
  }
  return(x.complete)
}