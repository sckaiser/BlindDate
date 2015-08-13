CompleteSpan <- function(x, set, missing.val = NA) {
  # If exactly one value in a vector is missing compared to a known set, returns
  # it. Otherwise, returns the original vector unchanged. The point is to fill
  # in the blanks when there is exactly one blank, and a known set of values.
  # For example, when guessing the order of a date (YMD or MDY), if the location
  # of M & D are known, the remaining location can be infered to be Y.
  # Args:
  #  x, a vector of values with potentially one to be filled; order matters.
  #  set, the vector of values if x were complete. Order does not matter.
  #  missing.val, which tells the function how the missing value in x is 
  #               represented as missing.
  # Solvable examples
  #  CompleteSpan(c(1, 2, NA), c(1, 2, 3))
  #  CompleteSpan(c(1, 2, NA, 1), c(1, 2, 3))
  # Unsolvable examples
  #  CompleteSpan(c(1, NA, 2), c(1, 2))
  #  CompleteSpan(c(1, 2, NA), c(1, 2, 3, 4))
  #  CompleteSpan(c(1, NA, NA, 3), c(1, 2, 3))
  # Null examples
  #  CompleteSpan(c(3, 2, 1), c(1, 2))
  #  CompleteSpan(c(3, 2, 1), c(1, 2, 3))
  #  CompleteSpan(c(3, 2, 1), c(1, 2, 3, 4))
  
  if (length(set) > length(x)) {
    return(x)  # if set is bigger than x, the problem is underdetermined
  }
  x.orig <- x
  if (!is.na(missing.val)) {
    x[x == missing.val] <- NA  # convert the missing value to NA
  }
  if (length(x[is.na(x)]) != 1) {  # check exactly one value is missing
    return(x.orig)  # if 0 or >1, return the original
  } else {
    fill <- setdiff(set, x)  # find the value that completes x
    if (is.empty(fill)) {
      return(x.orig)  # handle case when x spans set *and* has a missing value
    }  
    x[is.na(x)] <- fill  # assign it
    return(x)
  } 
}