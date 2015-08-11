CompleteSpan <- function(x, set, missing.val = NA) {
  # If exactly one value in a vector is missing, returns it. Otherwise, returns
  # the original vector unchanged. The point is to fill in the blanks when there
  # is exactly one blank, and a known set of values. For example, when guessing
  # the order of a date (YMD or MDY), if the location of M & D are known, Y can
  # be infered to be in the unknown location.
  # Args:
  #  x, a vector of values with potentially one to be filled; order matters.
  #  set, the vector of values if x were complete. Order does not matter.
  #  missing.val, which tells the function how the missing value in x is 
  #               represented as missing.
  
  x.orig <- x
  if(!is.na(missing.val)) {
    x[x == missing.val] <- NA  # 1. convert the missing value to NA
  }
  # 2. check no more than one value is missing. 
  if (length(x[is.na(x)]) != 1) {
    return(x.orig)  # return the original
  } else {
    fill <- setdiff(set, x)  # 3. find which value is to be completed
    if (length(fill) != 1) {
      
    }
    # 3a. error handling if the setdiff isn't length-one 
    
    x[is.na(x)] <- fill  # 4. assignment
    return(x)
  } 
}

# testing
CompleteSpan(c("D", "Y", NA), c("D", "M", "Y"))
CompleteSpan(c(1:4, NA, 6:10), 1:10)
CompleteSpan(c(7:10, NA, 1:5), 1:10)
CompleteSpan(c("D", "Y", NA), c("D", "M", "Y"))

