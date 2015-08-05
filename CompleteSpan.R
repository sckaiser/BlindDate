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
  
  # 1. convert the missing value to NA
  # 2. check exactly one value is missing
  # 3. setdiff x and set to find which value is to be completed
  # 3a. some error handling if the intersection doesn't produce exactly one 
  #     result; for example, if it is char0 or num0 or >1
  # 4. assignment
  # 5. conver the NAs back to the missing value
}