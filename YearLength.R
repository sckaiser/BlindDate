YearLength <- function(date.pos) {
  # Given an integer vector of years, returns y for 2 digit years;
  # otherwise, returns Y (implying 3 or more digit years).
  
  year.digits <- nchar(date.pos) # count number of digits
  year.digits <- TrueMode(year.digits) # pick most common.
  stopifnot (year.digits >= 2)
  if (year.digits == 2) {
    date.format <- "y" 
  } else {
    date.format <- "Y"
  }
  return(date.format)
}