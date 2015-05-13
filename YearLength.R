YearLength <- function(date.pos) {
  # Given an integer vector of years, returns y for 2 digit years;
  # otherwise, returns Y (implying 3 or more digit years).
  year.digits <- nchar(date.pos) # count number of digits
  year.digits <- TrueMode(year.digits) # pick most common.
  if (year.digits < 2) {
    stop("The year argument had less than two digits")
  }
  if (year.digits == 2) {
    date.format <- "y" 
  } else {
    date.format <- "Y"
  }
  return(date.format)
}