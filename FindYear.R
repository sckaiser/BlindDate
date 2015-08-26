FindYear <- function(x) {
  # Given a date element list, finds the one with 3 or more digits (the year).
  # Args:
  #  x, a list of vectors, each vector corresponding to a date element, i.e., 
  #     a day, month, or year.
  # Returns:
  #  an integer indicating which date element is the year. If it cannot
  #  determine the year, it returns NA.
  
  ModeNchar <- function(x) TrueMode(nchar(x))  # mode of number of characters
  
  x    <- lapply(x, ModeNchar)
  year <- which (x >= 3)   # Confirm exactly one date element longer than 2
  if (length(year) == 1) {
    year  # return it
  } else {
    NA  # else return NA
  }
}

FindYear(list(c("1990", "1991"), c("20", "31"), c("02", "10")))
FindYear(list(c("1990", "1991"), c("2001", "2031"), c("02", "10")))
FindYear(list(c("20", "31"), c("1990", "1991")))
FindYear(list(c("20", "31")))