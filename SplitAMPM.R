SplitAMPM <- function(x, PM.vector = T) {
  # Removes AM/PM from a 'character date' vector.
  # Args:
  #  x, a character vector containing dates.
  #  PM.vector, a logical indicating whether to return a map vector of
  #             AM/PM elements.
  # Returns:
  #  x, the vector with strings AM and PM removed.
  #  PM.times, a logical the length of x indicating which elements of x were PM;
  #            this implies the FALSE were AM.
  
  PM.times <- grepl("PM", x)  # store which times are PM
  x        <- gsub("PM", "", x)  # Remove PM
  x        <- gsub("AM", "", x)  # Remove AM
  if (PM.vector) {
    list(x, PM.times)
  } else {
    x
  }
}