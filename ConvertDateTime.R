ConvertDateTime <- function(x) {
  # Continuing adventures in dates & times
  # Args:
  #  x, a character vector
  # Returns:
  #  x.date, a POSIX time vector
  x <- rm.dup.space(x) # first, trim any leading or trailing spaces and duplicate spaces.
  sample.size <- 2000
  sample.size <- min(sample.size, length(x))
  x.sample <- x[sample(length(x), sample.size)]  # for speed, sample rather than computing all.
  # Handle Month Text
  text.month <- ConvertTextMonth(x.sample, F)  # Get proportion of entries which match a text month.
  if (text.month > .95) {  # if more than 95% have text months,
    x <- ConvertTextMonth(x)  # then convert to numeric months.
  }
  # Count mode number of spaces; if >1, replace (up to) the first two with -
  # This makes the assumption that if there's one space, it separates date from time.
  # If there's >1 space, up to the first two separate date from time.
  # Clearly, more real-life examples are needed since this seems shaky.
  n.spaces <- count.chars(x.sample, " ")  # check the sample.
  mode.spaces <- true.mode(n.spaces)
  if (mode.spaces > 1) {
    n.spaces <- count.chars(x, " ")  # find which entries have multiple spaces.
    x[n.spaces > 1] <- sub(" ", "-", x[n.spaces > 1])  # replace first occurrences, if any.
    x[n.spaces > 2] <- sub(" ", "-", x[n.spaces > 2])  # replace second occurrences, if any.
  }
  # Handle AM and PM text.
  PM.times <- grepl("PM", x)  # store vector of which times are PM
  x <- gsub("PM", "", x)  # Remove PM.
  x <- gsub("AM", "", x)  # Remove AM..
  
  # Guess the format.
  #date.format <- whichFormat(x)  # try {TimeDate}'s format guesser.
  # Commented out because it incorrectly guesses "7-4-2012 12:33" as "%Y%m%d%H%M%S".
  date.format <- guessFormat(x)  # Use new guess the date format.
  x.date <- as.POSIXlt(strptime(x, format = date.format))  # convert.
  if (any(PM.times)) {  # if we had any PM times, fix them.
    x.hour <- hour(x.date)  # get hours
    x.date[PM.times & x.hour != 12] <- x.date[PM.times & x.hour != 12] + (60 * 60 * 12)  # Add 12 hours to all PM times on or after 1PM.
    x.date[!PM.times & x.hour == 12 & !is.na(x.date)] <- x.date[!PM.times & x.hour == 12 & !is.na(x.date)] - (60 * 60 * 12)  # Subtract 12 hours from all PM times before 1AM.
  }
  return(x.date)
}