GuessFormat <- function(x, mnth.pos = NA, sample.size = length(x)) {
  # Guesses the orders argument of a character string representing dates.
  # Args:
  #  x, a character vector of dates.
  #  mnth.pos, an integer indicating which date-time element had a text month.
  #  sample.size, an optional integer to specify random sampling of x.
  # Returns:
  #  guess.orders, the sequence of year, month, day, hour, minute, and second 
  #     or some subset; interpretable by {lubridate}'s parse_date_time().
  
  # ignore NAs & blanks which provide no clues and complicate downstream steps:
  x            <- x[!is.na(x)]
  if (length(x) > sample.size) {
    x          <- x[sample(length(x), sample.size)]  # sample for speed
  }
  n.elements   <- CountElements(x)
  date.len     <- 3  # assume up to the first 3 elements are dates
  n.date.pos   <- min(n.elements, date.len)  # assume up to first 3 are dates
  has.times    <- ifelse(n.elements > date.len, T, F)  # more elements = times
  x            <- TokenizeDt(x)  # create a list of vectors of date elements
  x            <- unlist(x)
  dates        <- SplitVectors(x, n.elements)
  dates        <- lapply(dates, as.integer)  # TokenizeDt() ensures integers
  dates        <- lapply(dates, UniqueOrder)
  date.pos1    <- dates[[1]]
  date.pos2    <- ListExtract(dates, 2)
  date.pos3    <- ListExtract(dates, 3)
  dt.format    <- rep(-1, n.date.pos) # initialize with a failure value
  year.pos     <- FindYear(dates[1:n.date.pos])  # check for 4 digit dates
  if (year.pos >= 1 & year.pos <= n.date.pos) {
    dt.format[year.pos] <- "Y"
  }
  if (n.date.pos == 1) {
    # Assume a year.
    dt.format <- YearLength(dates)
  } else if (n.date.pos == 2) {
    # Assume a month and a year.
    # Check if either has values under 12:
    if (max(date.pos1) <= 12 & max(date.pos2) > 12) {
      dt.format[1] <- "m"
    } else if (max(date.pos1) > 12 & max(date.pos2) <= 12) {
      dt.format[2] <- "m"
    }
    # If we found only one position, impute the other:
    if (is.na(year.pos) | year.pos > 3) {
      span.vals <- c("y", "m")
    } else {
      span.vals <- c("Y", "m")
    }
    dt.format <- CompleteSpan(dt.format, span.vals, missing.val = "-1")
  } else if (n.date.pos == 3) {
    # We could be more conservative & assume that if we have fairly big data 
    # then we'll see all months & days; but for now, just look for ranges.
    # If exactly one position is between 1 and 12, assign that as the month.
    max.date.p1 <- max(date.pos1)
    max.date.p2 <- max(date.pos2)
    max.date.p3 <- max(date.pos3)
    if (max.date.p1 <= 12 & max.date.p2 > 12 & max.date.p3 > 12) {
      dt.format[1] <- "m"
    } else if (max.date.p1 > 12 & max.date.p2 <= 12 & max.date.p3 > 12) {
      dt.format[2] <- "m"
    } else if (max.date.p1 > 12 & max.date.p2 > 12 & max.date.p3 <= 12) {
      dt.format[3] <- "m"
    }
    # If exactly one position is between 1 and 31, assign that as the day.
    # Note that this will be unable to classify cases where all date positions
    # are between 1 & 12, i.e., 1/11/10, 1/12/12, etc.)
    if ((max.date.p1 > 12 & max.date.p1 <= 31) & !(max.date.p2 > 12 & max.date.p2 <= 31) & !(max.date.p3 > 12 & max.date.p3 <= 31)) {
      dt.format[1] <- "d"
    } else if (!(max.date.p1 > 12 & max.date.p1 <= 31) & (max.date.p2 > 12 & max.date.p2 <= 31) & !(max.date.p3 > 12 & max.date.p3 <= 31)) {
      dt.format[2] <- "d"
    } else if (!(max.date.p1 > 12 & max.date.p1 <= 31) & !(max.date.p2 > 12 & max.date.p2 <= 31) & (max.date.p3 > 12 & max.date.p3 <= 31)) {
      dt.format[3] <- "d"
    }
    
    # If the following steps could classify exactly 1 position, be more flexible
    # for the last two: If we can only ID the month...
    # And of the remaining, 1 has 1:31 and the other does not...
    # ...then assign 1:31 the day.

    if ((dt.format[1] == "m" & dt.format[2] == "-1" & dt.format[3] == "-1") & identical(date.pos2, 1:31) & !identical(date.pos3, 1:31))  {
      dt.format[2] <- "d"
    }
    if ((dt.format[1] == "m" & dt.format[2] == "-1" & dt.format[3] == "-1") & !identical(date.pos2, 1:31) & identical(date.pos3, 1:31))  {
      dt.format[3] <- "d"
    }
    if ((dt.format[1] == "-1" & dt.format[2] == "m" & dt.format[3] == "-1") & identical(date.pos1, 1:31) & !identical(date.pos3, 1:31)) {
      dt.format[1] <- "d"
    }
    if ((dt.format[1] == "-1" & dt.format[2] == "m" & dt.format[3] == "-1") & !identical(date.pos1, 1:31) & identical(date.pos3, 1:31)) {
      dt.format[3] <- "d"
    }
    if ((dt.format[1] == "-1" & dt.format[2] == "-1" & dt.format[3] == "m") & identical(date.pos1, 1:31) & !identical(date.pos2, 1:31))  {
      dt.format[1] <- "d"
    }
    if ((dt.format[1] == "-1" & dt.format[2] == "-1" & dt.format[3] == "m") & !identical(date.pos1, 1:31) & identical(date.pos2, 1:31))  {
      dt.format[2] <- "d"
    }
    
    # use if the caller gave the month position as 1 and we've not deduced it:
    dt.format[1]  <- ifelse(mnth.pos == 1 & dt.format[1] == -1
                      & dt.format[2] != "m" & dt.format[3] != "m", "m", dt.format[1])
    
    # if exactly one date position is missing, fill it:
    dt.format     <- CompleteSpan(dt.format, c("y", "m", "d"), missing.val = "-1")
  }
  dt.format       <- paste0(dt.format, collapse = "")  # combine the date positions
  # Now get the times.
  if (has.times) {
    n.time.pos    <- n.elements - date.len  # first 3 are dates
    # assume Hours:Minutes:Seconds
    time.format   <- substring("HMS", 0, min(3, n.time.pos))
    if (n.time.pos > 3) {
      msg         <- paste(n.time.pos, "time elements were found.
                           Elements beyond 3 may be discarded.")
      warning(msg)
    }
    paste0(dt.format, time.format)  # return merged date-time format
  } else {
    dt.format  # return date format
  }
}