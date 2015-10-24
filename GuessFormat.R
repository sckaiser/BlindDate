GuessFormat <- function(x, mnth.pos = NA, sample.size = length(x)) {
  # Guesses the orders argument of a character string representing dates.
  # Args:
  #  x, a character vector of dates.
  #  mnth.pos, an integer indicating which date-time element had a text month.
  #  sample.size, an optional integer to specify random sampling of x.
  # Returns:
  #  the sequence of year, month, day, hour, minute, and second or some subset;
  #      interpretable by {lubridate}'s parse_date_time().
  
  # ignore NAs & blanks which provide no clues and complicate downstream steps:
  x            <- x[!is.na(x)]
  if (length(x) > sample.size) {
    x          <- x[sample(length(x), sample.size)]  # sample for speed
  }
  n.elements   <- CountElements(x)
  date.len     <- 3L  # assume up to the first 3 elements are dates
  n.date.pos   <- min(n.elements, date.len)
  has.times    <- ifelse(n.elements > date.len, T, F)  # more elements = times
  x            <- TokenizeDt(x)  # create a list of date element vectors
  x            <- unlist(x)
  dates        <- SplitVectors(x, n.elements)
  dates        <- lapply(dates, as.integer)  # TokenizeDt() ensures integers
  dates        <- lapply(dates, UniqueOrder)
  date.pos1    <- dates[[1]]
  date.pos2    <- ListExtract(dates, 2L)
  date.pos3    <- ListExtract(dates, 3L)
  dt.format    <- rep(-1, n.date.pos) # initialize with a failure value
  full.span    <- c("y", "m", "d")[1:n.date.pos]  # the complete span
  max.dt.p1    <- max(date.pos1)
  max.dt.p2    <- if (is.null(date.pos2)) 1L[0] else max(date.pos2)
  max.dt.p3    <- if (is.null(date.pos3)) 1L[0] else max(date.pos3)
  year.pos     <- FindYear(dates[1:n.date.pos])  # check for 4 digit dates
  if (year.pos >= 1 && year.pos <= n.date.pos) {
    dt.format[year.pos] <- full.span[1] <- "Y"  # assign; & adjust span
  }
  if (n.date.pos == 1L) {
    # Assume a year.
    dt.format  <- YearLength(dates)
  } else if (n.date.pos == 2L) {
    # Assume a month and a year.
    # Check if either has values under 12:
    if (max.dt.p1 <= 12 && max.dt.p2 > 12) {
      dt.format[1] <- "m"
    } else if (max.dt.p1 > 12 && max.dt.p2 <= 12) {
      dt.format[2] <- "m"
    }
  } else if (n.date.pos == 3L) {
    # We could be more conservative & assume that if we have fairly big data 
    # then we'll see all months & days; but for now, just look for ranges.
    # If exactly one position is between 1 and 12, assign that as the month.
    if (max.dt.p1 <= 12 && max.dt.p2 > 12 && max.dt.p3 > 12) {
      dt.format[1] <- "m"
    } else if (max.dt.p1 > 12 && max.dt.p2 <= 12 && max.dt.p3 > 12) {
      dt.format[2] <- "m"
    } else if (max.dt.p1 > 12 && max.dt.p2 > 12 && max.dt.p3 <= 12) {
      dt.format[3] <- "m"
    }
    # If exactly one position is between 1 and 31, assign that as the day.
    # Note that this will be unable to classify cases where all date positions
    # are between 1 & 12, i.e., 1/11/10, 1/12/12, etc.)
    if ((max.dt.p1 > 12 && max.dt.p1 <= 31) && !(max.dt.p2 > 12 && max.dt.p2 <= 31) && !(max.dt.p3 > 12 && max.dt.p3 <= 31)) {
      dt.format[1] <- "d"
    } else if (!(max.dt.p1 > 12 && max.dt.p1 <= 31) && (max.dt.p2 > 12 && max.dt.p2 <= 31) && !(max.dt.p3 > 12 && max.dt.p3 <= 31)) {
      dt.format[2] <- "d"
    } else if (!(max.dt.p1 > 12 && max.dt.p1 <= 31) && !(max.dt.p2 > 12 && max.dt.p2 <= 31) && (max.dt.p3 > 12 && max.dt.p3 <= 31)) {
      dt.format[3] <- "d"
    }
    
    # If the following steps could classify exactly 1 position, be more flexible
    # for the last two: If we can only ID the month...
    # And of the remaining, 1 has 1:31 and the other does not...
    # ...then assign 1:31 the day.

    if ((dt.format[1] == "m" && dt.format[2] == "-1" && dt.format[3] == "-1") && identical(date.pos2, 1:31) && !identical(date.pos3, 1:31))  {
      dt.format[2] <- "d"
    }
    if ((dt.format[1] == "m" && dt.format[2] == "-1" && dt.format[3] == "-1") && !identical(date.pos2, 1:31) && identical(date.pos3, 1:31))  {
      dt.format[3] <- "d"
    }
    if ((dt.format[1] == "-1" && dt.format[2] == "m" && dt.format[3] == "-1") && identical(date.pos1, 1:31) && !identical(date.pos3, 1:31)) {
      dt.format[1] <- "d"
    }
    if ((dt.format[1] == "-1" && dt.format[2] == "m" && dt.format[3] == "-1") && !identical(date.pos1, 1:31) && identical(date.pos3, 1:31)) {
      dt.format[3] <- "d"
    }
    if ((dt.format[1] == "-1" && dt.format[2] == "-1" && dt.format[3] == "m") && identical(date.pos1, 1:31) && !identical(date.pos2, 1:31))  {
      dt.format[1] <- "d"
    }
    if ((dt.format[1] == "-1" && dt.format[2] == "-1" && dt.format[3] == "m") && !identical(date.pos1, 1:31) && identical(date.pos2, 1:31))  {
      dt.format[2] <- "d"
    }
    
    # if caller gave the month position as 1 and we've not deduced it:
    if (mnth.pos == 1 && dt.format[1] == -1 && !"m" %in% dt.format) {
      dt.format[1] <- "m"
    }
  }
  dt.format       <- CompleteSpan(dt.format, full.span, "-1")  # impute
  dt.format       <- paste0(dt.format, collapse = "")  # combine date positions
  # Now get the times.
  if (has.times) {
    n.time.pos    <- n.elements - date.len  # first 3 are dates
    # assume Hours:Minutes:Seconds
    time.format   <- substring("HMS", 0, min(3L, n.time.pos))
    if (n.time.pos > 3L) {
      msg         <- paste(n.time.pos, "time elements were found.
                           Elements beyond 3 may be discarded.")
      warning(msg)
    }
    paste0(dt.format, time.format)  # return merged date-time format
  } else {
    dt.format  # return date format
  }
}