GuessFormat <- function(x, sample.size = length(x)) {
  # Guesses the orders argument of a character string representing dates.
  # Args:
  #  x, a character vector of dates.
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
  date.format  <- -Inf  # initialize with a failure value
  if (n.date.pos == 1) {
    # Assume a year.
    date.format <- YearLength(dates)
  } else if (n.date.pos == 2) {
    # Assume a month and a year.
    pos1.digits <- nchar(date.pos1) # how many characters
    pos2.digits <- nchar(date.pos2)
    pos1.digits <- max(pos1.digits) # max as months & days may only have 1 or 2
    pos2.digits <- max(pos2.digits) # ditto
    if (pos1.digits == 4 & pos2.digits < 4) {
      date.format <- "Ym"
    } else if (pos1.digits < 4 & pos2.digits == 4) {
      date.format <- "mY"
    } else if (max(date.pos1) <= 12 & max(date.pos2) > 12) {
      date.format <- "my"
    } else if (max(date.pos1) > 12 & max(date.pos2) <= 12) {
      date.format <- "ym"
    }
  } else if (n.date.pos == 3) {
    pos1   <- pos2 <- pos3 <- -1 # intialize
    # We could be more conservative & assume that if we have fairly big data 
    # then we'll see all months & days; but for now, just look for ranges.
    # If exactly one position is between 1 and 12, assign that as the month.
    max.date.p1 <- max(date.pos1)
    max.date.p2 <- max(date.pos2)
    max.date.p3 <- max(date.pos3)
    if (max.date.p1 <= 12 & max.date.p2 > 12 & max.date.p3 > 12) {
      pos1 <- "m"
    } else if (max.date.p1 > 12 & max.date.p2 <= 12 & max.date.p3 > 12) {
      pos2 <- "m"
    } else if (max.date.p1 > 12 & max.date.p2 > 12 & max.date.p3 <= 12) {
      pos3 <- "m"
    }
    # If exactly one position is between 1 and 31, assign that as the day.
    # Note that this will be unable to classify cases where all date positions
    # are between 1 & 12, i.e., 1/11/10, 1/12/12, etc.)
    if ((max.date.p1 > 12 & max.date.p1 <= 31) & !(max.date.p2 > 12 & max.date.p2 <= 31) & !(max.date.p3 > 12 & max.date.p3 <= 31)) {
      pos1 <- "d"
    } else if (!(max.date.p1 > 12 & max.date.p1 <= 31) & (max.date.p2 > 12 & max.date.p2 <= 31) & !(max.date.p3 > 12 & max.date.p3 <= 31)) {
      pos2 <- "d"
    } else if (!(max.date.p1 > 12 & max.date.p1 <= 31) & !(max.date.p2 > 12 & max.date.p2 <= 31) & (max.date.p3 > 12 & max.date.p3 <= 31)) {
      pos3 <- "d"
    }
    
    # If the following steps could classify exactly 1 position, be more flexible
    # for the last two: If we can only ID the month...
    # And of the remaining, 1 has 1:31 and the other does not...
    # ...then assign 1:31 the day.

    if ((pos1 == "m" & pos2 == "-1" & pos3 == "-1") & identical(date.pos2, 1:31) & !identical(date.pos3, 1:31))  {
      pos2 <- "d"
    }
    if ((pos1 == "m" & pos2 == "-1" & pos3 == "-1") & !identical(date.pos2, 1:31) & identical(date.pos3, 1:31))  {
      pos3 <- "d"
    }
    if ((pos1 == "-1" & pos2 == "m" & pos3 == "-1") & identical(date.pos1, 1:31) & !identical(date.pos3, 1:31)) {
      pos1 <- "d"
    }
    if ((pos1 == "-1" & pos2 == "m" & pos3 == "-1") & !identical(date.pos1, 1:31) & identical(date.pos3, 1:31)) {
      pos3 <- "d"
    }
    if ((pos1 == "-1" & pos2 == "-1" & pos3 == "m") & identical(date.pos1, 1:31) & !identical(date.pos2, 1:31))  {
      pos1 <- "d"
    }
    if ((pos1 == "-1" & pos2 == "-1" & pos3 == "m") & !identical(date.pos1, 1:31) & identical(date.pos2, 1:31))  {
      pos2 <- "d"
    }
    
    # if exactly one date position is missing, fill it:
    date.format   <- CompleteSpan(c(pos1, pos2, pos3), c("y", "m", "d"), missing.val = "-1")
    date.format   <- paste0(date.format, collapse = "")  # combine the date positions
  }
  # Now get the times.
  if (has.times) {
    n.time.pos    <- n.elements - date.len  # first 3 are dates
    if (n.time.pos == 0) {
      time.format <- ""  # no time
    } else if (n.time.pos == 1) {
      time.format <- "H"  # Hours
    } else if (n.time.pos == 2) {
      time.format <- "HM"  # Hours:Minutes
    } else if (n.time.pos == 3) {
      time.format <- "HMS"  # Hours:Minutes:Seconds
    } else if (n.time.pos > 3) {
      msg         <- paste(n.time.pos, "time elements were found.
                           Elements beyond 3 may be discarded.")
      warning(msg)
      time.format <- "HMS"  # h:m:s + unknown others
    }
    paste0(date.format, time.format)  # return merged date-time format
  } else {
    date.format  # return date format
  }
}