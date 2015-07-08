GuessFormat <- function(x, sample.size = length(x)) {
  # Guesses the orders argument of a character string representing dates.
  # Args:
  #  x, a character vector of dates.
  #  sample.size, an optional integer to specify random sampling of x.
  # Returns:
  #  guess.orders, the sequence of year, month, day, hour, minute, and second 
  #     or some subset; interpretable by {lubridate}'s parse_date_time().
  
  # ignore NAs & blanks which provide no clues and complicate downstream steps:
  x <- x[!is.na(x) & x != "" & x != " " & x != "NA"]
  if (length(x) > sample.size) {
    x <- x[sample(length(x), sample.size)] # sample for speed
  }
  n.elements   <- CountElements(x)
  date.len     <- 3  # assume up to the first 3 elements are dates
  has.times    <- ifelse(n.elements > date.len, T, F)  # more elements = times
  if (has.times) {
    split.date <- strsplit(x, " ") # assume date-time separated by " "
    split.date <- unlist(split.date)
    split.date <- split(split.date, 1:length(split.date) %% 2 == 0) # pick every other element
    dates      <- as.vector(unlist(split.date[1]))
  } else {
    dates      <- x
  }
  n.date.pos   <- min(n.elements, 3)  # assume up to first 3 are dates.
  dates        <- TokenizeDt(dates)  # create a list of vectors of date elements
  dates        <- unlist(dates)
  dates        <- as.integer(dates)  # the TokenizeDt() call ensures integers
  if (n.date.pos == 1) {
    # Assume we have a year.
    date.format <- YearLength(dates)
  } else if (n.date.pos == 2) {
    # Assume we have a month and a year.
    dates       <- split(dates, 1:length(dates) %% 2 == 0)
    date.pos1   <- UniqueOrder(dates[1])
    date.pos2   <- UniqueOrder(dates[2])
    pos1.digits <- nchar(date.pos1) # how many characters
    pos2.digits <- nchar(date.pos2)
    pos1.digits <- max(pos1.digits) # take the max (months & days may only have 1 or 2)
    pos2.digits <- max(pos2.digits) # ditto
    if (pos1.digits == 4 & pos2.digits < 4) {
      date.format <- "Ym"
    } else if (pos1.digits < 4 & pos2.digits == 4) {
      date.format <- "mY"
    } else if (max(date.pos1) <= 12 & max(date.pos2) > 12) {
      date.format <- "my"
    } else if (max(date.pos1) > 12 & max(date.pos2) <= 12) {
      date.format <- "ym"
    } else {
      date.format <- -Inf # couldn't figure it out.
    }
  } else if (n.date.pos == 3) {
    pos1      <- pos2 <- pos3 <- -1 # intialize
    dates     <- split(dates, 1:length(dates) %% 3 == 0)
    date.pos3 <- UniqueOrder(dates[2])
    dates     <- as.vector(unlist(dates[1]))
    dates     <- split(dates, 1:length(dates) %% 2 == 0)
    date.pos1 <- UniqueOrder(dates[1])
    date.pos2 <- UniqueOrder(dates[2])
   
    # We could be more conservative & assume if we have fairly big data that we'll see all months & days.
    # But for now, just look for ranges.
    # If exactly only one position is between 1 and 12, assign that as the month.
    if ((min(date.pos1) >= 1 & max(date.pos1) <= 12) & !(min(date.pos2) >= 1 & max(date.pos2) <= 12) & !(min(date.pos3) >= 1 & max(date.pos3) <= 12)) {
      pos1 <- "m"
    } else if (!(min(date.pos1) >= 1 & max(date.pos1) <= 12) & (min(date.pos2) >= 1 & max(date.pos2) <= 12) & !(min(date.pos3) >= 1 & max(date.pos3) <= 12)) {
      pos2 <- "m"
    } else if (!(min(date.pos1) >= 1 & max(date.pos1) <= 12) & !(min(date.pos2) >= 1 & max(date.pos2) <= 12) & (min(date.pos3) >= 1 & max(date.pos3) <= 12)) {
      pos3 <- "m"
    }
    # If exactly only one position is between 1 and 31, assign that as the month.
    # Note that this will not be able to classify conditions when all date positions are between 1 & 12, i.e., 1/11/10, 1/12/12, 12/01/01, etc.)
    if ((min(date.pos1) >= 1 & max(date.pos1) > 12 & max(date.pos1) <= 31) & !(min(date.pos2) >= 1 & max(date.pos2) > 12 & max(date.pos2) <= 31) & !(min(date.pos3) >= 1 & max(date.pos3) > 12 & max(date.pos3) <= 31)) {
      pos1 <- "d"
    } else if (!(min(date.pos1) >= 1 & max(date.pos1) > 12 & max(date.pos1) <= 31) & (min(date.pos2) >= 1 & max(date.pos2) > 12 & max(date.pos2) <= 31) & !(min(date.pos3) >= 1 & max(date.pos3) > 12 & max(date.pos3) <= 31)) {
      pos2 <- "d"
    } else if (!(min(date.pos1) >= 1 & max(date.pos1) > 12 & max(date.pos1) <= 31) & !(min(date.pos2) >= 1 & max(date.pos2) > 12 & max(date.pos2) <= 31) & (min(date.pos3) >= 1 & max(date.pos3) > 12 & max(date.pos3) <= 31)) {
      pos3 <- "d"
    }
    
    # One way to further improve would be to check for four digit dates, and if any are found, classify the year.
    # Another improvement, we could look and see if only one position is missing, and assign it.
    
    # If the following steps could classify exactly 1 position, be more flexible for the last two:
    # If we can only ID the month...
    # And of the remaining, 1 has 1:31 and the other does not...
    # ...then assign 1:31 the day.
    if ((pos1 == "m" & pos2 == "-1" & pos3 == "-1") & (date.pos2 == 1:31 & date.pos3 != 1:31))  {
      pos2 <- "d"
    }
    if ((pos1 == "m" & pos2 == "-1" & pos3 == "-1") & (date.pos2 != 1:31 & date.pos3 == 1:31))  {
      pos3 <- "d"
    }
    if ((pos1 == "-1" & pos2 == "m" & pos3 == "-1") & (date.pos1 == 1:31 & date.pos3 != 1:31)) {
      pos1 <- "d"
    }
    if ((pos1 == "-1" & pos2 == "m" & pos3 == "-1") & (date.pos1 != 1:31 & date.pos3 == 1:31)) {
      pos3 <- "d"
    }
    if ((pos1 == "-1" & pos2 == "-1" & pos3 == "m") & (date.pos1 == 1:31 & date.pos2 != 1:31))  {
      pos1 <- "d"
    }
    if ((pos1 == "-1" & pos2 == "-1" & pos3 == "m") & (date.pos1 != 1:31 & date.pos2 == 1:31))  {
      pos2 <- "d"
    }
    
    # If month and day but not year are assigned, assign the year.
    if ((pos1 == "m" | pos2 == "m" | pos3 == "m") & (pos1 == "d" | pos2 == "d" | pos3 == "d") & (pos1 == -1 | pos2 == -1 | pos3 == -1)) {
      year.pos    <- which(c(pos1, pos2, pos3) == -1)
      year.pos    <- paste0("pos", year.pos)
      year.dates  <- get(paste0("date.", year.pos))
      year.format <- YearLength(year.dates)
      assign(year.pos, year.format)
    }
    date.format <- paste0(pos1, pos2, pos3)   # final date assignment
  }
  # Now get the times.
  if (has.times) {
    n.time.pos <- n.elements - date.len  # first 3 are dates
    if (n.time.pos == 0) {
      time.format <- "" # no time
    } else if (n.time.pos == 1) {
      time.format <- "h" # hours
    } else if (n.time.pos == 2) {
      time.format <- "hm" # h:m
    } else if (n.time.pos == 3) {
      time.format <- "hms" # h:m:s
    }
    guess.orders <- paste0(date.format, time.format) # merge date & time formats
  } else {
    guess.orders <- date.format
  }
  return(guess.orders)
}