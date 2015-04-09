guessFormat <- function(x) {
  # Extends {lubridate}'s guess_formats to guess the orders argument, then picks the most common format.
  # Args:
  #  x, a character vector of dates.
  # Returns:
  #  format.guess, a date-time format interpretable by strptime, etc.
  x <- x[!is.na(x)]  # when guessing the format, ignore NAs as they provide no clues and complicate downstream steps if kept.
  sample.size <- 4000
  if (length(x) > sample.size) {
    x <- x[sample(length(x), sample.size)]  # for speed, sample  rather than computing all.
  }
  space.count <- sum(grepl(" ", x))
  has.times <- ifelse(space.count > .5 * length(x), T, F)
  if (has.times) {
    split.date <- strsplit(x, " ")  # assume date-time separated by " "
    split.date <- unlist(split.date)
    split.date <- split(split.date, 1:length(split.date) %% 2 == 0)  # pick every other element
    dates <- as.vector(unlist(split.date[1])) 
  } else {
    dates <- x
  }
  date.sep <- gsub("[0123456789]", "", dates)  # str_split([:punct:]) isn't working, causing the next 3 LoC: 
  date.sep <- paste(date.sep, collapse = "")  # combine
  date.sep <- str_split(date.sep, pattern = "")  # split into individual characters
  date.sep <- table(date.sep)  # count how often each character appears
  date.sep <- names(which.max(date.sep))  # pick the most frequent
  dates <- strsplit(dates, date.sep)  # split on the presumed separator.
  date.pos <- unlist(lapply(dates, length))  # count the date posistions in each observation.
  n.date.pos <- true.mode(date.pos)  # choose the most common number.
  dates <- unlist(dates)
  dates <- as.integer(dates)  # we should be left with integers given the gsub() call above.
  if (n.date.pos == 1) {
    date.pos1 <- dates
    # Assume year & assume after year 999 and before year 10000.
    year.digits <- nchar(dates)  # count number of digits
    year.digits <- true.mode(year.digits)  # pick most common.
    if (year.digits == 2) {
      date.format <- "y"
    } else if (year.digits == 4) {
      date.format <- "Y"
    } else {
      date.format <- -Inf  # couldn't figure it out.
    }
  } else if (n.date.pos == 2) {
    # Assume we have a month and a year.
    dates <- split(dates, 1:length(dates) %% 2 == 0)
    date.pos1 <- as.vector(unlist(dates[1]))
    date.pos2 <- as.vector(unlist(dates[2]))
    pos1.digits <- nchar(date.pos1)  # how many characters
    pos2.digits <- nchar(date.pos2)
    pos1.digits <- max(pos1.digits)  # take the max (months & days may only have 1 or 2)
    pos2.digits <- max(pos2.digits)  # ditto
    if (pos1.digits == 4 & pos2.digits < 4) {
      date.format <- "Ym"
    } else if (pos1.digits < 4 & pos2.digits == 4) {
      date.format <- "mY"
    } else if (max(date.pos1) <= 12 & max(date.pos2) > 12) {
      date.format <- "my"
    } else if (max(date.pos1) > 12 & max(date.pos2) <= 12) {
      date.format = "ym"
    } else {
      date.format <- -Inf  # couldn't figure it out.
    }
  } else if (n.date.pos == 3) {
    pos1 <- pos2 <- pos3 <- -1  # intialize
    dates <- split(dates, 1:length(dates) %% 3 == 0)
    date.pos3 <- as.vector(unlist(dates[2]))
    dates <- as.vector(unlist(dates[1]))
    dates <- split(dates, 1:length(dates) %% 2 == 0)
    date.pos1 <- as.vector(unlist(dates[1]))
    date.pos2 <- as.vector(unlist(dates[2]))
    date.pos1 <- unique(date.pos1)
    date.pos2 <- unique(date.pos2)
    date.pos3 <- unique(date.pos3)
    date.pos1 <- date.pos1[order(date.pos1)]
    date.pos2 <- date.pos2[order(date.pos2)]
    date.pos3 <- date.pos3[order(date.pos3)]
    # We could be more conservative & assumption if we have fairly big data that we'll see all months & days.
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
    # Then, we could look and see if only one position is missing, and assign it.
    
    if ((pos1 == "m" | pos2 == "m" | pos3 == "m") & (pos1 == "d" | pos2 == "d" | pos3 == "d") & (pos1 == -1 | pos2 == -1 | pos3 == -1)) {
      # If month and day but not year are assigned, assign the year.
      year.pos <- which(c(pos1, pos2, pos3) == -1)
      year.dates <- get(paste0("date.pos", year.pos))
      year.pos <- paste0("pos", year.pos)
      year.digits <- nchar(year.dates)
      year.digits <- max(year.digits)  # Could alternately use true.mode() here.
      if (year.digits == 4) {
        year.format <- "Y"
      } else if (year.digits == 2) {
        year.format <- "y"
      } else {
        year.format <- "NA"
      }
      assign(year.pos, year.format)
    }
    date.format <- paste0(pos1, pos2, pos3)
  }  
  # Now get the times.
  if (has.times) {
    times <- as.vector(unlist(split.date[2]))
    times <- strsplit(times, "[:punct:]")
    time.pos <- unlist(lapply(times, length))  # count the time posistions in each observation.
    n.time.pos <- true.mode(time.pos)  # choose the most common number.
    if (n.time.pos == 0) {
      time.format <- ""  # no time
    } else if (n.time.pos == 1) {
      time.format <- "h"  # hours
    } else if (n.time.pos == 2) {
      time.format <- "hm"  # h:m
    } else if (n.time.pos == 3) {
      time.format <- "hms"  # h:m:s
    }
    guess.orders <- paste0(date.format, time.format)  # concatenate the date & time formats
  } else {
    guess.orders <- date.format
  }
  # guess.orders <- "mdYhm"  # old static assignment
  formats <- guess_formats(x, guess.orders)  # use {lubridate}'s format guesser.
  formats <- table(formats)  # count each format's frequency.
  format.guess <- names(which.max(formats))  # pick the most common
  return(format.guess)
}