ConvertDateTime <- function(x, t.format = "POSIXct", tz = "UTC", trim.dec = F) {
  # Converts a character vector of dates to a POSIXct vector.
  # Args:
  #  x, a character vector representing dates and possibly times
  #  t.format, whether to return "POSIXct", or "POSIXlt"
  #  tz,  timezone argument
  #  trim.dec, whether to trim decimals from the seconds.
  # Returns:
  #  x.date, a POSIX time vector
  
  x            <- CleanText(x)  # handle extra spaces and NA-synonyms
  if (trim.dec) {
    x          <- sub("\\.[0-9]+$", "", x)  # remove trailing decimals
  }
  sample.size  <- 4000
  sample.size  <- min(sample.size, length(x))
  x.sample     <- x[sample(length(x), sample.size)]  # sample for speed
  
  # Handle Month Text. First, find what proportion of x has text months:
  text.month   <- ConvertTextMonth(x.sample, F)
  mnth.pos     <- NA  # initialize
  if (text.month > .95) {  # if more than 95% have text months...
    out        <- ConvertTextMonth(x)  # ...then convert to numeric months
    x          <- out[[1]]  # extract the converted data
    mnth.pos   <- out[[2]]  # and which element had text months, if any. 
  }
  
  # Handle AM and PM text.
  PM.times     <- grepl("PM", x)  # store vector of which times are PM
  x            <- gsub("PM", "", x)  # Remove PM
  x            <- gsub("AM", "", x)  # Remove AM
  x.sample     <- x[sample(length(x), sample.size)]  # resample
  
  # Guess the format.
  date.format  <- GuessFormat(x.sample, mnth.pos, sample.size)  # Guess format
  # print(date.format)  # debug
  x.date       <- parse_date_time(x, orders = date.format)  # lubridate convert
  if (any(PM.times)) {  # if we had any PM times, fix them.
    x.hour     <- hour(x.date)  # get hours
    
    # Add 12 hours to all PM times on or after 1PM:
    x.date[PM.times & x.hour != 12] <-
              x.date[PM.times & x.hour != 12] + (60 * 60 * 12)
    
    # Subtract 12 hours from all PM times before 1AM:
    x.date[!PM.times & x.hour == 12 & !is.na(x.date)] <-
              x.date[!PM.times & x.hour == 12 & !is.na(x.date)] - (60 * 60 * 12)
  }
  if (tz != "UTC") {
    x.date     <- force_tz(x.date, tzone = tz)  # change timezone if requested
  }
  if (t.format == "POSIXlt") {
    x.date     <- as.POSIXlt(x.date)  # convert if requested
  }
  return(x.date)
}