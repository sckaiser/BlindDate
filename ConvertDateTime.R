ConvertDateTime <- function(x, t.format = "POSIXct", tz = "UTC", trim.dec = F) {
  # Converts a character vector of dates to a POSIXct vector.
  # Args:
  #  x, a character vector representing dates and possibly times
  #  t.format, whether to return "POSIXct", or "POSIXlt"
  #  tz,  timezone argument
  #  trim.dec, whether to trim decimals from the seconds.
  # Returns:
  #  x.date, a POSIX time vector
  
  sample.sz  <- min(4000L, length(x))
  if (is.useful(CleanText, x, 1, sample.sz)) {
    x        <- CleanText(x)  # handle extra spaces and NA-synonyms
  }
  if (trim.dec) {
    x        <- sub("\\.[0-9]+$", "", x)  # remove trailing decimals
  }
  mnth.pos   <- NA  # initialize
  if (is.useful(ConvertTextMonth, x, 1, sample.sz, pos = F)) {
    out      <- ConvertTextMonth(x)  # convert text to numeric months
    x        <- out[[1]]  # extract the converted data
    mnth.pos <- out[[2]]  # and which element had text months, if any. 
  }
  
  # Handle AM and PM text.
  if (is.useful(SplitAMPM, x, 1, sample.sz, PM.vector = F)) {
    out      <- SplitAMPM(x)  # remove AM &PM
    x        <- out[[1]]  # extract the converted data
    PM.times <- out[[2]]  # and which elements had AM/PM. 
  }
  
  # Guess the format.
  x.sample   <- x[sample(length(x), sample.sz)]  # resample
  dt.format  <- GuessFormat(x.sample, mnth.pos, sample.sz)  # Guess format
  # print(date.format)  # debug
  if (dt.format == "YmdHMS" && "fasttime" %in% .packages()) {
    x.date     <- fastPOSIXct(x)
  } else {
    x.date     <- parse_date_time(x, orders = dt.format)  # lubridate convert
  }
  if (exists("PM.times")) {  # if we had any PM times, fix them.
    x.hour   <- hour(x.date)  # get hours
    
    # Add 12 hours to all PM times on or after 1PM:
    x.date[PM.times & x.hour != 12] <-
              x.date[PM.times & x.hour != 12] + (60 * 60 * 12)
    
    # Subtract 12 hours from all PM times before 1AM:
    x.date[!PM.times & x.hour == 12 & !is.na(x.date)] <-
              x.date[!PM.times & x.hour == 12 & !is.na(x.date)] - (60 * 60 * 12)
  }
  if (tz != "UTC") {
    x.date   <- force_tz(x.date, tzone = tz)  # change timezone if requested
  }
  if (t.format == "POSIXlt") {
    x.date   <- as.POSIXlt(x.date)  # convert if requested
  }
  return(x.date)
}