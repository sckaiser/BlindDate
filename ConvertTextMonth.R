ConvertTextMonth <- function(x, convert = T, na.rm = F, pos = T) {
  # Checks or converts a text month to a number within a date-time string.
  # Args:
  #  x, a known or possible date-time character vector.
  #  na.rm, a logical indicating whether to ignore NA values.
  #  pos, a logical indicating whether to send back the text month position.
  # Returns:
  #  if pos = F, a character vector with text months replaced by numbers.
  #  if pos = T, the vector as above, plus the month's position.
  
  if (na.rm) {
   x <- x[!is.na(x)]
  }
  m1         <- c("January", "February", "March", "April",
                  "May", "June", "July", "August",
                  "September", "October", "November", "December")
  m2         <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  m3         <- c("Janvier", "Fevrier", "Mars", "Avril",
                  "Mai", "Juin", "Juillet", "Aout",
                  "Septembre", "Octobre", "Novembre",
                  "Decembre")  # extensibility example
  # Abbreviations must come after, i.e. be to the right of, the full months to
  # which they correspond. The matching logic requires this sequence.
  date.df    <- data.frame(m1 = m1, m2 = m2, m3 = m3, stringsAsFactors = F)
  # Find the best matching column.
  matches    <- apply(date.df, 2, GreplAny, x, ignore.case = T)   # match months
  if (!is.matrix(matches)) {
    matches  <- as.integer(matches)  # if only 1 row, convert it
  } else {
    matches  <- apply(matches, 2, sum)  # sum by column
  }
  denom      <- length(x)  # exclude NAs and null strings
  if (na.rm) {
    denom    <- length(x[!is.na(x)])
  }
  matches    <- matches / denom   # calculate the proportion of matches
  best.col   <- which.max(matches)  # pick the highest; if tied, pick the first
  patterns   <- date.df[ , best.col]
  out        <- MultiGsub(patterns, 1:12, x, ignore.case = T)  # convert data
  if(pos) {
    mnth.pos <- TrueMode(out[[2]])  # most likely position of converted month
    mnth.pos[mnth.pos != 1] <- NA  # only declare if near-certain
    list(out[[1]], mnth.pos)  # return the converted data & month position
  } else {
    out[[1]]
  }
}