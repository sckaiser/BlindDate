ConvertTextMonth <- function(x, convert = T, ignore.null = T) {
  # Checks or converts a text month to a number within a date-time string.
  # Args:
  #  x, a known or possible date-time character vector.
  #  convert, a logical indicating whether to convert to the month number.
  #  ignore.null, a logical indicating whether to ignore NA, "", and "NULL".
  # Returns:
  #  if convert = T, a character vector with text months replaced by numbers.
  #  if convert = F, a numeric indicating the proportion of x with text months.
  
  m1         <- c("January", "February", "March", "April",
                  "May", "June", "July", "August",
                  "September", "October", "November", "December")
  m2         <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  m3         <- c("Janvier", "Fevrier", "Mars", "Avril",
                  "Mai", "Juin", "Juillet", "Aout",
                  "Septembre", "Octobre", "Novembre",
                  "Decembre")  # extensibility example
  date.df    <- data.frame(m1 = m1, m2 = m2, m3 = m3, stringsAsFactors = F)
  if (ignore.null) {
    # Handle various string synonyms for NA.
    NA.syn   <- c("", "NULL", "Not Applicable", "NA")
    x[x %in% NA.syn] <- NA
  }
  # Find the best matching column.
  matches    <- apply(date.df, 2, GreplAny, x, ignore.case = T)   # match months
  if (!is.matrix(matches)) {
    matches    <- as.integer(matches)  # if only 1 row, convert it
  } else {
    matches    <- apply(matches, 2, sum)  # sum by column
  }
  denom      <- length(x)  # exclude NAs and null strings
  if (ignore.null) {
    denom    <- length(x[!is.na(x)])
  }
  matches    <- matches / denom   # calculate the proportion of matches
  best.col   <- WhichMax(matches)  # select the highest, breaking ties randomly
  proportion.matched <- as.vector(matches[best.col])
  if (convert) {
    patterns <- date.df[ , best.col]
    x.new    <- MultiGsub(patterns, 1:12, x, ignore.case = T)
    return(x.new)
  } else {
    return(proportion.matched)
  }
}