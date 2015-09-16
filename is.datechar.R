is.datechar <- function(x) {
  # Indicates whether a vector is a date vector stored as a character vector.
  # Args:
  #  x, a vector
  # Returns:
  #  a length-one logical indicating whether the vector is "date-as-character".

  if (is.character(x)) {
    x            <- CleanText(x)  # handle extra spaces and NA-synonyms
    x            <- sub("\\.[0-9]+$", "", x)  # remove trailing decimals
    sample.size  <- 4000
    sample.size  <- min(sample.size, length(x))
    x            <- x[sample(length(x), sample.size)]  # sample for speed
    
    text.month   <- ConvertTextMonth(x, F)
    if (text.month > .5) {  # if more than 50% have text months...
      T  # return True
    } else {
      x          <- MultiGsub(c("AM", "PM"), c("", ""), x)  # Remove AM & PM
      dt.format  <- GuessFormat(x)  # Guess format
      !grepl("-1", dt.format)  # if dt.format has -1, we couldn't fully parse
    }
  } else {
    F  # if it's not a character vector, it's not date-as-character.
  }
}