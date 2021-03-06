is.datechar <- function(x) {
  # Indicates whether a vector is a date vector stored as a character vector.
  # Args:
  #  x, a vector
  # Returns:
  #  a length-one logical indicating whether the vector is "date-as-character".
  
  cores         <- detectCores() - 1L
  if (is.character(x)) {
    x           <- CleanText(x)  # handle extra spaces and NA-synonyms
    x           <- sub("\\.[0-9]+$", "", x)  # remove trailing decimals
    sample.sz   <- min(4000L, length(x))
    x           <- UnSample(x, sample.sz)  # sample for speed
    
    text.month  <- ConvertTextMonth(x, F)
    if (is.useful(ConvertTextMonth, x, 1, sample.sz, pos = F)) {
      T 
    } else {
      x         <- MultiGsub(c("AM", "PM"), c("", ""), x)[[1]]  # Remove AM & PM
      dt.format <- try(GuessFormat(x), silent = T)  # Guess format
      if (class(dt.format)[1] == "try-error") {
        return(F)  # GuessFormat didn't work
      }
      !grepl("-1", dt.format)  # if dt.format has -1, we couldn't fully parse
    }
  } else {
    F  # if it's not a character vector, it's not date-as-character
  }
}