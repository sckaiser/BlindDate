CleanText <- function(x) {
  # Removes extraneous spaces and replaces text variants of NA with NA
  # Args:
  #  x, a character string
  
  x                <- RmDupSpace(x) # trim leading, trailing & duplicate spaces
  NA.syn           <- c("", "NULL", "Not Applicable", "NA", "N/A")
  x[x %in% NA.syn] <- NA  # convert NA synonyms
  return(x)
}