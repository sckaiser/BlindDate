CountElements <- function(x) {
  # Counts the number of distinct elements in a character string.
  # An element is a sequence of adjacent numbers or characters, exluduing
  # punctuation and spaces.
  # Args:
  #  x, a character vector
  # Returns:
  #  y, an integer value of the number of elements found.
  
  x <- gsub("[[:punct:]]", " ", x)  # use a single, common separtor
  x <- gsub("[:blank:]", " ", x)  # ditto
  x <- RmDupSpace(x)  # handle Oxford comma
  x <- str_split(x, " ")  # list of original x, each with elements in a vector
  y <- lapply(x, length)  # count elements in each x
  y <- unlist(y)
  TrueMode(y)  # return the most common.
}