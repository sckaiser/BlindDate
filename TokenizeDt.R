TokenizeDt <- function(x) {
  # Converts a character string to a list of character vectors; each vector
  # has the tokenized date elements.
  # Args:
  #  x, a character vector
  # Returns:
  #  y, a list of character vectors.
  
  x <- gsub("[[:punct:]]", " ", x)  # use a single, common separtor
  x <- gsub("[:blank:]", " ", x)  # ditto
  x <- RmDupSpace(x)  # handle Oxford comma
  str_split(x, " ")  # list of original x, each with elements in a vector
}