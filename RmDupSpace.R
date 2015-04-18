RmDupSpace <- function(x) {
  # Removes duplicate spaces using a lookbehind approach.
  # Ref: https://stackoverflow.com/questions/14737266/removing-multiple-spaces-and-trailing-spaces-using-gsub
  gsub("^ *|(?<= ) | *$", "", x, perl = T)
}