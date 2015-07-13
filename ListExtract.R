ListExtract <- function(x, idx) {
  # Extracts one item from a list, returning NULL if the item doesn't exist
  # Args:
  #  x, a list
  #  idx, an index of the list item to remove
  
  if (idx > length(x)) {
    NULL
  } else {
   x[[idx]]
  }
}