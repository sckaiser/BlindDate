MultiGsub <- function(patterns, replacements, x, ...) {
  # Replaces multiple patterns with multiple substitutions.
  # Args:
  #  patterns, a character vector of patterns to be matched.
  #  replacements, a vector of replacements, corresponding to and of the same length as patterns.
  #  x, a character vector on which to find and replace.
  # Returns:
  #  x.new, the modified character vector.
  # TODO: replace the for-loop with an apply.
  if (length(patterns) != length(replacements)) {
    print("Error: patterns and replacements have different lengths")
    stop()
  }
  x.new <- x
  for (i in 1:length(patterns)) {
    x.new <- gsub(patterns[i], replacements[i], x.new, ...)
  } # next pattern-replacement pair
  return(x.new)
}