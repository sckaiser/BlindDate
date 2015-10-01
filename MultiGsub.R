MultiGsub <- function(patterns, replacements, x, ...) {
  # Replaces multiple patterns with multiple substitutions.
  # Args:
  #  patterns, a character vector of patterns to be matched.
  #  replacements, a vector of replacements
  #                corresponding to and of the same length as patterns.
  #  x, a character vector on which to find and replace.
  # Returns:
  #  x.new, the modified character vector.
  #  loc, a vector of the first position in x that matched a pattern.
  
  if(length(replacements) < length(patterns) &&
       length(patterns) %% length(replacements) == 0) {
    replacements <- rep(replacements, length(patterns) %/% length(replacements))
  }
  
  stopifnot(length(patterns) == length(replacements))
  x.new   <- x
  # initialize a matrix to store the position in x of each pattern match
  loc.mat <- matrix(NA, nrow = length(x), ncol = length(patterns))
  for (i in 1:length(patterns)) {
    temp  <- str_locate(x, patterns[i])  # find start & end of the pattern
    loc.mat[ , i] <- temp[ , 1]  # save the start
    x.new <- gsub(patterns[i], replacements[i], x.new, ...)
  } # next pattern-replacement pair
  loc.mat[is.na(loc.mat)] <- Inf  # replace NA with Inf for min processing
  loc     <- apply(loc.mat, 1, min, na.rm = T)  # find first location
  loc[loc == Inf] <- NA  # replace with NA
  list(x.new, loc)  # return
}