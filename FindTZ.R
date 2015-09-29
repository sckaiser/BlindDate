FindTZ <- function(x) {
  # Finds and removes timezone abbreviations in a character vector
  # Args:
  #  x, a character vector
  # Returns:
  #  TBD, but probably the character vector and a vector of timezones,
  #  possibly just the most common
  
  # Find timezone names at the end of the input string by matching against a
  # vector of known timezones. Because we have to work on the entire input
  # string, this can be computationally intensive. Since many input 
  # strings won't have timezones, we can start by checking for a string at the
  # end of the input string, and stop if not present.
  
  # Call this function after converting months from names to numbers to avoid
  # matching text months. If timezones are present, the hour will also likely be
  # present. Assume time elements come at the end of the string.
  
  H            <- "[0-2]?[0-9]"  # 24h, don't assume the first digit is zero
  M <- S       <- "[0-6]?[0-9]?"
  decimal      <- "[0-9]*"  # allow for decimal seconds/time
  sep          <- "[:punct:]?"  # allow any punctuation to separate time values
  AM.PM        <- "[AM]?[PM]?"
  TZ           <- "[alnum]*[/]?[-]?[+]?[alnum]*"
  # the TZ still needs to handle underscores... up to two of them.
  

    # allow for an optional trailing decimal
  pattern      <- paste0(H, sep, M, sep, S, sep, decimal, "[:space:]*", AM.PM,
                         "[:space:]*", TZ, "[:space:]*$")
  # note that [:punct:] may need to be replaced with [[:punct]] or similar
  
  # need to make allowance for AM/PM.
  
  # If it is present, strip the timezone from the input string & return x with
  # the timezone removed, and a vector of the timezones.
  grepl(pattern, x)  # return value for testing
}

test.tz <- paste("12:21:51 AM", OlsonNames())
test.tz[FindTZ(test.tz)]  # what are we matching?
test.tz[!FindTZ(test.tz)]  # what are we missing?