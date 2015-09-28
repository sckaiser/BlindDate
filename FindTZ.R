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
  sep          <- "[:punct:]?"  # allow any punctuation to separate time values
  AM.PM        <- # to be added
  TZ           <- # to be added
    # allow for an optional trailing decimal
  pattern      <- paste0(H, sep, M, sep, S, sep, "[0-9]*",
                        "[:space:]*", AM.PM, "[:space:]*", TZ)
  # note that [:punct:] may need to be replaced with [[:punct]] or similar
  
  # need to make allowance for AM/PM.
  
  # If it is present, strip the timezone from the input string & return x with
  # the timezone removed, and a vector of the timezones.
}