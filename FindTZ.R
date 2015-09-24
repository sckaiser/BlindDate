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
  # strings won't have timezones, we can start by checking for a string of 3 
  # or more characters at the end of the input string, and stop if not present.
  
  # We can also call this function after converting months from names to numbers
  # which will 
  
  # If it is present, strip the timezone from the input string & return x with
  # the timezone removed, and a vector of the timezones.
}