SourceDir <- function(exclude = NA, path = getwd()) {
  # Variant of source() help file example;
  # sources all R files in the current directory.
  # Args:
  #  exclude, a list of filenames to exclude. 
  #  path, a directory.
  
  exclude <- c(exclude, "SourceDir.R")  # avoid infinite recursion
  files   <- list.files(path, pattern = "[.][Rr]$")
  files   <- files[!files %in% exclude]
  for (nm in files) {
    source(file.path(nm))
  }
}