# Test harness for ConvertDateTime.R script.
pkgs <- c("lubridate", "stringr", "fasttime", "parallel")
sapply(pkgs, library, character.only = T)
setwd("C:/Users/scott_kaiser/Desktop/Analytics and Data Science/BlindDate")
source("SourceDir.R")
SourceDir(exclude = c("format utility.R", "ConvertDateTime_test.R"))

# start at midnight, January 1. Add 61 minutes successively
t.size     <- 24 * 365
start.date <- as.POSIXct("01-01-2015 00:00:00", format = "%m-%d-%Y %H:%M:%S")
tm         <- rep(start.date, t.size)
increment  <- 61 * 60  # increment just more than an hour to generate more times
for (i in 2:t.size) {
  tm[i]    <- tm[i - 1] + increment
}

# One-time initialization:
# write.table(tm, "TestDates.csv", sep = ",", col.names = F, row.names = F)
# open in Excel; change formats. Lots of formats.

test.mat    <- read.csv("TestDates.csv", header = F, colClasses = "character")
test.mat    <- as.matrix(test.mat)
NA.pct      <- 0.02  # replace this percent of cells with NA synonyms
NA.cells    <- sample(length(test.mat), length(test.mat) * NA.pct)
NA.syn      <- c(" ", "NULL", "Not Applicable", "NA", "N/A")
suppressWarnings(test.mat[NA.cells] <- NA.syn)  # muffle vector recycling warn

convert.mat <- matrix(NA, nrow = nrow(test.mat), ncol = ncol(test.mat))
convert.mat <- data.frame(convert.mat)
convert.mat <- setNames(convert.mat, colnames(test.mat))

for (i in 1:ncol(test.mat)) {
  x <- test.mat[ , i]
  # print(GuessFormat(x))  # debug
  print(x[1])
  convert.mat[ , i] <- ConvertDateTime(x)
}

# Test is.datechar() in a separate loop to let each pass/fail independently
is.dt.vec   <- rep(NA, ncol(test.mat))
for (i in 1:ncol(test.mat)) {
  x <- test.mat[ , i]
  is.dt.vec[i]      <- is.datechar(x)
}
  
# This following section is loop-free,
# but harder to debug when a single format conversion fails.
# convert.mat <- apply(test.mat, 2, ConvertDateTime)
results.mat <- apply(convert.mat, 2, identical, tm)
results     <- apply(results.mat, 2, all)

# 
total       <- length(results)
pass        <- sum(results)
fail        <- total - pass
overall     <- pass / total

results <- paste(overall, "percent of", ncol(test.mat) ,"formats passed.")
print(results)
if (overall < 1) {
  print("Test failed on", fail, "formats.")
  print("The first entry of each failed format is provided below:")
  print("Need to add")
}