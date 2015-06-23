# Format test harness for Convert Format script.
library(lubridate)
library(stringr)
setwd("C:/Users/scott_kaiser/Desktop/Analytics and Data Science/BlindDate")
files <- c("ConvertDateTime.R", "ConvertTextMonth.R", "CountChars.R",
           "CountElements.R", "GreplAny.R", "GuessFormat.R", "MultiGsub.R",
           "RmDupSpace.R", "SetPartition.R", "TrueMode.R", "UniqueOrder.R",
           "WhichMax.R", "YearLength.R")
sapply(files, source)

# start at midnight, January 1. Add 61 minutes successively
t.size     <- 24 * 365
start.date <- as.POSIXct("01-01-2015 00:00:00", format = "%m-%d-%Y %H:%M:%S")
t <- rep(start.date, t.size)
increment  <- 61 * 60  # increment just more than an hour to generate more times
for (i in 2:t.size) {
  t[i] <- t[i - 1] + increment
}

# One-time initialization:
# write.table(t, "TestDates.csv", sep = ",", col.names = F, row.names = F)
# open in Excel; change formats. Lots of formats.

test.mat    <- read.csv("TestDates.csv", header = F, stringsAsFactors = F)
convert.mat <- matrix(NA, nrow = nrow(test.mat), ncol = ncol(test.mat))
convert.mat <- data.frame(convert.mat)
colnames(convert.mat) <- colnames(test.mat)

for (i in 1:ncol(test.mat)) {
  x <- test.mat[ , i]
  # print(GuessFormat(x))  # debug
  print(x[1])
  convert.mat[ , i] <- ConvertDateTime(x)
}

# This following section is loop-free,
# but harder to debug when a single format conversion fails.
# convert.mat <- apply(test.mat, 2, ConvertDateTime)
results.mat <- apply(convert.mat, 2, identical, t)
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