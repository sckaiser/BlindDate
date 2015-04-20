# Format test harness for Convert Format script.
library(lubridate, stringr)

# start at midnight, January 1. Add 61 minutes successively.
t.size <- 24 * 365
start.date <- as.POSIXct("01-01-2015 00:00:00", format = "%m-%d-%Y %H:%M:%S")
t <- rep(start.date, t.size)
increment <- 61 * 60  # increment slightly more than an hour to generate more times.
for (i in 2:t.size) {
  t[i] <- t[i - 1] + increment
}

setwd("C:/Users/scott_kaiser/Desktop/Analytics and Data Science/BlindDate")
write.table(t, "scratch.csv", sep = ",", col.names = F, row.names = F)
# open in Excel; change formats. Lots of formats.
test.mat <- read.csv("scratch.csv", header = F, stringsAsFactors = F)
convert.mat <- matrix(NA, nrow = nrow(test.mat), ncol = ncol(test.mat))
convert.mat <- data.frame(convert.mat)
colnames(convert.mat) <- colnames(test.mat)

for (i in 1:ncol(test.mat)) {
  x <- test.mat[ , i]
  # print(GuessFormat(x))
  convert.mat[ , i] <- parse_date_time(x)
  # convert.mat[ , i] <- ConvertDateTime(x)
}

convert.mat <- apply(test.mat, 2, ConvertDateTime)
results.mat <- apply(convert.mat, 2, identical, t)
results     <- apply(results.mat, 2, all)
total       <- length(results)
pass        <- sum(results)
fail        <- total - pass
overall     <- pass / total

print(paste("Test passed on", overall, "percent of formats (", ncol(test.mat), ")"))
if (overall < 1) {
  print("Test failed on", fail, "formats.")
  print("The first entry of each failed format is provided below:")
  print("Need to add")
}