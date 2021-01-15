# Function threshold
#
# Is this function different yet?

# pass in df, return threshold
threshold <- function(x,in_thresh) {
  result = x[, c("ResultMeasureValue")]
  percentiles = quantile(result, in_thresh)
  # for debug
  print(percentiles)
  stats = summary(result)
  print(stats)
    char = x$CharacteristicName
  #print(char)
  #hist(result, main=paste(char,"vs. Frequency"), xlab=char, ...)
  #threshold = readline("What is the threshold percentile (0-1)?")
  threshold = unname(round(quantile(result, c(in_thresh,1), na.rm = TRUE)))
  #threshold <<- threshold
  return(threshold)
}
