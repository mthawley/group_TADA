#select_COI

selectCOI <- function(x, coi = df$CharacteristicName) {
  df2 = subset(x, CharacteristicName == coi)
  df2 = df2[!(is.na(df2$ResultMeasureValue) | df2$ResultMeasureValue==""), ]
  return(df2)
}