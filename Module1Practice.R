#testing Git collaboration

#WQP Data Assessed: https://www.waterqualitydata.us/portal/#countrycode=US&statecode=US%3A34&countycode=US%3A34%3A003&siteType=Lake%2C%20Reservoir%2C%20Impoundment&siteType=Stream&sampleMedia=water&sampleMedia=Water&startDateLo=11-01-2015&startDateHi=11-01-2020&mimeType=csv
#List of library packages that may be useful. Ones that are not used are #'ed out

#library(caTools)
library(data.table)
#library(datasets)
#library(dplyr)
#library(DT)
#library(EnvStats)
#library(FSA)
#library(ggplot2)
library(lubridate)
#library(hrbrthemes)
#user may need to install.packages("hrbrthemes")
#library(markdown)
#library(readr)
#library(readxl)
#library(sf)
#library(stats)
library(stringr)
#library(tidyr)
#library(tidyverse)
#library(tbrf)
#library(utils)
#library(zoo)


# Hardwired inputs for now
# set directory
setwd("~/R_Projects/group_TADA")
fn = "WQP_Sample_Raw-11-25-20.csv"
coi = "Temperature, water"
mypercentile = .3
# end of hardwired inputs
#

# this source command would be replaced by library when package is created
source("threshold.R")
source("selectCOI.R")


df = read.csv(fn)

#replace ResultMeasureValue column in df as a numeric version of the column
df[, c("ResultMeasureValue")] = as.numeric(df[, c("ResultMeasureValue")])

#Use this instead if you want to suppress error: 'NAs introduced by coercion'
#df[, c("ResultMeasureValue")] = suppressWarnings(as.numeric(df[, c("ResultMeasureValue")]))

# years
df$Year = year(df$ActivityStartDate)

#View List of Unique Characteristics (charnames)
#Create a new subset of df called charnames with only unique characteristic names (drop duplicates)
charnames = df[, c("CharacteristicName")]
charnames = charnames[!duplicated(charnames)]

#Test SelectCOI function
df2 = selectCOI(df, coi)


#View List of Unique Units Associated with your coi (unit)
#Create a new subset of df2 called unit with only unique units (drop duplicates)
unit = df2[, c("ResultMeasure.MeasureUnitCode")]
unit = unit[!duplicated(unit)]
#View(unit)

mythresh = threshold(df2, mypercentile)

#Create filtered dataframes for results above and below defined criteria. Partition data into above and below the threshold so it can be charted in different colors
#FYI either notation works here
#df3 = subset(df2, df2[, c("ResultMeasureValue")]>threshold)
df3 = subset(df2, ResultMeasureValue > mythresh)

# format date using lubridate
df3$FormattedDate = ymd(df3$ActivityStartDate)
#df4 = subset(df2, df2[, c("ResultMeasureValue")]<=threshold)
df4 = subset(df2, ResultMeasureValue <= mythresh)
#reformat date column
#df4$FormattedDate=as.Date(df4$ActivityStartDate, format="%Y-%m-%d") #%m/%d/%Y
# lubridate
df4$FormattedDate = ymd(df4$ActivityStartDate)

#Create scatterplot of coi results over time
#Plot of number of instances above and below the threshold vs. year.
#define x1, x2, y1, and y2
x1=df3$FormattedDate
y1=df3$ResultMeasureValue
x2=df4$FormattedDate
y2=df4$ResultMeasureValue
#scatterplot with different colors based on threshold
#dev.new()
#xlim and ylim here fixes the x and y axis max and min so that it works for both x's and y's
plot(x1,y1,col="red",pch=16 , cex=1.3,main=paste(coi," vs Time"), xlab="Time", ylab=paste(coi,"(",unit,")"), xlim=c(min(min(x1),min(x2)),max(max(x1),max(x2))),ylim=c(min(min(y1),min(y2)),max(max(y1),max(y2))))
#points adds x2 and y2onto the same fig with x1 and y1
points(x2, y2, col="blue",pch=16 , cex=1.3)

#View table of number of instances above and below the defined criteria for each year (dfyears)
#Loop- Create a new subset of df2 (subset of df with only the characteristic of interest) called year with only unique years from the ActivityStartDate column. Loop through all years to find the number of instances above and below the threshold or percentile.
year = df2[, c("Year")]
year = year[!duplicated(year)]

numabove=nrow(df3)  #sum(df2$ResultMeasureValue > threshold)
numbelow=nrow(df4)  #sum(df2$ResultMeasureValue <= threshold)
numtotal=nrow(df2)
percentabove=round(100*numabove/numtotal, 2)
percentbelow=round(100*numbelow/numtotal, 2)

#initialize an empty vector, vec = vector(). Loop through all years starting at 0 (i=0) and going to the max year using length(year) and i=i+1. Stops looping when it has completed all years (it does this using a "while" loop).
i=0
Number_Above = vector()
while (i<length(year))
{
  n=sum(df3$Year==year[i])
  Number_Above = c(Number_Above, n)
  i=i+1
}

i=0
Number_Below = vector()
while (i<length(year))
{
  n=sum(df4$Year==year[i])
  Number_Below = c(Number_Below, n)
  i=i+1
}

#new dataframe with years, numabove, and numbelow
dfyears=data.frame(year, Number_Above, Number_Below)
#View(dfyears)

#Scatter plot of the number of instances above and below the defined criteria for each year
#New scatter plot of vectors by years
#define x3, x4, and y3
x3=dfyears$year
y3=dfyears$Number_Below
y4=dfyears$Number_Above

#scatterplot with different colors based on threshold
#dev.new()
plot(x3,y3,col="blue",pch=16 , cex=1.3,main=paste(coi,("Number of Instances vs Year")), xlab="Year", ylab="Number of Instances",ylim=c(min(min(y3),min(y4)),max(max(y3),max(y4)))) #ylim here fixes the y axis max and min so that it works for both y's
points(x3, y4, col="red",pch=16 , cex=1.3) #points adds y4 onto the same fig with y3

