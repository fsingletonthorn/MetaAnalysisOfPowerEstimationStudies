# load "Kazantzis2000RawData.csv"
library(readr)
Kazantzis2000RawData <- read_csv("FILE LOCATION") # replace "FILE LOCATION" with file location
Kazantzis2000RawData <- Kazantzis2000RawData[1:27,]
quantilesSampleSize<-quantile(Kazantzis2000RawData$n , c(.25,.5,.75))
quantilesPowerValuesmall<-quantile(Kazantzis2000RawData$PowerAtSmall, c(.25,.5,.75))
quantilesPowerValueMedian<-quantile(Kazantzis2000RawData$PowerAtMed, c(.25,.5,.75))
quantilesPowerValueLarge<-quantile(Kazantzis2000RawData$PowerAtLarge, c(.25,.5,.75))

