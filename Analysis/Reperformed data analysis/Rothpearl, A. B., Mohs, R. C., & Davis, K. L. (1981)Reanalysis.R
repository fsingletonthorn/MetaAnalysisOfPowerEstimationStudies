# requires "Rothpearl, A. B., Mohs, R. C., & Davis, K. L. (1981)" - raw data file

library(readr)
RawData <- read_csv("FILE LOCATION") # replace "FILE LOCATION" with file location (i.e., "Rothpearl, A. B., Mohs, R. C., & Davis, K. L. (1981)") avaliable on the OSF where the current file was downloaded

quantilesPowerValuesmall<-quantile(RawData$PowerAt.25, c(.25,.5,.75))
quantilesPowerValueMedian<-quantile(RawData$PowerAt.5, c(.25,.5,.75))
meanPowerSmall<-mean(RawData$PowerAt.25)
meanPowerMedium<-mean(RawData$PowerAt.5)
sdPowerSmall<-sd(RawData$PowerAt.25)
sdPowerMedium<-sd(RawData$PowerAt.5)

