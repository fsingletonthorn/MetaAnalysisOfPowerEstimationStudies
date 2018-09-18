#load Sindelar(1988)ReperformedDataAnalysis.csv
# Replace "Sindelar(1988)ReperformedDataAnalysis.csv" below with appropriate path
RawData <- read_csv("Sindelar(1988)ReperformedDataAnalysis.csv")
# Finding the mean of small medium large aca and soc columns when there are two
#
meanPower<- as.matrix(RawData[1:35,1:3])
as.matrix(meanPower)
meanPower[is.na(meanPower)]<-as.matrix(RawData[1:35,4:6])[is.na(meanPower)]
# fixing the one place where different values are given for Academic and Social power estimates - (simple mean average of RawData[18,1] and RawData [18,4], RawData[18,2] and RawData [18,5], and RawData[18,3] and RawData [18,6].   
meanPower[18,1:3]<- c(18.5, 43,77.5)

#making into dataframe and naming columns such that the below will work
meanPower<-as.data.frame(meanPower)
names(meanPower)<-c("PowerAtSmall","PowerAtMedium","PowerAtLarge")

# calculating values 
quantilesPowerValueSmall<-quantile(meanPower$PowerAtSmall, c(0,.25,.5,.75,1))
quantilesPowerValueMedian<-quantile(meanPower$PowerAtMedium, c(0,.25,.5,.75,1))
quantilesPowerValueLarge<-quantile(meanPower$PowerAtLarge, c(0,.25,.5,.75,1))
meanPowerSmall<-mean(meanPower$PowerAtSmall)
meanPowerMedium<-mean(meanPower$PowerAtMedium)
meanPowerLarge<-mean(meanPower$PowerAtLarge)
sdPowerSmall<-sd(meanPower$PowerAtSmall)
sdPowerMedium<-sd(meanPower$PowerAtMedium)
sdPowerLarge<-sd(meanPower$PowerAtLarge)

# importing sample size data - replace "Sindelar(1988)SampleSizes.csv" with appropraite path to file
RawNData <- read_csv("Sindelar(1988)SampleSizes.csv")
RawNData$SampleSize<- RawNData$numberOfGroups*RawNData$SampleSizePerGroup

quantilesN<-quantile(RawNData$SampleSize, c(.25,.5,.75))
meanN<-mean(RawNData$SampleSize)
sdN<-sd(RawNData$SampleSize)