#Schoonen (1991) data reanalysis Requires "Schoonen (1991) data NonDirecctional.csv" and "Schoonen (1991) data Direcctional.csv"

# non-directional tests  # Replace file location with appropriate link to "Schoonen (1991) data NonDirectional.csv"
RawData<- read_csv("Schoonen (1991) data Directional.csv")
quantilesPowerValueSmall<-quantile(RawData$PowerAtSmall, c(.25,.5,.75))
quantilesPowerValueMedian<-quantile(RawData$PowerAtMedium, c(.25,.5,.75))
quantilesPowerValueLarge<-quantile(RawData$PowerAtLarge, c(.25,.5,.75))
meanPowerSmall<-mean(RawData$PowerAtSmall)
meanPowerMedium<-mean(RawData$PowerAtMedium)
meanPowerLarge<-mean(RawData$PowerAtLarge)
sdPowerSmall<-sd(RawData$PowerAtSmall)
sdPowerMedium<-sd(RawData$PowerAtMedium)
sdPowerLarge<-sd(RawData$PowerAtLarge)

# directional tests # Replace file location with appropriate link to "Schoonen (1991) data Directional.csv"
RawData<- read_csv("PhD/Systematic Reviews/History of Power Estimation Studies/R files (working, figures, general working doc)/Reperformed data analysis/Schoonen (1991) data Directional.csv")
quantilesPowerValueSmall<-quantile(RawData$PowerAtSmall, c(.25,.5,.75))
quantilesPowerValueMedian<-quantile(RawData$PowerAtMedium, c(.25,.5,.75))
quantilesPowerValueLarge<-quantile(RawData$PowerAtLarge, c(.25,.5,.75))
meanPowerSmall<-mean(RawData$PowerAtSmall)
meanPowerMedium<-mean(RawData$PowerAtMedium)
meanPowerLarge<-mean(RawData$PowerAtLarge)
sdPowerSmall<-sd(RawData$PowerAtSmall)
sdPowerMedium<-sd(RawData$PowerAtMedium)
sdPowerLarge<-sd(RawData$PowerAtLarge)