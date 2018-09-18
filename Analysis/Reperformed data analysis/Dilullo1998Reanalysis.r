# Reading in sample sizes and power values from Dilullo, 1998, pgs 120 - 122.
sampleSizes <- c(81,4091,3613,1009,288,150,71,290,95,92,38,36,58,418,109,31,227,358,54,52,121,170,67,111,60,73,283,294,372,564,250,132,114,31,32,183,57,235,48,88,95)
powerValuesSmall<-c(.1,1,1,.89,.40,.18,.12,.40,.21,.16,.09,.09,.12,.53,.18,.08,.32,.47,.11,.11,.19,.26,.13,.18,.12,.13,.4,.4,.49,.66,.35,.21,.18,.08,.10,.4,.16,.48,.14,.25,.28)
powerValuesMedium<-c(.45,1,1,1,.99,.78,.54,1,.90,.83,.46,.44,.64,1,.89,.38,1,1,.61,.69,.92,.98,.70,.89,.65,.74,1,1,1,1,1,.94,.90,.38,.48,.99,.74,1,.66,.94,.96)

dilullo<-data.frame(sampleSizes,powerValuesSmall,powerValuesMedium)

# finding interquartile ranges and quantiles for these values
quantilesSampleSize<-quantile(sampleSizes , c(.25,.5,.75))
quantilesPowerValuesmall<-quantile(powerValuesSmall, c(.25,.5,.75))
quantilesPowerValueMedian<-quantile(powerValuesMedium, c(.25,.5,.75))