# estimating means and sds 
data <- read_excel("~/PhD/Systematic Reviews/History of Power Estimation Studies/PowerEstimationReviewDataCollection2018.03.25.xlsx")
# Counting
# number of articles included = 50 (i.e., not discounting for missing data) 
length(unique(subset(data$id, is.na(data$exclude) == TRUE))) 
# number of datapoints = 64
sum(is.na(data$exclude))
# removing all the exlusions
data.set <- subset(data, is.na(data$exclude))

# checked articles extracted with dat$id[which(is.na(dat$varMedium) & is.na(dat$IQRMedium))]
# these were all checked for frequency tables
articlesChecked <- c(96, 99, 73, 92, 59, 62, 42, 89, 112, 113, 121, 115, 123, 119, 122, 124, 131)

## All of the following means and SDs were caluclated using the following after loading in the 
# lb (lower bound from the frequency table), ub (upper bounds), ns between lb and ub, and powers -
# The results were then written into the excel file directly ~  in the SDSmallAlgEstFromCDT, SDMediumAlgEstFromCDT, & SDLargeAlgEstFromCDT cols
#  nsS<- small # etc. 
#  nS <-sum(ns)
#  estMeanS <- sum(powers * ns) / n
#  estVarS <-  (sum(ns * powers^2)/n) - estMean^2
#  estSDS <- sqrt(estVar)
#  estSDS
#  
#  nsM<- medium # etc. 
#  nM <-sum(ns)
#  estMeanM <- sum(powers * ns) / n
#  estVarM <-  (sum(ns * powers^2)/n) - estMean^2
#  estSDM <- sqrt(estVar)
#  estSDM
#  
#  nsL<- large # etc. 
#  nL <-sum(ns)
#  estMeanL <- sum(powers * ns) / n
#  estVarL <-  (sum(ns * powers^2)/n) - estMean^2
#  estSDL <- sqrt(estVar)
#  
# cASHEN, gEIGER 2004 # only small avaliable 
lb <- c(.99, .95, .9, .85, .8, .7, .6, .5, .4, .3, .2, .1, .05)
ub <-  c(1, .98, .94, .89, .84, .79, .69, .59, .49, .39, .29, .19, .09)
powers <- rowMeans(data.frame(lb, ub))
ns <- c(12, 1, 1, 1, 0, 0, 1, 1, 2, 10, 10, 29, 9)
# reported mean = .29 
# est mean = 0.3251309

# tracking means differences 
meanDiffs <- data.frame(NA, NA, NA)
names(meanDiffs) <- c("sd","md", "ld")
meanDiffs[1,1] <- .29 - 0.3251309

# Mazen  Hemmasi & Lewis 1987
# Table 2 
lb <- c(.99, .95, .9, .80, .7, .6, .5, .4, .3, .2, .1, .04)
ub <-  c(1, .98, .94, .89, .79, .69, .59, .49, .39, .29, .19, .09)
powers <-  rowMeans(data.frame(lb, ub))
small <- c(0, 0, 2, 1, 1, 1, 0, 2, 2, 5, 16, 14)
medium <- c(6, 2, 2, 3, 4, 4, 6, 4, 7, 3, 3, 0)
large <- c(12, 6, 5, 6, 7, 3, 2, 1, 0, 1, 1, 0) 
# mean diffs:
pos <- str_which(data.set$Author, "Mazen, A. M. M., Hemmasi, M. and Lewis, M. F")
reportedMeans <- c(data.set$PowerAtSmallEffectMean[pos], data.set$PowerAtMediumEffectMean[pos], data.set$PowerAtLargeEffectMean[pos]) 
estimatedMeans <- c(0.2328015, 0.6015909, 0.8289773)
meanDiffs[2,] <- reportedMeans - estimatedMeans

# Mone, M. A., Mueller, G. C. and Mauland, W. 1996, table 1
lb <- c(.99, .95, .9, .80, .7, .6, .5, .4, .3, .2, .1, .05)
ub <-  c(1, .98, .94, .89, .79, .69, .59, .49, .39, .29, .19, .09)
powers <-  rowMeans(data.frame(lb, ub))
small <- c(9, 3, 1, 5, 1, 6, 7, 9, 15, 43, 75, 36)
medium <- c(49, 15, 21, 24, 20, 19, 21, 18, 8, 11, 3, 1)
large <- c(124, 22, 17, 21, 8, 10, 4, 1, 2, 0, 1, 0)
pos <- str_which(data.set$Author, "Mone, M. A., Mueller, G. C. and Mauland, W.")
reportedMeans <- c(data.set$PowerAtSmallEffectMean[pos], data.set$PowerAtMediumEffectMean[pos], data.set$PowerAtLargeEffectMean[pos]) 
estimatedMeans <- c(0.2787381, 0.74, 0.9231667)
meanDiffs[3,] <- reportedMeans - estimatedMeans

# Haase 1974 # table 1
lb <- c(.99, .95, .9, .80, .7, .6, .5, .4, .3, .2, .1, .05)
ub <-  c(1, .98, .94, .89, .79, .69, .59, .49, .39, .29, .19, .09)
powers <-  rowMeans(data.frame(lb, ub))
small <- c(2, 1, 1, 1, 1, 2, 6, 1, 1, 6, 16, 30) 
medium <- c(4, 8, 5, 4, 3, 7, 7, 5, 5, 12, 12, 0) 
large <- c(13, 0, 0, 4, 4, 3, 0, 5, 9, 2, 0, 0)
# no reported means
# estimatedMeans <- c(0.2456618, 0.5399306, 0.67625)

# Woolley, Thomas W.	A comprehensive power-analytic investigation of research in medical education 
# table 2
lb <- c(.99, .95, .9, .80, .7, .6, .5, .4, .3, .2, .1, .0)
ub <-  c(1, .98, .94, .89, .79, .69, .59, .49, .39, .29, .19, .09)
powers <-  rowMeans(data.frame(lb, ub))
small <- c(1, 2, 0, 1, 0, 2, 3, 5, 7, 13, 47, 19) 
medium <- c(17, 7, 9, 11, 7, 11, 13, 8, 8, 6, 3, 0)
large <- c(50, 11, 8, 14, 6, 2, 5, 2, 1, 1, 0, 0)
pos <- str_which(data.set$Author, "Woolley, Thomas W\\.$")
estimatedMeans <- c(0.2219, 0.68865, 0.8952)
reportedMeans <- c(data.set$PowerAtSmallEffectMean[pos], data.set$PowerAtMediumEffectMean[pos], data.set$PowerAtLargeEffectMean[pos]) 
meanDiffs[4,] <- reportedMeans - estimatedMeans

## Borkowski, Susan C. Mary Jeanne Welsh and Zhang, Qinke	An Analysis of Statistical Power in Behavioral Accounting Research. Table 3
lb <- c(.95, .9, .8, .7, .6, .5, .4, .3, .2, .1, .0)
ub <-  c(.99, .94, .89, .79, .69, .59, .49, .39, .29, .19, .09)
powers <-  rowMeans(data.frame(lb, ub))
small <- c(0, 0, 1, 2, 1, 0, 4, 5, 20, 50, 13) 
medium <- c(10, 11, 13, 8, 12, 16, 12, 9, 5, 0, 0) 
large <- c(51, 12, 14, 11, 1, 6, 0, 1, 0, 0, 0)
pos <- str_which(data.set$Author, "Borkowski, Susan C. Mary Jeanne Welsh and Zhang")
reportedMeans <- c(data.set$PowerAtSmallEffectMean[pos], data.set$PowerAtMediumEffectMean[pos], data.set$PowerAtLargeEffectMean[pos]) 
estimatedMeans <- c(0.2002083, 0.6551562,  0.8832813)
meanDiffs[5,] <- reportedMeans - estimatedMeans

# Woolley, Thomas W. and Dawson, George O.	A Follow-up Power Analysis of the Statistical Tests Used in the Journal of Research in Science Teaching. Table III
lb <- c(.99, .95, .9, .80, .7, .6, .5, .4, .3, .2, .1, .0)
ub <-  c(1, .98, .94, .89, .79, .69, .59, .49, .39, .29, .19, .09)
powers <-  rowMeans(data.frame(lb, ub))
small <- c(1, 3, 2, 2, 2, 5, 8, 9, 8, 29, 79, 44) 
medium <- c(29, 8, 8, 22, 21, 13, 23, 24, 18, 13, 11, 2) 
large <- c(65, 30, 21, 26, 15, 14, 7, 5, 4, 4, 1, 0)
pos <- str_which(data.set$Author, "Woolley, Thomas W\\. and Dawson")
reportedMeans <- c(data.set$PowerAtSmallEffectMean[pos], data.set$PowerAtMediumEffectMean[pos], data.set$PowerAtLargeEffectMean[pos]) 
estimatedMeans <- c(0.2129196, 0.6294271,  0.8524219)
meanDiffs[6,] <- reportedMeans - estimatedMeans

# Overland, C. T.	Statistical Power in the Journal of Research in Music Education (2000-2010): A Retrospective Power Analysis	Bulletin of the Council for Research in Music Education	2014
# table 2 
lb <- c(.95, .9, .80, .7, .6, .5, .4, .3, .2, .1, .00)
ub <-  c(.98, .94, .89, .79, .69, .59, .49, .39, .29, .19, .09)
powers <-  rowMeans(data.frame(lb, ub))
small <- c(4, 0, 2, 0, 4, 4, 6, 16, 17, 37, 35) 
medium <- c(29, 3, 14, 16, 8, 12, 11, 9, 8, 11, 3) 
large <- c(70, 9, 11, 12, 5, 4, 3, 2, 2, 5, 2)
reportedMeans <- c(.24, .64, .84)
estimatedMeans <- c(0.23684, 0.6281048,  0.8224)
meanDiffs[7,] <- reportedMeans - estimatedMeans

# Mazen, A. M. M., Hemmasi, M. and Lewis, M. F	Assessment of statistical power in contemporary strategy research
# table 2
lb <- c(.99, .95, .9, .80, .7, .6, .5, .4, .3, .2, .1, .04)
ub <-  c(1, .98, .94, .89, .79, .69, .59, .49, .39, .29, .19, .09)
powers <-  rowMeans(data.frame(lb, ub))
small <- c(0, 0, 2, 1, 1, 1, 0, 2, 2, 5, 16, 14) 
medium <- c(6, 2, 2, 3, 4, 4, 6, 4, 7, 3, 3, 0)
large <- c(12, 6, 5, 6, 7, 3, 2, 1, 0, 1, 1, 0)
reportedMeans <- c(.23,.59 ,.83)
estimatedMeans <- c(0.2297727, 0.6015909,  0.8289773)

meanDiffs[8,] <- reportedMeans - estimatedMeans

meanAbsDiff <- mean(abs(as.matrix(meanDiffs)), na.rm = T)
