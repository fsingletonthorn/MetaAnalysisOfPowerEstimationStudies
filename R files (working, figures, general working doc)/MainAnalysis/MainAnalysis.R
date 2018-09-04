# packages 
library(readxl)
library(metafor)
library(stringr)

########### Setting up functions for data imputation ########
# the following estimators are from Wan, X., Wang, W., Liu, J., & Tong, T. (2014). Estimating the sample mean and standard deviation from the sample size, median, range and/or interquartile range. BMC Medical Research Methodology, 14(1), 135. doi:10.1186/1471-2288-14-135
# But the functions are from the package veramata, See Charles Grey's Dissertation for the completed package, or https://github.com/softloud/varameta before she finishes it. 
# parameters in the following functions: 
#   a Minimum value of sample.
##  m Median of sample.
##  b Maximum value of sample.
##  n Sample size.
##  q1 first quartile
##  q3 third quartile


eff_est_wan_c2 <- function(a, q.1, m, q.3, b, n) {
  x.bar <- (a + 2 * q.1 + 2 * m + 2 * q.3 + b) / 8
  s <- 
    (b - a) / (4 * qnorm(
      (n - 0.375) / (n + 0.25)
    )) +
    (q.3 - q.1) / (4 * qnorm(
      (0.75 * n - 0.125) / (n + 0.25)
    ))
  return(list(
    centre = x.bar,
    se = s / sqrt(n)
  ))
}


eff_est_wan_c3 <- function(q.1, m, q.3, n) {
  x.bar <- (q.1 + m + q.3) / 3
  s <- (q.3 - q.1) / (2 * qnorm(
    (0.75 * n - 0.125) / (n + 0.25)
  ))
  return(list(
    centre = x.bar,
    se = s / sqrt(n)
  ))
}

######### data importing and wrangling ###############

# importing data
data <- read_excel("~/PhD/Systematic Reviews/History of Power Estimation Studies/PowerEstimationReviewDataCollection2018.03.25.xlsx")

# Counting
# number of articles included (i.e., not discounting for missing data) 
N <- length(unique(subset(data$id, is.na(data$exclude) == TRUE))) 
# number of datapoints (i.e., non exlcuded papers)
n <- sum(is.na(data$exclude))

# removing all the exlusions
dat <- subset(data, is.na(data$exclude))

# converting to numerics
dat$NumberOfArticles<-as.numeric(dat$NumberOfArticles)

# figuring mean year of coverage for use in model later
singleYears <- as.numeric(dat$YearsStudied)
# splitting multiple years at "-"
years <- str_split(dat$YearsStudied, "-", simplify = T)
# converting to numerics
years <- apply(years,  2, as.numeric)
# calculating mean year of coverage 
dat$YearsStudiedMean <- rowMeans(years, na.rm = T)

# Ordering by year of coverage, 
dat<-dat[order(dat$YearsStudiedMean),]

# Adding whitespace at the end of columns to avoid issues that having identical study names causes later
for(i in unique(dat$StudyName)) {
  dat$StudyName[dat$StudyName == i] <- paste0(dat$StudyName[dat$StudyName == i], c("", " ", "  ", "    ", "     ", "       ")[1:length(dat$StudyName[dat$StudyName == i])])
}
    
    # calculating variance and IQR 
dat$varMedium <- dat$SDPowerAtMedium^2  
dat$IQRMedium <- dat$ThirdQuartilePowerAtMedium - dat$FirstQuartilePowerAtMedium

# number missing means and SDs but providing SDs quartiles and medians
sum(!is.na(dat$PowerAtSmallEffectMedian) & is.na(dat$SDPowerAtMedium) & is.na(dat$SDMedAlgEstFromCDT) & !is.na(dat$PowerAtMediumEffectMedian) & !is.na(dat$FirstQuartilePowerAtMedium) & !is.na(dat$ThirdQuartilePowerAtMedium))
# n articles  missing means and SDs but providing SDs quartiles and medians
length(unique(dat$id[(!is.na(dat$PowerAtSmallEffectMedian) & is.na(dat$SDPowerAtMedium) & is.na(dat$SDMedAlgEstFromCDT) & !is.na(dat$PowerAtMediumEffectMedian) & !is.na(dat$FirstQuartilePowerAtMedium) & !is.na(dat$ThirdQuartilePowerAtMedium))]))
# N articles no variances or quartiles 
length(unique(dat$id[(is.na(dat$SDPowerAtMedium) & is.na(dat$SDMedAlgEstFromCDT) & !is.na(dat$PowerAtMediumEffectMedian) & is.na(dat$FirstQuartilePowerAtMedium) & is.na(dat$ThirdQuartilePowerAtMedium))]))

## putting estimated Ms and SDs into the appropraite places 
# 
dat[c("PowerAtSmallEffectMean", "PowerAtMediumEffectMean", "PowerAtLargeEffectMean")][dat$id == 62,] <- c(0.2456618, 0.5399306, 0.67625)

#### Estimating missing parameters ####
###### Estimating Small var and sd
## setting up col for storring means + estiamted variance 
dat$estSmallMean <- NA
dat$varSmall <- NA

# calculating wan c1 estimated mean and SEs following wan et al 
dat[c('wanc2EstMean', 'wanc2EstSE')] <- eff_est_wan_c2(a = dat$PowerSmallMin, b = dat$PowerSmallMax, q.1 =  dat$FirstQuartilePowerAtSmall, m = dat$PowerAtSmallEffectMedian, 
                                                       q.3 =  dat$ThirdQuartilePowerAtSmall, n = dat$NumberOfArticles)

# calculating wan c3 estimated mean and SEs following wan et al 
dat[c('wanc3EstMean', 'wanc3EstSE')] <- eff_est_wan_c3(q.1 =  dat$FirstQuartilePowerAtSmall, m = dat$PowerAtSmallEffectMedian, 
                                                                    q.3 =  dat$ThirdQuartilePowerAtSmall, n = dat$NumberOfArticles)

# Building mean column, in order of preferences for the method that used the most inforamtion 
# I.e., authors reported, Wan et al method c2, Wan et al, method c3
dat$estSmallMean <- dat$wanc2EstMean 
dat$estSmallMean[is.na(dat$estSmallMean)] <- dat$wanc3EstMean[is.na(dat$estSmallMean)]
# calculating the mean absolute error for Wan's methods the ? articles for which this is possible (i.e., where these values can be calculated and the authors reported the mean)
absDiffSmall <- abs(dat$estSmallMean - dat$PowerAtSmallEffectMean)
# finishing off by using all of the reported means where possible
dat$estSmallMean[!is.na(dat$PowerAtSmallEffectMean)] <- dat$PowerAtSmallEffectMean[!is.na(dat$PowerAtSmallEffectMean)]


# Estimating variance column, in order of preferences for the method that has the most inforamtion 
# I.e., authors SD, Wan et al method c2, Wan et al, method c3,
dat$varSmall <- (dat$wanc2EstSE * sqrt(as.numeric(dat$NumberOfArticles)))^2
dat$varSmall[is.na(dat$varSmall)] <- ((dat$wanc3EstSE * sqrt(as.numeric(dat$NumberOfArticles)))^2)[is.na(dat$varSmall)] 
# calculating the mean absolute error for Wan's methods the ? articles for which this is possible (i.e., where these values can be calculated and the authors reported the sd)
absDiffVarSmall <- abs(dat$varSmall- dat$SDPowerAtSmall^2)
# overridding with vars calculated from quartiles 
dat$varSmall[!is.na(dat$SDSmallAlgEstFromCDT)] <- dat$SDSmallAlgEstFromCDT[!is.na(dat$SDSmallAlgEstFromCDT)]
# overridding with vars reported by authors  
dat$varSmall[!is.na(dat$SDPowerAtSmall)] <- dat$SDPowerAtSmall[!is.na(dat$SDPowerAtSmall)]^2


###### Estimating medium var and sd
## setting up col for storring means + estiamted variance 
dat$estMedMean <- NA
dat$varMed <- NA

# calculating wan c1 estimated mean and SEs following wan et al 
dat[c('wanc2EstMean', 'wanc2EstSE')] <- eff_est_wan_c2(a = dat$PowerMedMin, b = dat$PowerMedMax, q.1 =  dat$FirstQuartilePowerAtMedium, m = dat$PowerAtMediumEffectMedian, 
                                                       q.3 =  dat$ThirdQuartilePowerAtMedium, n = dat$NumberOfArticles)

# calculating wan c3 estimated mean and SEs following wan et al 
dat[c('wanc3EstMean', 'wanc3EstSE')] <- eff_est_wan_c3(q.1 =  dat$FirstQuartilePowerAtMedium, m = dat$PowerAtMediumEffectMedian, 
                                                       q.3 =  dat$ThirdQuartilePowerAtMedium, n = dat$NumberOfArticles)

# Building mean column, in order of preferences for the method that used the most inforamtion 
# I.e., authors reported, Wan et al method c2, Wan et al, method c3
dat$estMedMean <- dat$wanc2EstMean 
dat$estMedMean[is.na(dat$estMedMean)] <- dat$wanc3EstMean[is.na(dat$estMedMean)]
# calculating the mean absolute error for Wan's methods the ? articles for which this is possible (i.e., where these values can be calculated and the authors reported the mean)
absDiffMed <- abs(dat$estMedMean - dat$PowerAtMediumEffectMean)
# finishing off by using all of the reported means where possible
dat$estMedMean[!is.na(dat$PowerAtMediumEffectMean)] <- dat$PowerAtMediumEffectMean[!is.na(dat$PowerAtMediumEffectMean)]


# Estimating variance column, in order of preferences for the method that has the most inforamtion 
# I.e., authors SD, Wan et al method c2, Wan et al, method c3,
dat$varMed <- (dat$wanc2EstSE * sqrt(as.numeric(dat$NumberOfArticles)))^2
dat$varMed[is.na(dat$varMed)] <- ((dat$wanc3EstSE * sqrt(as.numeric(dat$NumberOfArticles)))^2)[is.na(dat$varMed)] 
# calculating the mean absolute error for Wan's methods the ? articles for which this is possible (i.e., where these values can be calculated and the authors reported the sd)
absDiffVarMed <- mean(abs(dat$varMed- dat$SDPowerAtMedium^2), na.rm = T)
# overridding with vars calculated from quartiles 
dat$varMed[!is.na(dat$SDMedAlgEstFromCDT)] <- dat$SDMedAlgEstFromCDT[!is.na(dat$SDMedAlgEstFromCDT)]
# overridding with vars reported by authors  
dat$varMed[!is.na(dat$SDPowerAtMedium)] <- dat$SDPowerAtMedium[!is.na(dat$SDPowerAtMedium)]^2

###### Estimating Large var and sd
## setting up col for storring means + estiamted variance 
dat$estLargeMean <- NA
dat$varLarge <- NA

# calculating wan c1 estimated mean and SEs following wan et al 
dat[c('wanc2EstMean', 'wanc2EstSE')] <- eff_est_wan_c2(a = dat$PowerLargeMin, b = dat$PowerLargeMax, q.1 =  dat$FirstQuartilePowerAtLarge, m = dat$PowerAtLargeEffectMedian, 
                                                       q.3 =  dat$ThirdQuartilePowerAtLarge, n = dat$NumberOfArticles)

# calculating wan c3 estimated mean and SEs following wan et al 
dat[c('wanc3EstMean', 'wanc3EstSE')] <- eff_est_wan_c3(q.1 =  dat$FirstQuartilePowerAtLarge, m = dat$PowerAtLargeEffectMedian, 
                                                       q.3 =  dat$ThirdQuartilePowerAtLarge, n = dat$NumberOfArticles)

# Building mean column, in order of preferences for the method that used the most inforamtion 
# I.e., authors reported, Wan et al method c2, Wan et al, method c3
dat$estLargeMean <- dat$wanc2EstMean 
dat$estLargeMean[is.na(dat$estLargeMean)] <- dat$wanc3EstMean[is.na(dat$estLargeMean)]
# calculating the mean absolute error for Wan's methods the ? articles for which this is possible (i.e., where these values can be calculated and the authors reported the mean)
absDiffLarge <- abs(dat$estLargeMean - dat$PowerAtLargeEffectMean)
# finishing off by using all of the reported means where possible
dat$estLargeMean[!is.na(dat$PowerAtLargeEffectMean)] <- dat$PowerAtLargeEffectMean[!is.na(dat$PowerAtLargeEffectMean)]

# Estimating variance column, in order of preferences for the method that has the most inforamtion 
# I.e., authors SD, Wan et al method c2, Wan et al, method c3,
dat$varLarge <- (dat$wanc2EstSE * sqrt(as.numeric(dat$NumberOfArticles)))^2
dat$varLarge[is.na(dat$varLarge)] <- ((dat$wanc3EstSE * sqrt(as.numeric(dat$NumberOfArticles)))^2)[is.na(dat$varLarge)] 
# calculating the mean absolute error for Wan's methods the ? articles for which this is possible (i.e., where these values can be calculated and the authors reported the sd)
absDiffVarLarge <- abs(dat$varLarge- dat$SDPowerAtLarge^2)
# overridding with vars calculated from quartiles 
dat$varLarge[!is.na(dat$SDLargeAlgEstFromCDT)] <- dat$SDLargeAlgEstFromCDT[!is.na(dat$SDLargeAlgEstFromCDT)]
# overridding with vars reported by authors  
dat$varLarge[!is.na(dat$SDPowerAtLarge)] <- dat$SDPowerAtLarge[!is.na(dat$SDPowerAtLarge)]^2
dat$varLarge[dat$varLarge == 0] <- NA

##### Calculating mean absolute diffs between estimated values and those provided in the paper 
meanAbsDiffVariance <- mean(c(absDiffVarLarge, absDiffVarMed, absDiffVarSmall) , na.rm = T)
mean(c(absDiffVarMed) , na.rm = T)
meanAbsDiffMean <- mean(c(absDiffLarge, absDiffMed, absDiffSmall), na.rm = T)

######## Data imputation #########
#### medium
replacementVis <- mean(dat$varMed, na.rm = T)
dat$varMed_MeanImpute <- dat$varMed
dat$varMed_MeanImpute[is.na(dat$varMed)] <- replacementVis
# Median imputation
replacementVis <- median(dat$varMed, na.rm = T)
dat$varMed_MedianImpute <- dat$varMed
dat$varMed_MedianImpute[is.na(dat$varMed)] <- replacementVis
# Min imputation
replacementVis <- min(dat$varMed, na.rm = T)
dat$varMed_MinImpute <- dat$varMed
dat$varMed_MinImpute[is.na(dat$varMed)] <- replacementVis
# Max imputation
replacementVis <- max(dat$varMed, na.rm = T)
dat$varMed_MaxImpute <- dat$varMed
dat$varMed_MaxImpute[is.na(dat$varMed)] <- replacementVis

##### data imputation small
replacementVis <- mean(dat$varSmall, na.rm = T)
dat$varSmall_MeanImpute <- dat$varSmall
dat$varSmall_MeanImpute[is.na(dat$varSmall)] <- replacementVis
# Median imputation
replacementVis <- median(dat$varSmall, na.rm = T)
dat$varSmall_MedianImpute <- dat$varSmall
dat$varSmall_MedianImpute[is.na(dat$varSmall)] <- replacementVis
# Min imputation
replacementVis <- min(dat$varSmall, na.rm = T)
dat$varSmall_MinImpute <- dat$varSmall
dat$varSmall_MinImpute[is.na(dat$varSmall)] <- replacementVis
# Max imputation
replacementVis <- max(dat$varSmall, na.rm = T)
dat$varSmall_MaxImpute <- dat$varSmall
dat$varSmall_MaxImpute[is.na(dat$varSmall)] <- replacementVis

#### Data imputation Large 
replacementVis <- mean(dat$varLarge, na.rm = T)
dat$varLarge_MeanImpute <- dat$varLarge
dat$varLarge_MeanImpute[is.na(dat$varLarge)] <- replacementVis
# Median imputation
replacementVis <- median(dat$varLarge, na.rm = T)
dat$varLarge_MedianImpute <- dat$varLarge
dat$varLarge_MedianImpute[is.na(dat$varLarge)] <- replacementVis
# Min imputation
replacementVis <- min(dat$varLarge, na.rm = T)
dat$varLarge_MinImpute <- dat$varLarge
dat$varLarge_MinImpute[is.na(dat$varLarge)] <- replacementVis
# Max imputation
replacementVis <- max(dat$varLarge, na.rm = T)
dat$varLarge_MaxImpute <- dat$varLarge
dat$varLarge_MaxImpute[is.na(dat$varLarge)] <- replacementVis


#### Calculating sampling variances ####
# Calculating the sampling variances medium with all data imputation methods
dat$samplingVarMed_Mean <- dat$varMed_MeanImpute/dat$NumberOfArticles
dat$samplingVarMed_Med <- dat$varMed_MedianImpute/dat$NumberOfArticles
dat$samplingVarMed_Min <- dat$varMed_MinImpute/dat$NumberOfArticles
dat$samplingVarMed_Max <- dat$varMed_MaxImpute/dat$NumberOfArticles

# Calculating the sampling variances small
dat$samplingVarSmall_Mean <- dat$varSmall_MeanImpute/dat$NumberOfArticles
dat$samplingVarSmall_Med <- dat$varSmall_MedianImpute/dat$NumberOfArticles
dat$samplingVarSmall_Min <- dat$varSmall_MinImpute/dat$NumberOfArticles
dat$samplingVarSmall_Max <- dat$varSmall_MaxImpute/dat$NumberOfArticles

# Calculating the sampling variances large
dat$samplingVarLarge_Mean <- dat$varLarge_MeanImpute/dat$NumberOfArticles
dat$samplingVarLarge_Med <- dat$varLarge_MedianImpute/dat$NumberOfArticles
dat$samplingVarLarge_Min <- dat$varLarge_MinImpute/dat$NumberOfArticles
dat$samplingVarLarge_Max <- dat$varLarge_MaxImpute/dat$NumberOfArticles

# same without imputed data
dat$samplingVarSmall_NoImputedData <- dat$SDPowerAtSmall^2/dat$NumberOfArticles
dat$samplingVarMed_NoImputedData <- dat$SDPowerAtMedium^2/dat$NumberOfArticles
dat$samplingVarLarge_NoImputedData <- dat$SDPowerAtLarge^2/dat$NumberOfArticles

#### Analysis ####
## Running models without any imputed data or any estimated data
#### Most basic model #### Small effect size
resSmallNoImp <- rma(yi = estSmallMean, vi = samplingVarSmall_NoImputedData,  data = dat, method = "REML")
######## THIS MODEL ACCOUNTS FOR GROUPING by paper, no moderators #####  
resSmallNoImpML <- rma.mv(yi = estSmallMean, V = dat$samplingVarSmall_NoImputedData, random = ~ 1 | id,  data = dat)
######## THIS MODEL ACCOUNTS FOR GROUPING by paper and years covered  ## , 
resSmallNoImpMLYear <- rma.mv(yi = estSmallMean, V = samplingVarSmall_NoImputedData, random = ~ 1 | id, mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)

# Medium 
#### Most basic model ####
resMedNoImp <- rma(yi = estMedMean, vi = samplingVarMed_NoImputedData,  data = dat, method = "REML")
######## THIS MODEL ACCOUNTS FOR GROUPING by paper, no moderators #####  
resMedNoImpML <- rma.mv(yi = estMedMean, V = dat$samplingVarMed_NoImputedData, random = ~ 1 | id,  data = dat)
######## THIS MODEL ACCOUNTS FOR GROUPING by paper and years covered  ## , 
resMedNoImpMLYear <- rma.mv(yi = estMedMean, V = samplingVarMed_NoImputedData, random = ~ 1 | id, mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)

# Large
# No ML
resLargeNoImp <- rma(yi = estLargeMean, vi = samplingVarLarge_NoImputedData,  data = dat, method = "REML")
######## THIS MODEL ACCOUNTS FOR GROUPING by paper, no moderators #####  
resLargeNoImpML <- rma.mv(yi = estLargeMean, V = dat$samplingVarLarge_NoImputedData, random = ~ 1 | id,  data = dat)
######## THIS MODEL ACCOUNTS FOR GROUPING by paper and years covered  ## , 
resLargeNoImpMLYear <- rma.mv(yi = estLargeMean, V = samplingVarLarge_NoImputedData, random = ~ 1 | id, mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)



# Running analyses with imputed data, this time with mean imputation. These are the main results in the paper. 
####### MEDIUM EFFECT SIZE BENCHMARK 
# Models with means 
#### Most basic model ####
resMedMean <- rma(yi = estMedMean, vi = samplingVarMed_Mean,  data = dat, method = "REML", slab=StudyName)
######## THIS MODEL ACCOUNTS FOR GROUPING by paper, no moderators #####  
resMedMeanML <- rma.mv(yi = estMedMean, V = dat$samplingVarMed_Mean, random = ~ 1 | id,  data = dat, slab=StudyName)
######## THIS MODEL ACCOUNTS FOR GROUPING by paper and years covered  ## , 
resMedMeanMLYear <- rma.mv(yi = estMedMean, V = samplingVarMed_Mean, random = ~ 1 | id, mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)

## same models with median imputation
#### Most basic model ####
resMedMed <- rma(yi = estMedMean, vi = samplingVarMed_Med,  data = dat, method = "REML")
######## THIS MODEL ACCOUNTS FOR GROUPING by paper, no moderators #####  
resMedMedML <- rma.mv(yi = estMedMean, V = dat$samplingVarMed_Med, random = ~ 1 | id,  data = dat)
######## THIS MODEL ACCOUNTS FOR GROUPING by paper and years covered  ## , 
resMedMedMLYear <- rma.mv(yi = estMedMean, V = samplingVarMed_Med, random = ~ 1 | id, mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)

## same models with min imputation 
#### Most basic model ####
resMedMin <- rma(yi = estMedMean, vi = samplingVarMed_Min,  data = dat, method = "REML")
######## THIS MODEL ACCOUNTS FOR GROUPING by paper, no moderators #####  
resMedMinML <- rma.mv(yi = estMedMean, V = dat$samplingVarMed_Min, random = ~ 1 | id,  data = dat)
######## THIS MODEL ACCOUNTS FOR GROUPING by paper and years covered  ## , 
resMedMinMLYear <- rma.mv(yi = estMedMean, V = samplingVarMed_Min, random = ~ 1 | id, mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)

## same models with max imputation 
#### Most basic model ####
resMedMax <- rma(yi = estMedMean, vi = samplingVarMed_Max,  data = dat, method = "REML")
######## THIS MODEL ACCOUNTS FOR GROUPING by paper, no moderators #####  
resMedMaxML <- rma.mv(yi = estMedMean, V = dat$samplingVarMed_Max, random = ~ 1 | id,  data = dat)
######## THIS MODEL ACCOUNTS FOR GROUPING by paper and years covered  ## , 
resMedMaxMLYear <- rma.mv(yi = estMedMean, V = samplingVarMed_Max, random = ~ 1 | id, mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)




#### SMALL EFFECT SIZE BENCHMARK
# Models with means 
#### Most basic model ####
resSmallMean <- rma(yi = estSmallMean, vi = samplingVarSmall_Mean,  data = dat, method = "REML", slab=StudyName)
######## THIS MODEL ACCOUNTS FOR GROUPING by paper, no moderators #####  
resSmallMeanML <- rma.mv(yi = estSmallMean, V = dat$samplingVarSmall_Mean, random = ~ 1 | id,  data = dat, slab=StudyName)
######## THIS MODEL ACCOUNTS FOR GROUPING by paper and years covered  ## , 
resSmallMeanMLYear <- rma.mv(yi = estSmallMean, V = samplingVarSmall_Mean, random = ~ 1 | id, mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat, slab=StudyName)

## same models with median imputation
#### Most basic model ####
resSmallMed <- rma(yi = estSmallMean, vi = samplingVarSmall_Med,  data = dat, method = "REML")
######## THIS MODEL ACCOUNTS FOR GROUPING by paper, no moderators #####  
resSmallMedML <- rma.mv(yi = estSmallMean, V = dat$samplingVarSmall_Med, random = ~ 1 | id,  data = dat)
######## THIS MODEL ACCOUNTS FOR GROUPING by paper and years covered  ## , 
resSmallMedMLYear <- rma.mv(yi = estSmallMean, V = samplingVarSmall_Med, random = ~ 1 | id, mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)

## same models with min imputation 
#### Most basic model ####
resSmallMin <- rma(yi = estSmallMean, vi = samplingVarSmall_Min,  data = dat, method = "REML")
######## THIS MODEL ACCOUNTS FOR GROUPING by paper, no moderators #####  
resSmallMinML <- rma.mv(yi = estSmallMean, V = dat$samplingVarSmall_Min, random = ~ 1 | id,  data = dat)
######## THIS MODEL ACCOUNTS FOR GROUPING by paper and years covered  ## , 
resSmallMinMLYear <- rma.mv(yi = estSmallMean, V = samplingVarSmall_Min, random = ~ 1 | id, mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)

## same models with max imputation 
#### Most basic model ####
resSmallMax <- rma(yi = estSmallMean, vi = samplingVarSmall_Max,  data = dat, method = "REML")
######## THIS MODEL ACCOUNTS FOR GROUPING by paper, no moderators #####  
resSmallMaxML <- rma.mv(yi = estSmallMean, V = dat$samplingVarSmall_Max, random = ~ 1 | id,  data = dat)
######## THIS MODEL ACCOUNTS FOR GROUPING by paper and years covered  ## , 
resSmallMaxMLYear <- rma.mv(yi = estSmallMean, V = samplingVarSmall_Max, random = ~ 1 | id, mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)



#### LARGE EFFECT SIZE BENCHMARK
# Models with means 

dat$samplingVarLarge_Mean
#### Most basic model ####
resLargeMean <- rma(yi = estLargeMean, vi = samplingVarLarge_Mean,  data = dat, method = "REML", slab=StudyName)
######## THIS MODEL ACCOUNTS FOR GROUPING by paper, no moderators #####  
resLargeMeanML <- rma.mv(yi = estLargeMean, V = dat$samplingVarLarge_Mean, random = ~ 1 | id,  data = dat, slab=StudyName)
######## THIS MODEL ACCOUNTS FOR GROUPING by paper and years covered  ## , 
resLargeMeanMLYear <- rma.mv(yi = estLargeMean, V = samplingVarLarge_Mean, random = ~ 1 | id, mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat, slab=StudyName)

## same models with median imputation
#### Most basic model ####
resLargeMed <- rma(yi = estLargeMean, vi = samplingVarLarge_Med,  data = dat, method = "REML")
######## THIS MODEL ACCOUNTS FOR GROUPING by paper, no moderators #####  
resLargeMedML <- rma.mv(yi = estLargeMean, V = dat$samplingVarLarge_Med, random = ~ 1 | id,  data = dat)
######## THIS MODEL ACCOUNTS FOR GROUPING by paper and years covered  ## , 
resLargeMedMLYear <- rma.mv(yi = estLargeMean, V = samplingVarLarge_Med, random = ~ 1 | id, mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)

## same models with min imputation 
#### Most basic model ####
resLargeMin <- rma(yi = estLargeMean, vi = samplingVarLarge_Min,  data = dat, method = "REML")
######## THIS MODEL ACCOUNTS FOR GROUPING by paper, no moderators #####  
resLargeMinML <- rma.mv(yi = estLargeMean, V = dat$samplingVarLarge_Min, random = ~ 1 | id,  data = dat)
######## THIS MODEL ACCOUNTS FOR GROUPING by paper and years covered  ## , 
resLargeMinMLYear <- rma.mv(yi = estLargeMean, V = samplingVarLarge_Min, random = ~ 1 | id, mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)

## same models with max imputation 
#### Most basic model ####
resLargeMax <- rma(yi = estLargeMean, vi = samplingVarLarge_Max,  data = dat, method = "REML")
######## THIS MODEL ACCOUNTS FOR GROUPING by paper, no moderators #####  
resLargeMaxML <- rma.mv(yi = estLargeMean, V = dat$samplingVarLarge_Max, random = ~ 1 | id,  data = dat)
######## THIS MODEL ACCOUNTS FOR GROUPING by paper and years covered  ## , 
resLargeMaxMLYear <- rma.mv(yi = estLargeMean, V = samplingVarLarge_Max, random = ~ 1 | id, mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)

####### Plots #######
forest(resLargeMeanML)
qqnorm(resMedNoImp)
funnel(resLargeMin)


##### PLOTS #### 
# Plot forest plot medium benchmark (mean imputation)
png(filename = "Plots/ForestPlotMedium.png",width = 900, height = 1100, units = "p")
par(mar=c(4,4,1,2), font = 1)
forest(resMedMean, xlim=c(-.8, 1.3), at = c(0,.25, .5, .75, 1),
       ilab = data.frame(dat$YearsStudied),
       ilab.xpos=c(-.12), cex=1.1, ylim=c(-1, length(dat$id)+3),
       xlab="Estimated power", mlab="", addfit = T, showweights = F)

text(-.8, -1, pos=4, cex=.95, bquote(paste("REML Model (Q = ",
                                            .(formatC(resMedMean$QE, digits=2, format="f")), ", df = ", .(resMedMean$k - resMedMean$p),
                                          .(ifelse(resMedMean$QEp < .001, ", p < ", ", p = ")), .(formatC(ifelse(resMedMean$QEp < .001, .001, resMedMean$QEp), digits=3, format="f")), "; ", I^2, " = ", 
                                            .(formatC(resMedMean$I2, digits=1, format="f")), "%)")))
# Bold font 
par(font=2)
### add column headings to the plot
text(-.66, length(dat$id)+2, c("Author(s) (year)"), cex = 1.25)
text(-.12, length(dat$id)+2, c("Years sampled"), cex = 1.25)

text(c(-8.75,-5.25),     27, c("Vaccinated", "Control"))
text(-16,                26, "Author(s) and Year",  pos=4)
text(6,                  26, "Risk Ratio [95% CI]", pos=2)
# normal font 
par(font=1)
dev.off()




# Plot forest plot small benchmark (mean sd imputation)
res <- resSmallMean
png(filename = "Plots/ForestPlotSmall.png",width = 900, height = 1100, units = "p")
par(mar=c(4,4,1,2), font = 1)
forest(res, xlim=c(-.8, 1.3), at = c(0,.25, .5, .75, 1),
       ilab = data.frame(dat$YearsStudied),
       ilab.xpos=c(-.12), cex=1.1, ylim=c(-1, length(dat$id)+3),
       xlab="Estimated power", mlab="", addfit = T, showweights = F)

text(-.8, -1, pos=4, cex=.95, bquote(paste("REML Model (Q = ",
                                           .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                           .(ifelse(res$QEp < .001, ", p < ", ", p = ")), .(formatC(ifelse(res$QEp < .001, .001, res$QEp), digits=3, format="f")), "; ", I^2, " = ", 
                                           .(formatC(res$I2, digits=1, format="f")), "%)")))
# Bold font 
par(font=2)
### add column headings to the plot
text(-.66, length(dat$id)+2, c("Author(s) (year)"), cex = 1.25)
text(-.12, length(dat$id)+2, c("Years sampled"), cex = 1.25)

text(c(-8.75,-5.25),     27, c("Vaccinated", "Control"))
text(-16,                26, "Author(s) and Year",  pos=4)
text(6,                  26, "Risk Ratio [95% CI]", pos=2)
# normal font 
par(font=1)
dev.off()


# Plot forest plot large benchmark (mean sd imputation)
res <- resLargeMean
png(filename = "Plots/ForestPlotLarge.png",width = 900, height = 1100, units = "p")
par(mar=c(4,4,1,2), font = 1)
forest(res, xlim=c(-.8, 1.3), at = c(0,.25, .5, .75, 1),
       ilab = data.frame(dat$YearsStudied),
       ilab.xpos=c(-.12), cex=1.1, ylim=c(-1, length(dat$id)+3),
       xlab="Estimated power", mlab="", addfit = T, showweights = F)

text(-.8, -1, pos=4, cex=.95, bquote(paste("REML Model (Q = ",
                                           .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                           .(ifelse(res$QEp < .001, ", p < ", ", p = ")), .(formatC(ifelse(res$QEp < .001, .001, res$QEp), digits=3, format="f")), "; ", I^2, " = ", 
                                           .(formatC(res$I2, digits=1, format="f")), "%)")))
# Bold font 
par(font=2)
### add column headings to the plot
text(-.66, length(dat$id)+2, c("Author(s) (year)"), cex = 1.25)
text(-.12, length(dat$id)+2, c("Years sampled"), cex = 1.25)

text(c(-8.75,-5.25),     27, c("Vaccinated", "Control"))
text(-16,                26, "Author(s) and Year",  pos=4)
text(6,                  26, "Risk Ratio [95% CI]", pos=2)
# normal font 
par(font=1)
dev.off()




### VERSION WITH AREA OF RESEARCH 

par(mar=c(4,4,1,2), font = 1)
forest(resMedMean, xlim=c(-.65, 1.15), at = c(0,.25, .5, .75, 1), ilab = data.frame(dat$YearsStudied, dat$SubfieldClassification),
       ilab.xpos=c(-.04, -.26), cex=0.75, ylim=c(-1, length(dat$id)+3),
       xlab="Estimated power", mlab="", addfit = T, showweights = F)



text(-.6, -1, pos=4, cex=0.75, bquote(paste("REML Model for All Studies (Q = ",
                                            .(formatC(resMedMean$QE, digits=2, format="f")), ", df = ", .(resMedMean$k - resMedMean$p),
                                            ", p = ", .(formatC(resMedMean$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(resMedMean$I2, digits=1, format="f")), "%)")))
# Bold font 
par(font=2)
### add column headings to the plot
text(-.675, length(dat$id)+2, c("Author(s) (year)"), cex = .75)
text(-.2, length(dat$id)+2, c("Years sampled"), cex = .75)

text(c(-8.75,-5.25),     27, c("Vaccinated", "Control"))
text(-16,                26, "Author(s) and Year",  pos=4)
text(6,                  26, "Risk Ratio [95% CI]", pos=2)
# normal font 
par(font=1)









par(mar=c(4,4,1,2))

### fit random-effects model (use slab argument to define study labels)
res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, measure="RR",
           slab=paste(author, year, sep=", "), method="REML")


forest(res, xlim=c(-16, 6), at=log(c(0.05, 0.25, 1, 4)), atransf=exp,
       ilab=cbind(dat.bcg$tpos, dat.bcg$tneg, dat.bcg$cpos, dat.bcg$cneg),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.75, ylim=c(-1, 27),
       order=order(dat.bcg$alloc), rows=c(3:4,9:15,20:23),
       xlab="Risk Ratio", mlab="", psize=1)


text(-16, -1, pos=4, cex=0.75, bquote(paste("RE Model for All Studies (Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))

### set font expansion factor (as in forest() above) and use bold italic
### font and save original settings in object 'op'
op <- par(cex=0.75, font=4)

### add text for the subgroups
text(-16, c(24,16,5), pos=4, c("Systematic Allocation",
                               "Random Allocation",
                               "Alternate Allocation"))

### switch to bold font
par(font=2)

### add column headings to the plot
text(c(-9.5,-8,-6,-4.5), 26, c("TB+", "TB-", "TB+", "TB-"))
text(c(-8.75,-5.25),     27, c("Vaccinated", "Control"))
text(-16,                26, "Author(s) and Year",  pos=4)
text(6,                  26, "Risk Ratio [95% CI]", pos=2)

### set par back to the original settings
par(op)







forest(resMedNoImpMLYear)
forest(resMedMeanMLYear)


# PLOT median 
samplingVar <- dat$samplingVarMed_Mean
values <- dat$estMedMean
## Model with unstandardised years for plotting
res <- rma.mv(yi = values, V = samplingVar, random = ~ 1 | id, mods = YearsStudiedMean,  data = dat, slab = StudyName)
### calculate predicted risk ratios for 0 to 60 degrees absolute latitude
preds <- predict(res, newmods =(min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))))

# forest
forest(res)

### calculate point sizes by rescaling the standard errors
wi    <- 1/sqrt(dat$samplingVarMed_NoImputedData)
size  <- 0.5 + 3.0 * (wi - min(wi, na.rm = T))/(max(wi, na.rm = T) - min(wi, na.rm = T))

### plot the risk ratios against absolute latitude
plot(dat$YearsStudiedMean[!is.na(samplingVar)], dat$estMedMean[!is.na(samplingVar)], pch=19, cex=size, 
     xlab="Year", ylab="Power at Medium Benchmark",
     las=1, bty="l")

### add predicted values (and corresponding CI bounds)
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$pred)
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$ci.lb, lty="dashed")
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$ci.ub, lty="dashed")



# PLOT UNSTANDARDISED NO IMPUTATION 

## Model with unstandardised years for plotting
UnstandMed_NoImputedData <- rma.mv(yi = estMedMean, V = samplingVarMed_NoImputedData, random = ~ 1 | id, mods = YearsStudiedMean,  data = dat, slab = StudyName)
### calculate predicted risk ratios for 0 to 60 degrees absolute latitude
preds <- predict(UnstandMed_NoImputedData, newmods =(min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))))

forest(UnstandMed_NoImputedData)

### calculate point sizes by rescaling the standard errors
wi    <- 1/sqrt(dat$samplingVarMed_NoImputedData)
size  <- 0.5 + 3.0 * (wi - min(wi, na.rm = T))/(max(wi, na.rm = T) - min(wi, na.rm = T))

### plot the risk ratios against absolute latitude
plot(dat$YearsStudiedMean[!is.na(dat$samplingVarMed_NoImputedData)], dat$estMedMean[!is.na(dat$samplingVarMed_NoImputedData)], pch=19, cex=size, 
     xlab="Year", ylab="Power at Medium Benchmark",
     las=1, bty="l")

### add predicted values (and corresponding CI bounds)
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$pred)
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$ci.lb, lty="dashed")
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$ci.ub, lty="dashed")







#### CHECKING THAT THERE ARE NO DISCREPANCIES IN COEFFICENT VALUES DEPENDING ON IMPUTATION METHOD #### 
max(unlist(c(c(resMedMax[[1]], resMedMin[[1]], resMedMed[[1]]) - c(resMedMean[[1]]),
             c(resMedMaxML[[1]], resMedMinML[[1]], resMedMedML[[1]]) - c(resMedMeanML[[1]]),
             data.frame(resMedMaxMLYear[[1]], resMedMinMLYear[[1]], resMedMedMLYear[[1]]) - data.frame(resMedMeanMLYear[[1]], resMedMeanMLYear[[1]], resMedMeanMLYear[[1]]))))

max(unlist(c(c(resSmallMax[[1]], resSmallMin[[1]], resSmallMed[[1]]) - c(resSmallMean[[1]]),
             c(resSmallMaxML[[1]], resSmallMinML[[1]], resSmallMedML[[1]]) - c(resSmallMeanML[[1]]),
             data.frame(resSmallMaxMLYear[[1]], resSmallMinMLYear[[1]], resSmallMedMLYear[[1]]) - data.frame(resSmallMeanMLYear[[1]], resSmallMeanMLYear[[1]], resSmallMeanMLYear[[1]]))))

max(unlist(c(c(resLargeMax[[1]], resLargeMin[[1]], resLargeMed[[1]]) - c(resLargeMean[[1]]),
             c(resLargeMaxML[[1]], resLargeMinML[[1]], resLargeMedML[[1]]) - c(resLargeMeanML[[1]]),
             data.frame(resLargeMaxMLYear[[1]], resLargeMinMLYear[[1]], resLargeMedMLYear[[1]]) - data.frame(resLargeMeanMLYear[[1]], resLargeMeanMLYear[[1]], resLargeMeanMLYear[[1]]))))


## Discrepancies depending on whether data is imputed or not
resSmallNoImpMLYear; resSmallMeanMLYear
resMedNoImpMLYear; resMedMeanMLYear
resLargeNoImpMLYear; resLargeMeanMLYear






