# packages 
library(readxl)
library(metafor)
library(tidyverse)
library(stringr)
library(ggplot2)
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

effect_se <- function(centre,
                      spread,
                      n,
                      centre_type = "mean",
                      spread_type = "sd") {
  
  # Estimate the standard error of the effect, depending on how that effect is
  # reported (median or mean).
  #
  # @param centre A sample mean or a median.
  # @param spread The associated measure of spread for the sample mean: either
  # a sample sd, sample interquartile range, or sample range.
  # @param n The sample size.
  # @param centre_type Specify if the center is "mean" or "median".
  # @param spread_type Specify if the spread is reported as "sd", "var", "iqr", or "range".
  #
  # @export
  
  if (centre_type == "mean" & spread_type == "sd") {
    return(se = spread / sqrt(n))
  } else if (centre_type == "median") {
    if (spread_type == "iqr") {
      sn_arg <- 3 / 4
    } else if (spread_type == "range") {
      sn_arg <- (n - 1 / 2) / n
    } else if (spread_type == "var") {
      return(se = sqrt(spread /  n))
    } else {
      stop("Check that your spread_type is either \"var\",  \"iqr\", or \"range\".")
    }
    
    # Estimate mu.
    mu <- log(centre)
    
    # Estimate sigma.
    sigma <-
      1 / qnorm(sn_arg) *
      log(1 / 2 *
            (spread * exp(-mu) + sqrt(spread ^ 2 * exp(-2 * mu) + 4)))
    
    return(1 / (2 * sqrt(n) * dlnorm(
      centre, meanlog = mu, sdlog = sigma
    )))
  } else {
    stop("Check that your centre_type is of the form \"mean\" or \"median\".")
  }
}

######### data importing and wrangling ###############
# importing data
data <- read_excel("~/PhD/Systematic Reviews/History of Power Estimation Studies/PowerEstimationReviewDataCollection2018.03.25.xlsx")

# Counting
# number of articles included (i.e., not discounting for missing data) 
N <- length(unique(subset(data$id, is.na(data$exclude) == TRUE))) 
# number of datapoints (i.e., non exlcuded papers)
n <- sum(is.na(data$exclude))

# removing all of the exlusions
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
# Number missing SDs
sum(is.na(dat$SDPowerAtSmall) |is.na(dat$SDPowerAtMedium)| is.na(dat$SDPowerAtLarge) )
# Number missing means
sum(is.na(dat$PowerAtLargeEffectMean) | is.na(dat$PowerAtMediumEffectMean) | is.na(dat$PowerAtSmallEffectMean))

# number missing means and SDs but providing SDs quartiles and medians
sum(!is.na(dat$PowerAtSmallEffectMedian) & is.na(dat$SDPowerAtMedium) & is.na(dat$SDMedAlgEstFromCDT) & !is.na(dat$PowerAtMediumEffectMedian) & !is.na(dat$FirstQuartilePowerAtMedium) & !is.na(dat$ThirdQuartilePowerAtMedium))
# n articles  missing means and SDs but providing SDs quartiles and medians
length(unique(dat$id[(!is.na(dat$PowerAtSmallEffectMedian) & is.na(dat$SDPowerAtMedium) & is.na(dat$SDMedAlgEstFromCDT) & !is.na(dat$PowerAtMediumEffectMedian) & !is.na(dat$FirstQuartilePowerAtMedium) & !is.na(dat$ThirdQuartilePowerAtMedium))]))
# N articles no variances or quartiles 
length(unique(dat$id[(is.na(dat$SDPowerAtMedium) & is.na(dat$SDMedAlgEstFromCDT) & !is.na(dat$PowerAtMediumEffectMedian) & is.na(dat$FirstQuartilePowerAtMedium) & is.na(dat$ThirdQuartilePowerAtMedium))]))

## Recoding areas of reserach as per the preregistration 
# “clinical psychology/psychiatry”, “social/personality”, “education”, “general psychology” (i.e., those studies which look across fields of psychology research), “management/IO psychology”, “cognitive psychology” “neuropsychology”, “meta-analysis”

dat$SubfieldClassification[dat$SubfieldClassification == "Cognitive neuroscience, psychology, psychiatry"] <- "General Psychology"
dat$SubfieldClassification[dat$SubfieldClassification == "Clinical"] <- "Clinical Psychology/Psychiatry"
dat$SubfieldClassification[dat$SubfieldClassification == "Neuroscience"] <- "Neuropsychology"
dat$SubfieldClassification[dat$SubfieldClassification == "Medical Education"] <- "Education"

# removing cohen 1962's at large and small effect sizes (benchmark values were different)
dat$PowerAtSmallEffectMedian[dat$id==103] <- NA
dat$PowerAtLargeEffectMedian[dat$id==103] <- NA
dat$PowerAtSmallEffectMean[dat$id==103] <- NA
dat$PowerAtLargeEffectMean[dat$id==103] <- NA

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

# Counting the number of articles for which the wanc3estmean is used 
sum(!is.na(dat$estSmallMean) & (is.na(dat$PowerAtSmallEffectMean)))

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
absDiffVarMed <- abs(dat$varMed - dat$SDPowerAtMedium^2)
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
# counting number of articles for which this varaiance and means estimation approach could be validated against 
# N means validated against
sum(!is.na(data.frame(absDiffLarge, absDiffMed, absDiffSmall)))
# N articles means validated against
sum(!is.na(rowMeans(data.frame(absDiffLarge, absDiffMed, absDiffSmall), na.rm = T)))
# n articles variances validated against
sum(!is.na(rowMeans(data.frame(absDiffVarLarge, absDiffVarMed, absDiffVarSmall), na.rm = T)))
# n variances validated against
sum(!is.na(data.frame(absDiffVarLarge, absDiffVarMed, absDiffVarSmall)))

## putting Means estimated from frequency plots into the appropraite places 
dat[c("estSmallMean", "estMedMean", "estLargeMean")][dat$id == 62,] <- c(0.2456618, 0.5399306, 0.67625)

######## Missing data imputation #########
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
resSmallNoImpMLYearField <- rma.mv(yi = estSmallMean, V = samplingVarSmall_NoImputedData, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)

# Medium 
resMedNoImpMLYearField <- rma.mv(yi = estMedMean, V = samplingVarMed_NoImputedData, random =  ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)

# Large
resLargeNoImpMLYearField <- rma.mv(yi = estLargeMean, V = samplingVarLarge_NoImputedData, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)


####### MEDIUM EFFECT SIZE BENCHMARK 
######## model accounting for area of research # Models with means 
resMedMeanMLYearField <- rma.mv(yi = estMedMean, V = samplingVarMed_Mean, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)), slab=StudyName,  data = dat)
# ICCs
round(resMedMeanMLYearField$sigma2[1] / sum(resMedMeanMLYearField$sigma2), 3)
round(resMedMeanMLYearField$sigma2[2] / sum(resMedMeanMLYearField$sigma2), 3)
round(resMedMeanMLYearField$sigma2[3] / sum(resMedMeanMLYearField$sigma2), 3)

## same models with median imputation
######## model accounting for area of research 
resMedMedMLYearField <- rma.mv(yi = estMedMean, V = samplingVarMed_Med, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)

## same models with min imputation 
resMedMinMLYearField <- rma.mv(yi = estMedMean, V = samplingVarMed_Min, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)

## same models with max imputation 
resMedMaxMLYearField <- rma.mv(yi = estMedMean, V = samplingVarMed_Max, random = list(~ 1 | id, ~ 1 |  SubfieldClassification), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)


#### SMALL EFFECT SIZE BENCHMARK
resSmallMeanMLYearField <- rma.mv(yi = estSmallMean, V = samplingVarSmall_Mean, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat, slab=StudyName)
# ICCs
round(resSmallMeanMLYearField$sigma2[1] / sum(resSmallMeanMLYearField$sigma2), 3)
round(resSmallMeanMLYearField$sigma2[2] / sum(resSmallMeanMLYearField$sigma2), 3)
round(resSmallMeanMLYearField$sigma2[3] / sum(resSmallMeanMLYearField$sigma2), 3)

## same models with median imputation
resSmallMedMLYearField <- rma.mv(yi = estSmallMean, V = samplingVarSmall_Med, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)

## same models with min imputation 
resSmallMinMLYearField <- rma.mv(yi = estSmallMean, V = samplingVarSmall_Min, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)

## same models with max imputation 
resSmallMaxMLYearField <- rma.mv(yi = estSmallMean, V = samplingVarSmall_Max, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)

#### LARGE EFFECT SIZE BENCHMARK
# Models with mean imputation
resLargeMeanMLYearField <- rma.mv(yi = estLargeMean, V = samplingVarLarge_Mean, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat, slab=StudyName)
# ICCs
round(resMedMeanMLYearField$sigma2[1] / sum(resMedMeanMLYearField$sigma2), 3)
round(resMedMeanMLYearField$sigma2[2] / sum(resMedMeanMLYearField$sigma2), 3)
round(resMedMeanMLYearField$sigma2[3] / sum(resMedMeanMLYearField$sigma2), 3)


## same models with median imputation
resLargeMedMLYearField <- rma.mv(yi = estLargeMean, V = samplingVarLarge_Med, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)

## same models with min imputation 
resLargeMinMLYearField <- rma.mv(yi = estLargeMean, V = samplingVarLarge_Min, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)

## same models with max imputation 
resLargeMaxMLYearField <- rma.mv(yi = estLargeMean, V = samplingVarLarge_Max, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat)


#### Sensititivty analysis ####
##### Weigting by number of studies ####
# Estimating these with added year and field of research 
mediumWeightedYearField <- rma.mv(yi = estMedMean, V = dat$samplingVarMed_Mean, random = ~ 1 | SubfieldClassification / id / I(1:53),mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat, slab=StudyName, W = dat$NumberOfArticles)
smallWeightedYearField <-  rma.mv(yi = estSmallMean, V = dat$samplingVarSmall_Mean, random = ~ 1 | SubfieldClassification / id / I(1:53),mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat, slab=StudyName, W = dat$NumberOfArticles)
largeWeightedYearField <-  rma.mv(yi = estLargeMean, V = dat$samplingVarLarge_Mean, random = ~ 1 | SubfieldClassification / id / I(1:53),mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat, slab=StudyName, W = dat$NumberOfArticles)


## Estimating differences caused by weigting by number of studies 
mediumWeightedYearField$b - resMedMeanMLYearField$b
smallWeightedYearField$b - resSmallMeanMLYearField$b
largeWeightedYearField$b - resLargeMeanMLYearField$b

#### Differences with imputation or not ####
## Estimating differences caused by data imputation 
resSmallNoImpMLYearField$b - resSmallMeanMLYearField$b
resMedNoImpMLYearField$b - resMedMeanMLYearField$b
resLargeNoImpMLYearField$b - resLargeMeanMLYearField$b


### Sensitivity analysis output for final model 

smallModelsYearField <- t(data.frame(resSmallMeanMLYearField$b,
                                     smallWeightedYearField$b,
                                     resSmallNoImpMLYearField$b,
                                     resSmallMaxMLYearField$b,
                                     resSmallMinMLYearField$b,
                                     resSmallMedMLYearField$b))

mediumModelsYearField <- t(data.frame(resMedMeanMLYearField$b, 
                                      mediumWeightedYearField$b,
                                      resMedNoImpMLYearField$b,
                                      resMedMaxMLYearField$b,
                                      resMedMinMLYearField$b,
                                      resMedMedMLYearField$b))

largeModelsYearField <- t(data.frame(resLargeMeanMLYearField$b, 
                                     largeWeightedYearField$b,
                                     resLargeNoImpMLYearField$b,
                                     resLargeMaxMLYearField$b,
                                     resLargeMinMLYearField$b,
                                     resLargeMedMLYearField$b)) 

SensitivityAnalysisYearField<- data.frame(smallModelsYearField, mediumModelsYearField, largeModelsYearField)
# write.csv(round(SensitivityAnalysisYearField, 3), file = "Plots/TableSensitivity3.csv")

# max change from different imputation methods 

max(data.frame(resLargeMaxMLYearField$b, resLargeMinMLYearField$b, resLargeMedMLYearField$b) - data.frame(resLargeMeanMLYearField$b,resLargeMeanMLYearField$b,resLargeMeanMLYearField$b))
max(data.frame(resSmallMaxMLYearField$b, resSmallMinMLYearField$b, resSmallMedMLYearField$b) - data.frame(resSmallMeanMLYearField$b,resSmallMeanMLYearField$b,resSmallMeanMLYearField$b))
max(data.frame(resMedMaxMLYearField$b, resMedMinMLYearField$b, resMedMedMLYearField$b) - data.frame(resMedMeanMLYearField$b,resMedMeanMLYearField$b,resMedMeanMLYearField$b))


## Checking what happens if we include the two .99s with minimum variances 
######## model accounting for area of research 
dat$samplingVarLarge_Mean
resLargeMeanMLYearField <- rma.mv(yi = estLargeMean, V = samplingVarLarge_Mean, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)),  data = dat, slab=StudyName)


####### Plots #######
# Plot forest plot medium benchmark (mean imputation)
png(filename = "Plots/ForestPlotMedium.png",width = 950, height = 1200, units = "p")
par(mar=c(4,4,1,2), font = 1)
res <- resMedMeanMLYearField
forest(res, xlim=c(-1.5, 1.35), at = c(0,.25, .5, .75, 1),
       ilab = data.frame(dat$YearsStudied, dat$SubfieldClassification),
       ilab.xpos=c(-.16, -.58), cex=1.1, ylim=c(-1, length(dat$id)+3),
       xlab="Estimated power", mlab="", addfit = F, showweights = F)


addpoly(x =  res$b[1], ci.lb = res$ci.lb[1], ci.ub = res$ci.ub[1], cex = 1.1)

# Bold font 
par(font=2)
### add column headings to the plot
text(-1.3, length(dat$id)+2, c("Author(s) (year)"), cex = 1.25)
text(-.58, length(dat$id)+2, c("Subfield"), cex = 1.25)
text(-.18, length(dat$id)+2, c("Years sampled"), cex = 1.25)
# normal font 
par(font=1)
dev.off()


# Plot forest plot small benchmark (mean sd imputation)
res <- resSmallMeanMLYearField
png(filename = "Plots/ForestPlotSmall.png",width = 950, height = 1200, units = "p")
par(mar=c(4,4,1,2), font = 1)
forest(res, xlim=c(-1.5, 1.35), at = c(0,.25, .5, .75, 1),
       ilab = data.frame(dat$YearsStudied, dat$SubfieldClassification),
       ilab.xpos=c(-.16, -.58), cex=1.1, ylim=c(-1, res$k+3),
       xlab="Estimated power", mlab="", addfit = F, showweights = F)


addpoly(x =  res$b[1], ci.lb = res$ci.lb[1], ci.ub = res$ci.ub[1], cex = 1.1)

# Bold font 
par(font=2)
### add column headings to the plot
text(-1.3, res$k+2, c("Author(s) (year)"), cex = 1.25)
text(-.58, res$k+2, c("Subfield"), cex = 1.25)
text(-.18, res$k+2, c("Years sampled"), cex = 1.25)
# normal font 
par(font=1)
dev.off()


# Plot forest plot large benchmark (mean sd imputation)
res <- resLargeMeanMLYearField
png(filename = "Plots/ForestPlotLarge.png",width = 950, height = 1200, units = "p")
par(mar=c(4,4,1,2), font = 1)
forest(res, xlim=c(-1.5, 1.35), at = c(0,.25, .5, .75, 1),
       ilab = data.frame(dat$YearsStudied, dat$SubfieldClassification),
       ilab.xpos=c(-.16, -.58), cex=1.1, ylim=c(-1, res$k+3),
       xlab="Estimated power", mlab="", addfit = F, showweights = F)


addpoly(x =  res$b[1], ci.lb = res$ci.lb[1], ci.ub = res$ci.ub[1], cex = 1.1)

# Bold font 
par(font=2)
### add column headings to the plot
text(-1.3, res$k+2, c("Author(s) (year)"), cex = 1.25)
text(-.58, res$k+2, c("Subfield"), cex = 1.25)
text(-.18, res$k+2, c("Years sampled"), cex = 1.25)
# normal font 
par(font=1)
dev.off()


#### Plots of change over time 

##### Small 
png(filename = "Plots/ScatterPlotSmall.png",width = 800, height = 500, units = "p")
# PLOT medium  
samplingVar <- dat$samplingVarSmall_Mean
values <- dat$estSmallMean
## Model with unstandardised years for plotting
res <-  rma.mv(yi = estSmallMean, V = samplingVarSmall_Mean, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ YearsStudiedMean,  data = dat)
### calculate predicted risk ratios for 0 to 60 degrees absolute latitude
preds <- predict(res, newmods =(min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))))

### calculate point sizes by rescaling the standard errors
wi    <- 1/sqrt(samplingVar)
size  <- 0.5 + 3.0 * (wi - min(wi, na.rm = T))/(max(wi, na.rm = T) - min(wi, na.rm = T))

### plot the risk ratios against absolute latitude
plot(dat$YearsStudiedMean[!is.na(samplingVar)], values[!is.na(samplingVar)], pch=19, cex=size, 
     xlab="Year", ylab="Power at Small Benchmark", ylim = c(0,1),
     las=1, bty="l")

### add predicted values (and corresponding CI bounds)
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$pred)
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$ci.lb, lty="dashed")
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$ci.ub, lty="dashed")
dev.off()

##### Medium 
png(filename = "Plots/ScatterPlotMedium.png",width = 800, height = 500, units = "p")
# PLOT medium  
samplingVar <- dat$samplingVarMed_Mean
values <- dat$estMedMean
## Model with unstandardised years for plotting
res <- rma.mv(yi = values, V = samplingVar, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ YearsStudiedMean,  data = dat)
### calculate predicted risk ratios for 0 to 60 degrees absolute latitude
preds <- predict(res, newmods =(min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))))

### calculate point sizes by rescaling the standard errors
wi    <- 1/sqrt(samplingVar)
size  <- 0.5 + 3.0 * (wi - min(wi, na.rm = T))/(max(wi, na.rm = T) - min(wi, na.rm = T))

### plot the risk ratios against absolute latitude
plot(dat$YearsStudiedMean[!is.na(samplingVar)], dat$estMedMean[!is.na(samplingVar)], pch=19, cex=size, 
     xlab="Year", ylab="Power at Medium Benchmark", ylim = c(0,1),
     las=1, bty="l")

### add predicted values (and corresponding CI bounds)
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$pred)
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$ci.lb, lty="dashed")
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$ci.ub, lty="dashed")
dev.off()


##### Large 
png(filename = "Plots/ScatterPlotLarge.png",width = 800, height = 500, units = "p")
# PLOT values   
samplingVar <- dat$samplingVarLarge_Mean
values <- dat$estLargeMean
## Model with unstandardised years for plotting
res <- rma.mv(yi = values, V = samplingVar, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ YearsStudiedMean,  data = dat)
### calculate predicted risk ratios for 0 to 60 degrees absolute latitude
preds <- predict(res, newmods =(min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))))

### calculate point sizes by rescaling the standard errors
wi    <- 1/sqrt(samplingVar)
size  <- 0.5 + 3.0 * (wi - min(wi, na.rm = T))/(max(wi, na.rm = T) - min(wi, na.rm = T))

### plot the risk ratios against absolute latitude
plot(dat$YearsStudiedMean[!is.na(samplingVar)], values[!is.na(samplingVar)], pch=19, cex=size, 
     xlab="Year", ylab="Power at Large Benchmark", ylim = c(0,1),
     las=1, bty="l")

### add predicted values (and corresponding CI bounds)
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$pred)
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$ci.lb, lty="dashed")
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$ci.ub, lty="dashed")
dev.off()


#### Plots of change over time 

##### Small 
png(filename = "Plots/ScatterPlotSmall.png",width = 800, height = 500, units = "p")
# PLOT medium  
samplingVar <- dat$samplingVarSmall_Mean
values <- dat$estSmallMean
## Model with unstandardised years for plotting
res <- rma.mv(yi = values, V = samplingVar, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = YearsStudiedMean,  data = dat, slab = StudyName)
### calculate predicted 
preds <- predict(res, newmods =(min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))))

### calculate point sizes by rescaling the standard errors
wi    <- 1/sqrt(samplingVar)
size  <- 0.5 + 3.0 * (wi - min(wi, na.rm = T))/(max(wi, na.rm = T) - min(wi, na.rm = T))

### plot the risk ratios against absolute latitude
plot(dat$YearsStudiedMean[!is.na(samplingVar)], values[!is.na(samplingVar)], pch=19, cex=size, 
     xlab="Year", ylab="Power at Small Benchmark", ylim = c(0,1),
     las=1, bty="l")

### add predicted values (and corresponding CI bounds)
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$pred)
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$ci.lb, lty="dashed")
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$ci.ub, lty="dashed")
dev.off()

##### Medium 
png(filename = "Plots/ScatterPlotMediumSimple.png",width = 800, height = 500, units = "p")
# PLOT medium  
samplingVar <- dat$samplingVarMed_Mean
values <- dat$estMedMean
## Model with unstandardised years for plotting
res <- rma.mv(yi = values, V = samplingVar, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = YearsStudiedMean,  data = dat, slab = StudyName)
### calculate predicted scores 
preds <- predict(res, newmods =(min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))))

### calculate point sizes by rescaling the standard errors
wi    <- 1/sqrt(samplingVar)
size  <- 0.5 + 3.0 * (wi - min(wi, na.rm = T))/(max(wi, na.rm = T) - min(wi, na.rm = T))

### plot the risk ratios against absolute latitude
plot(dat$YearsStudiedMean[!is.na(samplingVar)], dat$estMedMean[!is.na(samplingVar)], pch=19, cex=size, 
     xlab="Year", ylab="Power at Medium Benchmark", ylim = c(0,1),
     las=1, bty="l")

### add predicted values (and corresponding CI bounds)
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$pred)
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$ci.lb, lty="dashed")
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$ci.ub, lty="dashed")

dev.off()

mean(dat$samplingVarMed_Mean)
mean(dat$samplingVarLarge_Mean)


##### Large 
png(filename = "Plots/ScatterPlotLargeSimple.png",width = 800, height = 500, units = "p")
# PLOT values   
samplingVar <- dat$samplingVarLarge_Mean
values <- dat$estLargeMean
## Model with unstandardised years for plotting
res <- rma.mv(yi = values, V = samplingVar, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = YearsStudiedMean,  data = dat, slab = StudyName)
### predict 
preds <- predict(res, newmods =(min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))))

### calculate point sizes by rescaling the standard errors
wi    <- 1/sqrt(samplingVar)
size  <- 0.5 + 3.0 * (wi - min(wi, na.rm = T))/(max(wi, na.rm = T) - min(wi, na.rm = T))

### plot the risk ratios against absolute latitude
plot(dat$YearsStudiedMean[!is.na(samplingVar)], values[!is.na(samplingVar)], pch=19, cex=size, 
     xlab="Year", ylab="Power at Large Benchmark", ylim = c(0,1),
     las=1, bty="l")

### add predicted values (and corresponding CI bounds)
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$pred)
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$ci.lb, lty="dashed")
lines((min(dat$YearsStudiedMean):ceiling(max(dat$YearsStudiedMean))), preds$ci.ub, lty="dashed")
dev.off()




## Model diagnostics #### 
l1OutSlopeS <-  rep(NA, length(dat$id))
l1OutSlopeM <-  rep(NA, length(dat$id))
l1OutSlopeL <-  rep(NA, length(dat$id))

l1OutInterceptS <- rep(NA, length(dat$id))
l1OutInterceptM <- rep(NA, length(dat$id))
l1OutInterceptL <- rep(NA, length(dat$id))

# Leave one our cv 
dat <- as.data.frame(dat) 
dat$id2 <- 1:length(dat$id) 

for(i in 1:length(dat$id)) {
  tempS <- rma.mv(yi = estSmallMean, V = samplingVarSmall_Mean, random = ~ 1 | SubfieldClassification / id / id2, mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)), slab=StudyName,  data = dat[-which(dat$id==dat$id[i]),])
  tempM <- rma.mv(yi = estMedMean, V = samplingVarMed_Mean, random = ~ 1 | SubfieldClassification / id / id2, mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)), slab=StudyName, data = dat[-which(dat$id == dat$id[i]),])
  tempL <- rma.mv(yi = estLargeMean, V = samplingVarLarge_Mean, random = ~ 1 | SubfieldClassification / id / id2, mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)), slab=StudyName,  data = dat[-which(dat$id == dat$id[i]),])
  l1OutSlopeS[i] <- tempS$beta[2]
  l1OutSlopeM[i] <- tempM$beta[2]
  l1OutSlopeL[i] <- tempL$beta[2]
  
  l1OutInterceptS[i] <- tempS$beta[1]
  l1OutInterceptM[i] <- tempM$beta[1]
  l1OutInterceptL[i] <- tempL$beta[1]
}

# Quickly viewing the differences seen with leave one out cross validation 
round(l1OutSlopeS - resSmallMeanMLYearField$b[2], 3)
round(l1OutInterceptS - resSmallMeanMLYearField$b[1], 3)
round(l1OutSlopeM - resMedMeanMLYearField$b[2], 3)
round(l1OutInterceptM - resMedMeanMLYearField$b[1], 3)
round(l1OutSlopeL - resLargeMeanMLYearField$b[2], 3)
round(l1OutInterceptL - resLargeMeanMLYearField$b[1], 3)

# calculating the maximum differences seen
max(l1OutSlopeS - resSmallMeanMLYearField$b[2])
max(l1OutInterceptS - resSmallMeanMLYearField$b[1])
max(l1OutSlopeM - resMedMeanMLYearField$b[2])
max(l1OutInterceptM - resMedMeanMLYearField$b[1])
max(l1OutInterceptL - resLargeMeanMLYearField$b[1])
max(l1OutSlopeL - resLargeMeanMLYearField$b[2])

## Estimating the impact of pub bias ~ confounded with the issue that samping variance is expected to be confounded with proximity to the upper and lower benchmark
rma.mv(yi = estSmallMean, V = samplingVarSmall_Mean, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)) + samplingVarSmall_Mean, slab=StudyName,  data = dat)
rma.mv(yi = estMedMean, V = samplingVarMed_Mean, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)) + samplingVarMed_Mean, slab=StudyName,  data = dat)
rma.mv(yi = estLargeMean, V = samplingVarLarge_Mean, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)) + samplingVarLarge_Mean, slab=StudyName,  data = dat)

## Estimating the association b/w sample size and estimated power not sampling variance #### 
rma.mv(yi = estMedMean, V = samplingVarMed_Mean, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)) + NumberOfArticles, slab=StudyName,  data = dat)
rma.mv(yi = estSmallMean, V = samplingVarSmall_Mean, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)) + NumberOfArticles, slab=StudyName,  data = dat)
rma.mv(yi = estLargeMean, V = samplingVarLarge_Mean, random = ~ 1 | SubfieldClassification / id / I(1:53), mods = ~ as.numeric(YearsStudiedMean - mean(YearsStudiedMean)) + NumberOfArticles, slab=StudyName,  data = dat)

#### Descriptives####
# Subfield classification table
knitr::kable(table(dat$SubfieldClassification)[order(table(dat$SubfieldClassification), decreasing = T)])

#Years included in analysis diagram
# splitting multiple years at "-"
years <- str_split(dat$YearsStudied, "-", simplify = T)
# converting to numerics
years <- apply(years,  2, as.numeric)

years <- data.frame(years, dat$SubfieldClassification, (log(dat$NumberOfArticles)/(max(log(dat$NumberOfArticles)))))
names(years) <- c('startYear','endYear', 'Subfield', "NumArticles")
# years$endYear <- ifelse(is.na(years$endYear), years$startYear, years$endYear)
years <- years[order(years['startYear']),]
coolplot<-ggplot(years, aes(Subfield, y= startYear, ymin = startYear, ymax = endYear, colour = Subfield)) + 
  theme_classic() +
  geom_linerange(na.rm = T, alpha = .7, position = position_jitter(w = .3, h = .0), size = (log(dat$NumberOfArticles))) + coord_flip() +
  geom_point(na.rm = T, alpha = .7, position = position_jitter(w = .3, h = .0), size = (log(dat$NumberOfArticles))) +
  theme(legend.position="bottom", legend.justification = c(-.5, 1), legend.title = element_blank()) +  
  ylab("Year") 
coolplot
ggsave("PlotW.pdf", coolplot, device = 'pdf', width = 25, height = 12, units = 'cm')

# Random effects empirical bayes estimates 
write.csv(round(ranef(resMedMeanMLYearField)$SubfieldClassification, 3), "Plots/bloopMed.csv")
write.csv(round(ranef(resSmallMaxMLYearField)$SubfieldClassification, 3), "Plots/bloopSmall.csv")
write.csv(round(ranef(resLargeMeanMLYearField)$SubfieldClassification, 3), "Plots/bloopLarge.csv")