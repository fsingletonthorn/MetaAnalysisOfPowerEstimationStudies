# packages 
library(readxl)

# importing data
data <- read_excel("~/PhD/Systematic Reviews/History of Power Estimation Studies/PowerEstimationReviewDataCollection2018.03.25.xlsx")

# Counting
# number of articles included = 50 (i.e., not discounting for missing data) 
length(unique(subset(data$id, is.na(data$exclude) == TRUE))) 
# number of datapoints (i.e., non exlcuded papers)
sum(is.na(data$exclude))

# removing all the exlusions
dat <- subset(data, is.na(data$exclude))

# converting to numerics
dat$NumberOfArticles<-as.numeric(dat$NumberOfArticles)

# calculating variance and IQR 
dat$varMedium <- dat$SDPowerAtMedium^2  
dat$IQRMedium <- dat$ThirdQuartilePowerAtMedium - dat$FirstQuartilePowerAtMedium

which(is.na(dat$varMedium) & is.na(dat$IQRMedium))

# num giving the median power at a medium benchmark
sum(!is.na(dat$PowerAtSmallEffectMedian))
sum(!is.na(dat$PowerAtMediumEffectMedian))
sum(!is.na(dat$PowerAtLargeEffectMedian))

# number missing means and SDs but providing SDs quartiles and medians
sum(!is.na(dat$PowerAtSmallEffectMedian) & is.na(dat$SDPowerAtMedium) & is.na(dat$SDMedAlgEstFromCDT) & !is.na(dat$PowerAtMediumEffectMedian) & !is.na(dat$FirstQuartilePowerAtMedium) & !is.na(dat$ThirdQuartilePowerAtMedium))
# n articles  missing means and SDs but providing SDs quartiles and medians
length(unique(dat$id[(!is.na(dat$PowerAtSmallEffectMedian) & is.na(dat$SDPowerAtMedium) & is.na(dat$SDMedAlgEstFromCDT) & !is.na(dat$PowerAtMediumEffectMedian) & !is.na(dat$FirstQuartilePowerAtMedium) & !is.na(dat$ThirdQuartilePowerAtMedium))]))
# N articles no variances or quartiles 
length(unique(dat$id[(is.na(dat$SDPowerAtMedium) & is.na(dat$SDMedAlgEstFromCDT) & !is.na(dat$PowerAtMediumEffectMedian) & is.na(dat$FirstQuartilePowerAtMedium) & is.na(dat$ThirdQuartilePowerAtMedium))]))
# n article 

## putting estimated Ms and SDs into the appropraite places 
# 
# dat[c("PowerAtSmallEffectMean", "PowerAtMediumEffectMean", "PowerAtLargeEffectMean")][dat$id == 42,] <- c(0.2219, 0.68865, 0.8952)
dat[c("PowerAtSmallEffectMean", "PowerAtMediumEffectMean", "PowerAtLargeEffectMean")][dat$id == 62,] <- c(0.2456618, 0.5399306, 0.67625)

# the following estimators are from Wan, X., Wang, W., Liu, J., & Tong, T. (2014). Estimating the sample mean and standard deviation from the sample size, median, range and/or interquartile range. BMC Medical Research Methodology, 14(1), 135. doi:10.1186/1471-2288-14-135
# But the functions are from the package veramata, See Charles Grey's Dissertation for the completed package, or https://github.com/softloud/varameta before she finishes it. 
# parameters in the following functions: 
#   a Minimum value of sample.
##  m Median of sample.
##  b Maximum value of sample.
##  n Sample size.
##  q1 first quartile
##  q3 third quartile

eff_est_wan_c1 <- function(a, m, b, n) {
  x.bar = (a + 2 * m + b) / 4
  s = (b - a) / (2 * qnorm(
    (n - 0.375) / (n + 0.25)
  ))
  
  return(list(
    centre = x.bar,
    se = s / sqrt(n)
  ))
}

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
  
  # Calculate the standard deviation of all studies' effects.
  #
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
dat$varSmall[!is.na(dat$SDPowerAtSmall)] <- dat$SDPowerAtSmall[!is.na(dat$SDPowerAtSmall)]


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
dat$varMed[!is.na(dat$SDPowerAtMedium)] <- dat$SDPowerAtMedium[!is.na(dat$SDPowerAtMedium)]

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
dat$varLarge[!is.na(dat$SDPowerAtLarge)] <- dat$SDPowerAtLarge[!is.na(dat$SDPowerAtLarge)]

##### Calculating mean absolute diffs
meanAbsDiffVariance <- mean(c(absDiffVarLarge, absDiffVarMed, absDiffVarSmall) , na.rm = T)
mean(c(absDiffVarMed) , na.rm = T)
meanAbsDiffMean <- mean(c(absDiffLarge, absDiffMed, absDiffSmall), na.rm = T)


### Need to impute SDs here for articles with various approaches




weighted.mean(dat$estMedMean[!is.na(dat$estMedMean) & !is.na(as.numeric(dat$NumberOfArticles))], 
              as.numeric(dat$NumberOfArticles[!is.na(dat$estMedMean) & !is.na(as.numeric(dat$NumberOfArticles))]))

weighted.mean(dat$estSmallMean[!is.na(dat$estSmallMean) & !is.na(as.numeric(dat$NumberOfArticles))], 
              as.numeric(dat$NumberOfArticles[!is.na(dat$estSmallMean) & !is.na(as.numeric(dat$NumberOfArticles))]))

weighted.mean(dat$estLargeMean[!is.na(dat$estLargeMean) & !is.na(as.numeric(dat$NumberOfArticles))], 
              as.numeric(dat$NumberOfArticles[!is.na(dat$estLargeMean) & !is.na(as.numeric(dat$NumberOfArticles))]))



