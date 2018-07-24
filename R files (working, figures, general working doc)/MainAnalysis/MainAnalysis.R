# packages 
library(readxl)

# importing data
data <- read_excel("~/PhD/Systematic Reviews/History of Power Estimation Studies/PowerEstimationReviewDataCollection2018.03.25.xlsx")

# Counting
# number of articles included = 50 (i.e., not discounting for missing data) 
length(unique(subset(data$id, is.na(data$exclude) == TRUE))) 
# number of datapoints = 64
sum(is.na(data$exclude))

# removing all the exlusions
dat <- subset(data, is.na(data$exclude))

# calculating variance and IQR 
dat$varMedium <- dat$SDPowerAtMedium^2  
dat$IQRMedium <- dat$ThirdQuartilePowerAtMedium - dat$FirstQuartilePowerAtMedium

which(is.na(dat$varMedium) & is.na(dat$IQRMedium))

# num giving the median power at a medium benchmark
sum(!is.na(dat$PowerAtLargeEffectMedian))

# importing functions from veramata to estimate means from medians and to estimate the SE for various methods
# (See Charles Grey's Dissertation for the completed package), or https://github.com/softloud/varameta before she finishes it! 
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


## setting up col for storring means + estiamted means 
is.na(dat$PowerAtMediumEffectMean) & is.na(dat$IQRMedium)
  
  
  
  
  
  
  
  
