# Sensativity analysis of the secondary analysis of the proportion of studies reporting a PA, meta-anaylsis without mods, and meta-analysis with year as a moderator

#library(metafor); library(readxl)

# Importing data - you will need to replace this file path with the file path to the file from https://osf.io/gpvbq/
dataSetOri <- read_excel("SecondaryAnalysisData2018.03.25.xlsx", 
                         sheet = "Data_prop_reporting_PA")
dataSetOri <-as.data.frame(dataSetOri)

dataSet  <- dataSetOri

############# Data cleaning ############ 

### replace year data with median year ###

#extract years vector for easy use
years<-dataSet$YearsStudied
# Extract just those studies that cover 1 year (produces warning, as studies which cover ranges are changed to NAs)
medianYear<-as.numeric(years) # as.numeric(years)[is.na(as.numeric(years))]

# index through from 1 to years
for(i in 1:length(years)) {
  # if the years is not a numeric (i.e., if "as.numeric(years)[i]" is na, AND unlisting a split string of years[i] does produce a values (i.e., (as.numeric(unlist(strsplit(years[i], "-"))[2])) is NOT NA, meaning there is a second value when you split the cell apart at "-")
  if ((is.na(as.numeric(years)[i])) && (!is.na(as.numeric(unlist(strsplit(years[i], "-"))[2])))) {
    # then set minYear to be the first value of "strsplit(years[i], "-"))" - i.e., the first year listed
    minYear <- unlist(strsplit(years[i], "-"))[1]
    # and set maxYear to be the second value of "strsplit(years[i], "-"))" - i.e., the second year listed
    maxYear <- unlist(strsplit(years[i], "-"))[2]
    # and then take the range of minYear to MaxYear
    yearRange <- (minYear:maxYear)
    # and then take the median of yearRange, the median year covered by the range of values
    medianYear[i] <- median(yearRange)
  }
}

# adding medianYear col to dataSet
dataSet$medianYear<-medianYear

#### Excluding data with text in the exclude box and removing McKeown et al., (2015), Gaskin & Happell (2013), and Gaskin & Happell (2013) which are, respectivly, a medical study, and two nursing studies #### 
dataSet$excludeAndJustification[c(6,7,3)] <- "Exclude"
dat <- subset(dataSet, is.na(dataSet$exclude))

### Ordering by median year ###
dat <- dat[order(dat$medianYear),]

################### meta analysis ##################
# Transfrom proportion of successes to number of successes and save as xi (rounding to estimate number of articles, most proportions are correct the ~ 6 decimal or so, so the value will be exact, only McKeown, Andrew, Jennifer S. Gewandter, Michael P. McDermott, Joseph R. Pawlowski, Joseph J. Poli, Daniel Rothstein, John T. Farrar, et al. "Reporting of Sample Size Calculations in Analgesic Clinical Trials: Acttion Systematic Review." The Journal of Pain 16, no. 3 2015 could be a problem, and that's only by 1 success in either direction)
dat$xi<-round(dat$ProportionReportingPA * dat$NumberOfArticlesExamined)

## DOUBLE ARCSINE TRANSFORM of number of articles, which should make the sampling distribution more normal - xi = number of "sucesses", ni = number sampled total (dat$NumberOfArticlesExamined)
# See Miller, J. J. (1978). The Inverse of the Freeman - Tukey Double Arcsine Transformation. The American Statistician, 32(4), 138-138. doi:10.1080/00031305.1978.10479283 for details about the transformation, the formula used is the same, except that there is a multiplicative constant of .5 (accounted for in the transformation and back-transformation)
# The TF double arcsine transformation is used to normalie and variance-stabilise the sampling distribution of proportions
dat <-escalc(measure="PFT",xi=xi,ni=NumberOfArticlesExamined, data=dat)

#### Comparing Fixed effects and restricted maximum-likelihood estimator meta analysis used, 
resRE <- rma(yi, vi, method="REML", data=dat)
predRE <- predict(res, transf=transf.ipft.hm, targs=list(ni=dat$NumberOfArticlesExamined))
predRE

#### USIGN FIXED EFFECTSmeta analysis used, restricted maximum-likelihood estimator
res <- rma(yi, vi, method="FE", data=dat)
pred <- predict(res, transf=transf.ipft.hm, targs=list(ni=dat$NumberOfArticlesExamined))
pred

# back transform, yi = doub arcsin transformed value 
dat.back <- summary(dat, transf=transf.ipft, ni=dat$NumberOfArticlesExamined)

# forest plot with back transformed variables
forest(dat.back$yi, ci.lb=dat.back$ci.lb, ci.ub=dat.back$ci.ub, psize=1,
       xlim=c(-1.6,1.7), alim=c(0,1), ylim=c(-1.1,25.7), refline=NA, digits=3L, xlab="Proportion of studies reporting a power analysis", slab = dat.back$PaperCitation)
addpoly(pred$pred, ci.lb=pred$ci.lb, ci.ub=pred$ci.ub, row=-0.5, digits=3, mlab="FE Model", efac=1.3)
abline(h=0.4)
text(-.8, 25, "Study",               pos=4)
text( 1.5, 25, "Proportion [95% CI]", pos=2)

#### Random effects meta analysis including median year as a moderator, comparing with FEMA below #####
res1 <- rma(yi, vi, method="REML", data=dat, mods = medianYear)
pred1 <- predict(res1, transf=transf.ipft.hm, targs=list(ni=dat$NumberOfArticlesExamined))
pred1

# Ploting proportion by year
# a scatterplot of the data, a line (or rather: curve after the back-transformation) with the estimated average proportion of studies reporting a power analysis by years
# weighting size of dots by standard error
cexs <- 1/dat$vi
cexs <- 1 + (cexs - min(cexs)) / (max(cexs) - min(cexs))
#plotting predicted proportion on year, with points size weighted by standard error
plot(NA, NA, xlab="Year", ylab="Proportion of studies reporting a power analysis", xlim=c(min(dat$medianYear),max(dat$medianYear)), ylim=c(0,1), bty="l")
lines(dat$medianYear, pred1$ci.lb, col="darkgray",  lty = "dashed",  lwd=2)
# Upper and lower CIs
lines(dat$medianYear, pred1$ci.ub, col="darkgray", lty = "dashed", lwd=2)
lines(dat$medianYear, pred1$pred, col="darkgray", lwd=2)
points(dat$medianYear, transf.ipft(dat$yi,dat$NumberOfArticlesExamined), pch=19, cex=cexs)


#### Random effects meta analysis including median year as a moderator #####
res1 <- rma(yi, vi, method="FE", data=dat, mods = medianYear)
pred1 <- predict(res1, transf=transf.ipft.hm, targs=list(ni=dat$NumberOfArticlesExamined))
pred1

# Ploting proportion by year
# a scatterplot of the data, a line (or rather: curve after the back-transformation) with the estimated average proportion of studies reporting a power analysis by years
# weighting size of dots by standard error
cexs <- 1/dat$vi
cexs <- 1 + (cexs - min(cexs)) / (max(cexs) - min(cexs))
#plotting predicted proportion on year, with points size weighted by standard error
plot(NA, NA, xlab="Year", ylab="Proportion of studies reporting a power analysis", xlim=c(min(dat$medianYear),max(dat$medianYear)), ylim=c(0,1), bty="l")
lines(dat$medianYear, pred1$ci.lb, col="darkgray",  lty = "dashed",  lwd=2)
# Upper and lower CIs
lines(dat$medianYear, pred1$ci.ub, col="darkgray", lty = "dashed", lwd=2)
lines(dat$medianYear, pred1$pred, col="darkgray", lwd=2)
points(dat$medianYear, transf.ipft(dat$yi,dat$NumberOfArticlesExamined), pch=19, cex=cexs)
