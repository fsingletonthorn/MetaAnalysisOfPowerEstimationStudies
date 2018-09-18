# Sim data for meta-analysis of proportion of studies which report a power analysis, and analysis script
## http://www.metafor-project.org/doku.php/faq#how_is_the_freeman-tukey_trans ~ Example from this used as a rough model for analysis script (i.e., not FE, but REML estimation used, but otherwise quite similar)
library(metafor)
library(readxl)

# Importing data - you will need to replace this file path with the file path to the file from https://osf.io/gpvbq/
dataSetOri <- read_excel("PhD/Systematic Reviews/History of Power Estimation Studies/SecondaryAnalysisData2018.03.25.xlsx", 
                         sheet = "Data_prop_reporting_PA")
dataSetOri <-as.data.frame(dataSetOri)

dataSet  <- dataSetOri

###### simulated data generation #######
# When doing analysis (on final data), ignore this up to data cleaning. BUT - RUN THIS SECTION BEFORE RUNNING ANALYSIS SCRIPT UNTILL ANALYSIS SCRIPT IS FINALISED 
# index through each row (i) and each col (j) of one line
for(i in 1:nrow(dataSet)) {
  for (j in 1:ncol(dataSet)) {
    # if the cell is a numeric,
    if (!is.na(as.numeric(dataSet[i, j]))) { # this causes "warnings" as NAs are introduced by coercion, which is the point
      # assess whether the value is between 0 and .99
      if ((dataSet[i, j] < .99) &&
          (dataSet > 0)) {
        # replace with a value sampled from a binomial distribtution with a centre of .5 
        dataSet[i, j] <- ((sum(rbinom(dataSet$NumberOfArticlesExamined[i], 1, .5)))/(dataSet$NumberOfArticlesExamined[i]))
        # else if it = 1, replace with 0 or 1 with equal probability
      } else if (dataSet[i, j] == 1) {
        dataSet[i, j] <- rbinom(n = 1,
                                       size = 1,
                                       prob = .50)
      } else {
        # else if (still only if it is a numeric) replace data with a rand num from chi squared dist with df = 40, (chi square distribution was chosen fairly randomly)
        dataSet[i, j] <- round(rchisq(1, 40))}
    } else{
      # else set to equal original value
      dataSet[i, j] <- dataSet[i, j]
    }
  }
}

# replace years with randomly generated "medians"
for(i in 1:length(dataSet$YearsStudied)){
  # if the value cannot be convertedd to a number AND is not NA, then it is a range so we ...
  if(is.na(as.numeric(dataSet$YearsStudied[i]) && (!is.na(dataSet$YearsStudied[i])))){
    # replace the value with firstYear (a random v. from uniform between 1962 - 2017) and ..
    firstYear<-round(runif(1,1962,2017))
    # SecondYear (first year + random V from chisquare dist w/ 2df)
    secondYear<-firstYear+round(rchisq(1,2))
    # unless secondYear is above 2017 or equal to firstYear, in which case we replace it with firstYear+1
    if((secondYear > 2017) || (secondYear<=firstYear)){
      secondYear <- firstYear+1
    }
    # and then we set the value to concatinated "firstYear","-", "SecondYear", no seperation
    dataSet$YearsStudied[i]<-paste(firstYear, "-", secondYear, sep="")
    # if the value was just a year, we replace it with a uniform random value from 1962-2018
  } else if(!is.na(as.numeric(dataSet$YearsStudied[i])))
    dataSet$YearsStudied[i]<-round(runif(1,1962,2018))
}
############# Data cleaning ############ 

###### replace year data with median year #######
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


#### Excluding data with text in the exclude box ####
dat <- subset(dataSet, is.na(dataSet$exclude))

### Ordering by median year ####

dat <- dat[order(dat$medianYear),]

################### meta analysis ##################
# Transfrom proportion of successes to number of successes and save as xi (rounding to estimate number of articles, most proportions are correct the ~ 6 decimal or so, so the value will be exact, only McKeown, Andrew, Jennifer S. Gewandter, Michael P. McDermott, Joseph R. Pawlowski, Joseph J. Poli, Daniel Rothstein, John T. Farrar, et al. "Reporting of Sample Size Calculations in Analgesic Clinical Trials: Acttion Systematic Review." The Journal of Pain 16, no. 3 2015 could be a problem, and that's only by 1 success in either direction)
dat$xi<-round(dat$ProportionReportingPA * dat$NumberOfArticlesExamined)

## DOUBLE ARCSINE TRANSFORM of number of articles, which should make the sampling distribution more normal - xi = number of "sucesses", ni = number sampled total (dat$NumberOfArticlesExamined)
# See Miller, J. J. (1978). The Inverse of the Freeman - Tukey Double Arcsine Transformation. The American Statistician, 32(4), 138-138. doi:10.1080/00031305.1978.10479283 for details about the transformation, the formula used is the same, except that there is a multiplicative constant of .5 (accounted for in the transformation and back-transformation)
# The TF double arcsine transformation is used to normalie and variance-stabilise the sampling distribution of proportions
dat <-escalc(measure="PFT",xi=xi,ni=NumberOfArticlesExamined, data=dat)

#### Random effects meta analysis used, restricted maximum-likelihood estimator
res <- rma(yi, vi, method="REML", data=dat)
pred <- predict(res, transf=transf.ipft.hm, targs=list(ni=dat$NumberOfArticlesExamined))
pred

# back transform, yi = doub arcsin transformed value 
dat.back <- summary(dat, transf=transf.ipft, ni=dat$NumberOfArticlesExamined)

# forest plot with back transformed variables
forest(dat.back$yi, ci.lb=dat.back$ci.lb, ci.ub=dat.back$ci.ub, psize=1,
       xlim=c(-.8,1.5), alim=c(0,1), ylim=c(-1.1,28.7), refline=NA, digits=3L, xlab="Proportion", slab = dat.back$PaperCitation)
addpoly(pred$pred, ci.lb=pred$ci.lb, ci.ub=pred$ci.ub, row=-0.5, digits=3, mlab="REML Model", efac=1.3)
abline(h=0.4)
text(-.8, 28, "Study",               pos=4)
text( 1.5, 28, "Proportion [95% CI]", pos=2)

#### Random effects meta analysis including median year as a moderator #####
res1 <- rma(yi, vi, method="REML", data=dat, mods = medianYear)
pred1 <- predict(res1, transf=transf.ipft.hm, targs=list(ni=dat$NumberOfArticlesExamined))
pred1

# Ploting proportion by year
# a scatterplot of the data, a line (or rather: curve after the back-transformation) with the estimated average proportion of studies reporting a power analysis by years
# weighting size of dots by standard error
cexs <- 1/dat$vi
cexs <- 1 + (cexs - min(cexs)) / (max(cexs) - min(cexs))
#plotting predicted proportion on year, with points size weighted by standard error
plot(NA, NA, xlab="Year", ylab="Proportion", xlim=c(min(dat$medianYear),max(dat$medianYear)), ylim=c(0,1), bty="l")
lines(dat$medianYear, pred1$ci.lb, col="darkgray",  lty = "dashed",  lwd=2)
# Upper and lower CIs
lines(dat$medianYear, pred1$ci.ub, col="darkgray", lty = "dashed", lwd=2)
lines(dat$medianYear, pred1$pred, col="darkgray", lwd=2)
points(dat$medianYear, transf.ipft(dat$yi,dat$NumberOfArticlesExamined), pch=19, cex=cexs)
