# This script creates a new file with the same patterns of missingness from the original dataset,
# replaces all of the numbers (with values of 0 - .99 for numbers in that range, 1 or 0 for values of 1, and numbers from a chi square distribtuion [with 40 df] otherwise)
# This dataset is going to be used to test and design a meta-analysis program to work (i.e., just produce output, I have never used metafor before), and then the actual data will be replace
library(metafor); library(readxl)
 
# import data from data collection (primary file) -  ## ! redirect read_excel() to read file avaliable at: https://osf.io/bne8p/download !
PowerEstimationReviewDataOrig <- read_excel("PhD/Systematic Reviews/History of Power Estimation Studies/PowerEstimationReviewDataCollection2018.03.05.xlsx")

# Setting up matrix with same info as PowerEstimationReviewData - Change to dataframe for main analysis 
PowerEstimationReviewData<-as.matrix(PowerEstimationReviewDataOrig)

# for main analysis just use [dataForTesting <- as.data.frame(PowerEstimationReviewData)] instead of the above

# You also need to ensure that the power vectors are treated as numbers - 
dataForTesting$PowerAtSmallEffectMedian<- as.numeric(as.character(dataForTesting$PowerAtSmallEffectMedian))
dataForTesting$PowerAtMediumEffectMedian<- as.numeric(as.character(dataForTesting$PowerAtMediumEffectMedian))
dataForTesting$PowerAtLargeEffectMedian<- as.numeric(as.character(dataForTesting$PowerAtLargeEffectMedian))

dataForTesting$PowerAtSmallEffectMean<- as.numeric(as.character(dataForTesting$PowerAtSmallEffectMean))
dataForTesting$PowerAtMediumEffectMean<- as.numeric(as.character(dataForTesting$PowerAtMediumEffectMean))
dataForTesting$PowerAtLargeEffectMean<- as.numeric(as.character(dataForTesting$PowerAtLargeEffectMean))

# The above (plus "data cleaning" below) can be used for data cleaning for main results, create dataframe named "dataForTesting" to do cleaning and change colnames (i.e., dataForTesting<-as.data.frame(PowerEstimationReviewData); colnames(dataForTesting)<-PowerEstimationReviewDataOrig[1,])


#

# set up matrix of same dimensions as data set 
dataForTesting<-matrix(NA,nrow(PowerEstimationReviewData), ncol(PowerEstimationReviewData))

######## "test" data generation ########
# index through each row (i) and each col (j) of one line
for(i in 1:nrow(PowerEstimationReviewData)) {
  for (j in 1:ncol(PowerEstimationReviewData)) {
    # if the cell is a numeric,
    if (!is.na(as.numeric(PowerEstimationReviewData[i, j]))) { # this causes "warnings" as NAs are introduced by coercion, which is the point
      # assess whether the value is between 0 and .99
      if ((PowerEstimationReviewData[i, j] < .99) &&
          (PowerEstimationReviewData > 0)) {
        # replace with a value sampled from a uniform distribtution between 0 and 1 with a prob of .5
        dataForTesting[i, j] <- round(runif(1, min = 0, max = 1), 3)
        # else if it = 1, replace with 0 or 1 with equal probability
      } else if (PowerEstimationReviewData[i, j] == 1) {
        dataForTesting[i, j] <- rbinom(n = 1,
                                       size = 1,
                                       prob = .50)
      } else {
        # else if (still only if it is a numeric) replace data with a rand num from chi squared dist with df = 40, (chi square distribution was chosen fairly randomly)
        dataForTesting[i, j] <- round(rchisq(1, 40))}
      } else{
        # else set to equal original value
        dataForTesting[i, j] <- PowerEstimationReviewData[i, j]
      }
    }
  }

#Naming data cols with same name from PowerEstimationReviewDataOrig
colnames(dataForTesting)<-PowerEstimationReviewDataOrig[1,]
dataForTesting[,1] <-  PowerEstimationReviewData[,1]

#replace publication years with years from original data-set
dataForTesting[,5] <- PowerEstimationReviewData[,5]
# The next line replaced years covered with the original, was used to make sure that the median finding tool worked, but not in the simulated data test development procedure
#dataForTesting[,9] <- PowerEstimationReviewData[,9]

# replace years with randomly generated "medians"
for(i in 1:length(dataForTesting[,9])){
  # if the value cannot be convertedd to a direct AND is not NA, then it is a range so we ...
  if(is.na(as.numeric(dataForTesting[i,9]) && (!is.na(dataForTesting[i,9])))){
        # replace the value with firstYear (a random v. from uniform between 1962 - 2017) and ..
        firstYear<-round(runif(1,1962,2017))
        # SecondYear (first year + random V from chisquare dist w/ 2df)
    secondYear<-firstYear+round(rchisq(1,2))
    # unless secondYear is above 2017 or equal to firstYear, in which case we replace it with firstYear+1
      if((secondYear > 2017) || (secondYear<=firstYear)){
        secondYear <- firstYear+1
      }
    # and then we set the value to concatinated "firstYear","-", "SecondYear", no seperation
    dataForTesting[i,9]<-paste(firstYear, "-", secondYear, sep="")
    # if the value was just a year, we replace it with a uniform random value from 1962-2018
      } else if(!is.na(as.numeric(dataForTesting[i,9])))
    dataForTesting[i,9]<-round(runif(1,1962,2018))
}
############# Data cleaning ############ 

### replace year data with median year ###

#extract years vector for easy use
years<-dataForTesting[,9]
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

# converting to dataframe - if the medianYear script is to be re-run, the data must be reimported, as that script doesn't work when the years collumn gets treated as a set of factors
dataForTesting <- as.data.frame(dataForTesting)

# adding medianYear col to dataForTesting
dataForTesting$medianYear <- medianYear

### APA style column of [author (year)] for labeling plots later ###
# setting up collumn
dataForTesting$Label <- rep(NA,length(dataForTesting$Author))

# index through all cells, 
for(i in 1:length(dataForTesting$Author)){
  # if there is author informaiton
  if(!is.na(dataForTesting$Author[i])){
    # save temp as author names split at ","
  temp<-unlist(strsplit(as.character(dataForTesting$Author[i]), ","))
# If there are more than 2 authors
  if(length(temp) > 4) {
    # select every second cell starting with cell 1 - i.e., the last names of authors, removing last name
  temp2<-unlist(strsplit(temp, "and"))[seq(from =1, to = length(temp), by = 2)]
  # select last name
  temp3<-temp2[length(temp2)]
  # and save last names seperated by "," apart from the last family name (i.e., temp3)
  dataForTesting$Label[i]<-paste(temp2[1:(length(temp2)-1)], collapse =",")
  # now save the cell (all family names seperated by ",", apart from the last family name), followed by ", &", "[last family name]", " (", "[year published]", ")"
  dataForTesting$Label[i]<-paste0(dataForTesting$Label[i], ", & ", temp3, " (", dataForTesting$Year[i], ")")
  }  
  # if there are exactly 2 authors, do the same as above, but do not include a "," before the " &"
  else if((length(temp) > 2) && (length(temp) < 5)) {
    temp2<-unlist(strsplit(temp, "and"))[seq(from =1, to = length(temp), by = 2)]
    # select last name
    temp3<-temp2[length(temp2)]
    dataForTesting$Label[i]<-paste(temp2[1:(length(temp2)-1)], collapse =",")
    dataForTesting$Label[i]<-paste0(dataForTesting$Label[i], " & ", temp3, " (", dataForTesting$Year[i], ")")
  }  
  # And finally, if there is only one author, no the same but do not add an "&"
  else if(length(temp) < 3) {    
    temp <- unlist(strsplit(temp, "and"))[seq(from =1, to = length(temp), by = 2)]
    dataForTesting$Label[i]<-paste(temp[1:(length(temp))], collapse =",")
    dataForTesting$Label[i]<-paste0(dataForTesting$Label[i], " ", "(", dataForTesting$Year[i], ")")
  } else {dataForTesting$Label[i]<-NA }
  }
}

### exclude studies ###

# Create a binary for inclusion / inclusion in


### Excluding data with text in the exclude box ###
dat <- subset(dataForTesting, is.na(dataForTesting$exclude))



