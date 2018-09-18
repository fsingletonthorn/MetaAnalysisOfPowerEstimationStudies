# This script creates a new file with the same patterns of missingness from the original dataset,
# replaces all of the numbers (with values of 0 - .99 for numbers in that range, 1 or 0 for values of 1, and numbers from a chi square distribtuion [with 40 df] otherwise)
# This dataset is going to be used to get my meta-analysis program to work

# import data from data collection
PowerEstimationReviewDataOrig <- read_excel("PhD/Systematic Reviews/History of Power Estimation Studies/PowerEstimationReviewDataForProgramTesting2018.01.06.xlsx")
# Setting up matrix with same info as PowerEstimationReviewData - removing dead rows
PowerEstimationReviewData<-as.matrix(PowerEstimationReviewDataOrig)[2:(nrow(PowerEstimationReviewDataOrig)-3),1:ncol(PowerEstimationReviewDataOrig)]

#Naming data cols with same name from PowerEstimationReviewDataOrig
colnames(dataForTesting)<-colnames(PowerEstimationReviewDataOrig)

# The above can be used for data cleaning later

# set up matrix of same dimensions as data set 
dataForTesting<-matrix(NA,nrow(PowerEstimationReviewData), ncol(PowerEstimationReviewData))


# index through each row (i) and each col (j)
for(i in 1:nrow(PowerEstimationReviewData)){ 
  for(j in 1:ncol(PowerEstimationReviewData)){
    # if the cell is a numeric, 
    if(!is.na(as.numeric(PowerEstimationReviewData[i,j]))){
      # assess whether the value is between 0 and .99
      if((PowerEstimationReviewData[i,j] < .99) && (PowerEstimationReviewData>0)){
        # replace with a value sampled from a uniform distribtution between 0 and 1 with a prob of .5
        dataForTesting[i,j]<-round(runif(1, min = 0, max = 1),3)
        # else if it = 1, replace with 0 or 1 with equal probability 
        } else if(PowerEstimationReviewData[i,j] == 1){
          dataForTesting[i,j] <-rbinom(n=1, size=1, prob=.50)
        } else
          # else if (still only if it is a numeric) replace data with a rand num from chi squared dist with df = 40, (chi square distribution was chosen fairly randomly)
          dataForTesting[i,j] <-round(rchisq(1, 40))
    } else{
      # else set to equal original value
      dataForTesting[i,j] <- PowerEstimationReviewData[i,j]
    }
  }
}

# replacing cols that are constant and randomising year variable to plausible range (i.e., 1975-2000)
#Naming data cols with same name from PowerEstimationReviewDataOrig
colnames(dataForTesting)<-colnames(PowerEstimationReviewDataOrig)
dataForTesting[,1] <-  PowerEstimationReviewData[,1]

# replace year data with medians

# replace

