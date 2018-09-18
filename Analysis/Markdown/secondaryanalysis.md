---
title: "Meta-analysis results section"
author: "Felix Singleton Thorn"
date: "25 March 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

## Analysis

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r}
# Setting up data for secondary analysis
#Secondary analysis of the proportion of studies reporting a PA, meta-anaylsis without mods, and meta-analysis with year as a moderator
## Code adapted from http://www.metafor-project.org/doku.php/faq#how_is_the_freeman-tukey_trans ~ Example from this used as a rough model (i.e., not FE, but REML estimation used, but otherwise quite similar), code for last diagram adapted from "http://www.metafor-project.org/doku.php/analyses:viechtbauer2007b"
#library(metafor); library(readxl)

# Importing data - you will need to replace this file path with the file path to the file from https://osf.io/65bsp/download
dataSetOri <- read_excel("PhD/Systematic Reviews/History of Power Estimation Studies/PowerEstimationReviewDataCollection2018.03.05.xlsx", 
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

#### Excluding data with text in the exclude box ####
dat <- subset(dataSet, is.na(dataSet$exclude))

### Ordering by median year ###
dat <- dat[order(dat$medianYear),]

```

## Including Plots

You can also embed plots, for example:

```{r, echo = FALSE}


```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
