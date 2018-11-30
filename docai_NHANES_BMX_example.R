library(survey)
library(tidyverse)

# define non-library(survey) based approach to computing survey mean
svymean.2 <- function(variable,weight.col,df) {
# df is a data frame that contains sampling info and variable name (as a string)
  dat <- df[,which(colnames(df)==variable)]
  idx.to.keep <- which(!is.na(dat))
  weights <- df[,which(colnames(df)==weight)]
  wts <- weights[idx.to.keep]
  sum.wts <- sum(wts)
  dat <- dat[idx.to.keep]
  sum(dat*wts/sum.wts)
}

# load NHANES BMX variables, demographics, and sampling info. from 2013-14 cohort
load('bmx_8.RData')
bigData <- as.data.frame(bigData)

# create survey design object for NHANES
NHANES <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, nest=TRUE, weights=~WTMEC2YR, data=bigData)

## mean
# check a few:
svymean(~BMXBMI,NHANES,na.rm=TRUE)[[1]] # 27.071
svymean.2("BMXBMI","WTMEC2YR",bigData) # 27.071

svymean(~BMXHEAD,NHANES,na.rm=TRUE)[[1]] # 41.669
svymean.2("BMXHEAD","WTMEC2YR",bigData) # 41.669

svymean(~BMXHT,NHANES,na.rm=TRUE)[[1]] # 161.893
svymean.2("BMXHT","WTMEC2YR",bigData) # 161.893

## variance:
svyvar(~BMXBMI,NHANES,na.rm=TRUE)[[1]] # 60.498
svyvar(~BMXHEAD,NHANES,na.rm=TRUE)[[1]] # 6.489
svyvar(~BMXHT,NHANES,na.rm=TRUE)[[1]] # 384.586
