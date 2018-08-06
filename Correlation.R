#### Set the environment and choose the working dictionary
setwd("C:/Users/NM5555/Desktop/WestBasin")
#### Load the necessary packages into environment
library('AnomalyDetection')
library('zoo')
library('data.table')
library('dplyr')
library('compare')
library('dygraphs')
library('xts')
#### Load the data into environment
# 
#Input:
# test: A dataframe of the numeric data
# parametersdf: A dataframes of the parameters
load('DBtest.RData')
load('parametersdf.RData')
parametersdf = as.data.table(parametersdf)
parametersdf[Type == 'Permeate  Flow']$Type = 'Permeate Flow' 
for (train.num in 1:9) {
  tryCatch (
    {
      ccf(test[, paste("Norm Inter stage1 dP.T", train.num, sep=''), drop=F], 
          test[, paste("Stage1 dP.T", train.num, sep=''), drop=F], 
          #lag.max=ceil(maxLagCCF), 
          plot=T, 
          use="pairwise", 
          method="kendall", 
          na.action=na.pass)
    }
  )
}
