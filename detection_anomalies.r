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

DeleteAnomalies <- function (input.df) {
  ## Function to delete all the anomalies in the input data
  #
  #Agrs:
  #  input.df: The raw data with anomalies
  #Returns:
  #  A dataframe with no anomalies
  rownames(input.df) <- NULL
  if (exists('data.list')) {
   rm('date.list')
  }

  date.list <- as.data.frame(input.df[, "Date"])
  colnames(date.list)[1] <- "Date"
  date.list$Date <- as.POSIXct(substring(strftime(date.list$Date), 1, 10))
  var.list = colnames(input.df)

  for (var.name in var.list[2:length(var.list)]) {
    # Use the package "AnomalydetectionTS to detect the anomalies for the time serie"
    res <- AnomalyDetectionTs(na.omit(input.df[, c("Date", var.name)], na.rm=F), 
                               max_anoms=0.1, direction='both', 
                               plot=TRUE)
    if (length(res$anoms)!= 0) {
      colnames(res$anoms) <- c("Date", var.name)
      tt <- res$anoms
      ttt <- input.df[, c("Date", var.name)]
      tt$Date <- as.POSIXct(tt$Date)
      tt$Date <- substring(strftime(tt$Date), 1, 10)
      ttt$Date <- substring(strftime(ttt$Date), 1, 10)
      t <- ttt[!(ttt$Date %in% tt$Date), ]
      t$Date <- as.POSIXct(t$Date)
      date.list <- merge(date.list, t, by = 'Date', all = T)
    } else {
      ttt <- input.df[, c("Date", var.name)]
      ttt$Date <- substring(strftime(ttt$Date), 1, 10)
      ttt$Date <- as.POSIXct(ttt$Date)
      date.list <- merge(date.list, ttt, by = 'Date', all = T)
    }
  }
  date.list
}


GraphDy <- function(i, parametersdf, plotDB){
  # Function to generate the dygraph for time series
  #
  #Args:
  #  i: The number of the dygraphs
  #  parametersdf: The dataframe of the parameters
  #  plotDB: The dataframe of the numeric data
  #Returns:
  #  A dygraphs for all the parameters
  type.param <- unique(parametersdf$Type)[i]
  ind.sel <- parametersdf$Type == type.param
  if (any(ind.sel)) {
      # transform into proper variable type
    toPlot <- xts(plotDB[, which(ind.sel) + 1, drop = F], 
                  order.by = plotDB[, 1], 
                  tzone = 'UTC')
      # create the dygraph
    dg <- dygraph(toPlot, 
                  width  = 1000, 
                  height = 200, 
                  main   = type.param, 
                  group   = "input.df") %>% 
          dyRangeSelector(height = 15) %>%
          dyLegend(show = "follow") %>% 
          dyUnzoom( ) %>% 
          dyOptions(useDataTimezone = T)
    dg
  }
}

test.type <- unique(parametersdf$Type)
test.type <- which(!grepl("PumpStatus", test.type) & !grepl("Status", test.type))
data.plot <- DeleteAnomalies(test)
## trick to put dygraph in a loop => lapply
res.raw <- lapply(test.type, GraphDy, parametersdf, data.plot)
