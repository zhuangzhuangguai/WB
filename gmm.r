library('plyr')
library('dplyr')
library('AnomalyDetection')
library('reshape2')
library('ggplot2')
library('zoo')
library('trend')
library('Kendall')
library('trend')
library('cluster')
library('plotly')
library("factoextra")
library("kohonen")
library("Rtsne")
library('data.table')
library('mclust')
###############################
setwd('C:/Users/NM5555/Desktop/WestBasin')
###############################
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
#---------------------------------#
# Une fonction pour le calcule de pente pendant une péridoe de n jours 
# Les résultats sont des valeurs de pente pour les périodes avec au minimum 2 values, sinon retouner 'Na' 
TSPente <- function(x) {
  ## Function to calculat the slope of variable on a period of time
  #
  #Args:
  # x: The dataframe that we want to execut the calculation of the slope
  #Returns:
  # A datafarme of the slope of each time step
  
  
  x <- na.omit(x)
  if (length(x) >= 3) {
    pt <- sens.slope(as.ts(x), conf.level = 0.95)$estimates
  } else {
    pt <- NA
  }
  pt
}
#----------------------------------------#
# Une fonction pour le  de test de Mannkendall pou une péridoe de n jours 
# Les résultats sont des valeurs de p-values pour les périodes avec au minimum 3 values, sinon retouner 'Na' 
TSPvalue <- function(x) {
  ## Function to calculat the slope of variable on a period of time
  #
  #Args:
  # x: The dataframe that we want to execut the calculation of the slope
  #Returns:
  # A datafarme of the slope of each time step
  
  
  x <- na.omit(x)
  if (length(x) >= 3) {
    #pvalue <- MannKendall(x)$sl[1]
    pvalue <- mk.test(as.ts(x), alternative = c("greater"))$p.value
  } else {
    pvalue <- NA
  }
  pvalue
}
#---------------------------------#
###################################

datebegin <- '2015-09-07'
dateend <- '2016-12-31'
DeleteAnomaly <- TRUE
TrainID <- 2
TrainProject <- paste('T', TrainID, sep = "")
#Feed Pressure, Flow Normalized dP, Norm Perm Conductivity, Specific FLUX actual#
#0:400, 
ParamProject <- c('Feed Pressure','Flow Normalized dP','Specific FLUX actual', 'Norm Perm Conductivity')
Seuil.min <- 0
Seuil.max <- 0.5
Seuil <- 0.06
testMK <- F

setoff <- 3
NJourPente <- 7
perDB <- 1
#####################################
load("C:/Users/NM5555/Desktop/WestBasin/DB_WestBasin1.RData")
load("C:/Users/NM5555/Desktop/WestBasin/CleanDate.RData")
testdata <- completeDB[[2]]

colnum <- 1
for(para in ParamProject){
  colnum <- c(colnum, grep(para, colnames(testdata), perl = TRUE, value = FALSE))
}
data <- testdata[, colnum]
coln <- colnames(data)
coln <- gsub("-_-dataWB", "", coln)
coln <- gsub("WB ", "",coln)
coln <- gsub("RO ", "", coln)
coln <- gsub('Train ', 'T', coln)
colnames(data) <- coln
data$Date <- as.Date(data$Date)
attr(data$Date, "tzone") <- "UTC"
data <- filter(data,Date >= datebegin)
data <- filter(data,Date <= dateend)
data <- data[c(1,grep(TrainProject, colnames(data), perl = TRUE, value = FALSE))]

datac <- filter(CleanDataPlot, Train == paste('T', TrainID, sep = ''))
datac <- filter(datac, Date <= dateend)
datac <- filter(datac, Date >= datebegin)
for(j in seq(from = nrow(datac), to = 1)){
  if((datac[j,'Date'] - 24 * 60 * 60) %in% datac$Date || 
     (datac[j,'Date'] - 2 * 24 * 60 * 60) %in% datac$Date ||
     (datac[j,'Date'] - 24 * 60 * 60) %in% datac$Date ){
    datac[j,'Date'] <- NA
  }
}
datac <- na.omit(datac)
if( nrow(datac) >= 1) {
  datedf <- as.data.frame(list())
  for(dy in seq(1:(nrow(datac) - 1 ))) {
    sq <- seq(to = datac$Date[dy + 1], from = datac$Date[dy] + setoff * 60 * 60 * 24, by = 'day')
    Date <- as.Date(sq)
    sq <- as.data.frame(Date)
    datedf <- rbind(datedf, sq)
  }
}
Date <- datedf$Date
dftemp <- filter(data, Date %in% Date)
Date <- dftemp$Date[NJourPente :length(dftemp$Date)]
slope.df <- as.data.frame(rollapply(dftemp[, -1], width = NJourPente * perDB, by = perDB, by.column = T, FUN = TSPente, align = "left"))
pvalue.df <- as.data.frame(rollapply(dftemp[, -1], width = NJourPente * perDB, by = perDB, by.column = T, FUN = TSPvalue, align = "left"))

if(testMK == TRUE) {
  slope.df[pvalue.df > 0.05] <- 0
}
slope.df <- cbind(Date, slope.df)
slope.df[is.na(slope.df)] <- 0
slope.pca <- prcomp(slope.df[, -1], center = T, scale. = T)
slope.mclust <- Mclust(slope.pca$x[, c(1, 2)], G = 4)
slope.tsne <- Rtsne(as.matrix(slope.df[, -1]), check_duplicates = FALSE, pca = FALSE, perplexity=30, theta=0.5, dims=2)
slope.mclust <- Mclust(slope.tsne$Y, G = 4)
plot(slope.mclust)
