library('plyr')
library('dplyr')
library('AnomalyDetection')
library('reshape2')
library('ggplot2')
########################
datebegin <- '2012-01-01'
dateend <- '2016-12-31'
DeleteAnomaly <- TRUE
########################
slopeCal <- function(datac, data, n, offset){
  dt <- filter(data, Date <= datac - offset * 24 *60 *60) 
  dt <- filter(dt, Date >= datac - ( n + offset - 1) * 24 * 60 * 60)
  Kendall(x = dt$Date, y = dt[,2])
}

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
########################
testdata <- completeDB[[2]]
#Feed Pressure -> FP
FP <- grep("Feed Pressure", colnames(testdata), perl = TRUE, value = FALSE)
FP <- c(1,FP)
FPdata <- testdata[, FP]
FPdata[FPdata <= 120] <- NA
colnames(FPdata)
#Flow Normalized dP -> DP
DP <- grep("Flow Normalized dP", colnames(testdata), perl = TRUE, value = FALSE)
DP <- c(1,DP)
DPdata <- testdata[, DP]
colnames(DPdata)
#Permeate Conductivity -> PC
PC <- grep("Permeate Conductivity", colnames(testdata), perl = TRUE, value = FALSE)
PC <- c(1,PC)
PCdata <- testdata[, PC]
colnames(PCdata)
#Specific FLUX actual -> SPA
SPA <- grep("Specific FLUX actual", colnames(testdata), perl = TRUE, value = FALSE)
SPA <- c(1,SPA)
SPAdata <- testdata[, SPA]
colnames(SPAdata)
#Norm Inter-stage1 dP -> S1DP
S1DP <- grep("Norm Inter-stage1 dP", colnames(testdata), perl = TRUE, value = FALSE)
S1DP <- c(1,S1DP)
S1DPdata <- testdata[, S1DP]
colnames(S1DPdata)
#Norm Inter-stage2 dP -> S2DP
S2DP <- grep("Norm Inter-stage2 dP", colnames(testdata), perl = TRUE, value = FALSE)
S2DP <- c(1,S2DP)
S2DPdata <- testdata[, S2DP]
colnames(S2DPdata)

if(FPdata$Date == DPdata$Date && DPdata$Date == PCdata$Date && PCdata$Date == SPAdata$Date && SPAdata$Date == S1DPdata$Date && S1DPdata$Date == S2DPdata$Date){
  listdata <- list(FPdata,DPdata,PCdata,SPAdata, S1DPdata, S2DPdata)
  data <- join_all(listdata, by = 'Date', type = "full", match = "all")
  coln <- colnames(data)
  coln <- gsub("-_-dataWB", "", coln)
  coln <- gsub("WB ", "",coln)
  coln <- gsub("RO ", "", coln)
  coln <- gsub('Train ', 'T', coln)
  colnames(data) <- coln
}
data <- filter(data,Date >= datebegin)
data <- filter(data,Date <= dateend)

listplot <- list()
for(i in seq(1:11)) {
  col <- grep(paste('T', i, ' ', sep = ''), colnames(data), perl = TRUE, value = TRUE)
  plotdata <- data[,c('Date', col)]
  if (DeleteAnomaly) {
    for( j in 2:length(plotdata)) {
      plotdata[, j] <- DeleteAnomalies(plotdata[, c(1, j)])[, 2]
      #plotdata[, j] <- DeleteAnomalies(plotdata[, c(1, j)])[, 2]
    }
  }
  #plotdata[, 2] <- as.numeric(smooth(plotdata[, 2]))
  plotdata <- melt(plotdata, id=c("Date"))
  
  datac <- filter(CleanDataPlot, Train == paste('T', i, sep = ''))
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
  #datap <- data[extrema(data$T4FP)$minima[,1],]
  listplot[[i]] <-  ggplot(data = plotdata, aes_string(x = 'Date', y = 'value', group = 'variable')) +
    geom_point() +
    geom_line(color = "gray34") +
    geom_vline(data = datac, aes(xintercept = as.Date(Date)),color = 'blue', size = 0.5) +
    labs(title = paste("Plot of Train ", i, sep = '')) +
    theme_bw() +
    facet_grid(variable~., scales = "free")
  
            
}
sapply(datac$Date, FUN = slopeCal, data = data, n = 20, offset = 4)
rollapply(data, width = 20, FUN = Kendall, x = colnames(data)[1], y = colnames(data)[2], by = 1, by.column = FALSE)
