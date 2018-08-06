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
    pvalue <- mk.test(as.ts(x), alternative = c("two.sided"))$p.value
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
TrainID <- 4
TrainProject <- paste('T', TrainID, sep = "")
#Feed Pressure, Flow Normalized dP, Norm Perm Conductivity, Specific FLUX actual#
#0:400,?:? ,?:? , 0:0.5
ParamProject <- 'Specific FLUX actual'
Seuil.min <- 0
Seuil.max <- 0.5
Seuil <- 0.06
testMK <- FALSE

setoff <- 2
NJourPente <- 15
perDB <- 1
#####################################
load("C:/Users/NM5555/Desktop/WestBasin/DB_WestBasin1.RData")
load("C:/Users/NM5555/Desktop/WestBasin/CleanDate.RData")
testdata <- completeDB[[2]]

colnum <- grep(ParamProject, colnames(testdata), perl = TRUE, value = FALSE)
colnum <- c(1,colnum)
data <- testdata[, colnum]
data.date<- data[, 1]
data.value <- data[, -1]
data.value[data.value <= Seuil.min] <- NA
data.value[data.value >= Seuil.max] <- NA
data <- cbind(data.date, data.value)
colnames(data)[1] <- 'Date'
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

datalist <- list()
if( nrow(datac) >= 1) {
  for(dy in seq(1:(nrow(datac) - 1 ))) {
    sq <- seq(to = datac$Date[dy + 1], from = datac$Date[dy] + setoff * 60 * 60 * 24, by = 'day')
    sq <- as.Date(sq)
    dftemp <- filter(data, Date %in% sq)
    if(nrow(dftemp) != 0 ) {
      #listdate[[dy]] <- dftemp
      dftempdate <- dftemp[, 'Date']
      if(length(dftempdate) >= NJourPente) {
        dfslope <- as.data.frame(rollapply(dftemp[, -1], width = NJourPente * perDB, by = perDB, by.column = F, FUN = TSPente, align = "left"))
        dfpvalue  <- as.data.frame(rollapply(dftemp[, -1], width = NJourPente * perDB, by = perDB, by.column = F, FUN = TSPvalue, align = "left"))
        if (testMK == TRUE) {
          dfslope[dfpvalue > 0.05] <- 0
        }
        colnames(dfslope) <- "Slope"
        #Add 'Na' for the first 'NJourPente' days in order to have a complet table of each days
        dfslopeSupp = as.data.frame(matrix(NA, nrow = NJourPente - 1,ncol = 1))
        colnames(dfslopeSupp) = colnames(dfslope)
        dfslope = rbind(dfslopeSupp,dfslope)
        colnames(dftemp) <- c('Date','Value')
        dfslope <- cbind(dftemp, dfslope)
        #change the dataframe to matrix if needed
        #dfslope = data.matrix(dfslope)
        df.deltaT <- dfslope
        dT <- (Seuil - df.deltaT$Value)/df.deltaT$Slope
        df.deltaT <- cbind(df.deltaT, dT)
        datalist[[dy]] <- df.deltaT
      }
    }
  }
}   
cycle <- 1
ggplot(data = datalist[[cycle]])+
  geom_line(aes(x = Date, y = Value))+
  geom_point(aes(x = Date, y = Value))+
  geom_point(aes(x = Date, y = (dT)/4000), shape = 0, fill = "blue", color = "darkred")+
  scale_y_continuous(
    name = expression("Value"), 
    sec.axis = sec_axis(~ .*4000   , name = "dT"), 
    limits = c(-0.02, 0.1))+
  geom_hline(yintercept = Seuil, color = "red")+
  geom_hline(yintercept = 0.001, color = "green")


  
