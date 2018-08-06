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
########################
setwd('C:/Users/NM5555/Desktop/WestBasin')
datebegin <- '2015-09-07'
dateend <- '2016-12-31'
DeleteAnomaly <- TRUE
########################
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

#########################################
paracluster <- function(data, ngroups) {
  pam(data, k = ngroups)$cluster
}

########################
load("C:/Users/NM5555/Desktop/WestBasin/DB_WestBasin1.RData")
load("C:/Users/NM5555/Desktop/WestBasin/CleanDate.RData")
testdata <- completeDB[[2]]
#Feed Pressure -> FP
FP <- grep("Feed Pressure", colnames(testdata), perl = TRUE, value = FALSE)
FP <- c(1,FP)
FPdata <- testdata[, FP]
FPdate <- FPdata[, 1]
FPdata <- FPdata[, -1]
FPdata[FPdata <= 0] <- NA
FPdata[FPdata >= 400] <- NA
FPdata <- cbind(FPdate, FPdata)
colnames(FPdata)[1] <- 'Date'
#Flow Normalized dP -> DP
DP <- grep("Flow Normalized dP", colnames(testdata), perl = TRUE, value = FALSE)
DP <- c(1,DP)
DPdata <- testdata[, DP]
DPdate <- DPdata[, 1]
DPdata <- DPdata[, -1]
DPdata[DPdata <= 0] <- NA
DPdata[DPdata >= 100] <- NA
DPdata <- cbind(DPdate, DPdata)
colnames(DPdata)[1] <- 'Date'
#Norm Perm Conductivity -> PC
PC <- grep("Norm Perm Conductivity", colnames(testdata), perl = TRUE, value = FALSE)
PC <- c(1,PC)
PCdata <- testdata[, PC]
PCdate <- PCdata[, 1]
PCdata <- PCdata[, -1]
PCdata[PCdata <= 0] <- NA
#PCdata[PCdata >= 300] <- NA
PCdata <- cbind(PCdate, PCdata)
colnames(PCdata)[1] <- 'Date'
#Specific FLUX actual -> SPA
SPA <- grep("Specific FLUX actual", colnames(testdata), perl = TRUE, value = FALSE)
SPA <- c(1,SPA)
SPAdata <- testdata[, SPA]
SPAdate <- SPAdata[, 1]
SPAdata <- SPAdata[, -1]
SPAdata[SPAdata <= 0] <- NA
SPAdata[SPAdata >= 0.5] <- NA
SPAdata <- cbind(SPAdate, SPAdata)
colnames(SPAdata)[1] <- 'Date'
#Norm Inter-stage1 dP -> S1DP
S1DP <- grep("Norm Inter-stage1 dP", colnames(testdata), perl = TRUE, value = FALSE)
S1DP <- c(1,S1DP)
S1DPdata <- testdata[, S1DP]
S1DPdate <- S1DPdata[, 1]
S1DPdata <- S1DPdata[, -1]
S1DPdata[S1DPdata <= 0] <- NA
S1DPdata[S1DPdata >= 200] <- NA
S1DPdata <- cbind(S1DPdate, S1DPdata)
colnames(S1DPdata)[1] <- 'Date'
#Norm Inter-stage2 dP -> S2DP
S2DP <- grep("Norm Inter-stage2 dP", colnames(testdata), perl = TRUE, value = FALSE)
S2DP <- c(1,S2DP)
S2DPdata <- testdata[, S2DP]
S2DPdate <- S2DPdata[, 1]
S2DPdata <- S2DPdata[, -1]
S2DPdata[S2DPdata <= 0] <- NA
S2DPdata[S2DPdata >= 200] <- NA
S2DPdata <- cbind(S2DPdate, S2DPdata)
colnames(S2DPdata)[1] <- 'Date'

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
data$Date <- as.Date(data$Date)
attr(data$Date, "tzone") <- "UTC"
data <- filter(data,Date >= datebegin)
data <- filter(data,Date <= dateend)

NJourPente <- 15 
perDB <- 1
TrainSlopeCleaned15 <- list()
for(i in seq(1:11)) {
  col <- grep(paste('T', i, ' ', sep = ''), colnames(data), perl = TRUE, value = TRUE)
  slopedata <- data[,c('Date', col)]
  if (DeleteAnomaly) {
    for( j in 2:length(slopedata)) {
      if(table(is.na(slopedata[, j]))['TRUE'] < length(slopedata[, j])){
        slopedata[, j] <- DeleteAnomalies(slopedata[, c(1, j)])[, 2]
      }
      #plotdata[, j] <- DeleteAnomalies(plotdata[, c(1, j)])[, 2]
    }
  }
  #slopedata[, 2] <- as.numeric(smooth(slopedata[, 2]))
  
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
  
  #listdate <- list()
  slopelist <- list()
  setoff <- 3
  if( nrow(datac) >= 1) {
    for(dy in seq(1:(nrow(datac) - 1 ))) {
    sq <- seq(to = datac$Date[dy + 1], from = datac$Date[dy] + setoff * 60 * 60 * 24, by = 'day')
    sq <- as.Date(sq)
    dftemp <- filter(slopedata, Date %in% sq)
    if(nrow(dftemp) != 0 ) {
    #listdate[[dy]] <- dftemp
    dftempdate <- dftemp[, 'Date']
    if(length(dftempdate) >= NJourPente) {
      dfslope <- rollapply(dftemp[, -1], width = NJourPente * perDB, by = perDB, by.column = TRUE, FUN = TSPente, align = "left")
    #Add 'Na' for the first 'NJourPente' days in order to have a complet table of each days
    dfslopeSupp = as.data.frame(matrix(NA, nrow = NJourPente - 1,ncol = ncol(dfslope)))
    colnames(dfslopeSupp) = colnames(dfslope)
    dfslope = rbind(dfslopeSupp,dfslope)
    dfslope <- cbind(dftempdate, dfslope)
    colnames(dfslope)[1] <- 'Date'
    #change the dataframe to matrix if needed
    #dfslope = data.matrix(dfslope)
    slopelist[[dy]] <- dfslope
    }
    }
  }
  }
  TrainSlopeCleaned15[[i]] <- slopelist
  
}
save(TrainSlopeCleaned15, file = 'ListSlopeCleanedTrain15d.RData')



load('ListPvalueTrain15d.RData')
load('ListSlopeTrain15d.RData')
Pvalue <- TrainPvalue15
Slope <- TrainSlope15
pdf("PC plots3d.pdf",width=6, height=5 )
for(TrainID in seq(1:11)){
  dfTrain <- as.data.frame(list())
  if(length(Slope[[TrainID]]) > 0){
    for(d in seq(1:length(Slope[[TrainID]]))) {
    slope <- Slope[[TrainID]][d][[1]][, -1]
    slopeDate <- Slope[[TrainID]][d][[1]][, 1]
    pvalue <- Pvalue[[TrainID]][d][[1]][, -1]
    pvalue[pvalue > 0.05] <- NA
    pvalue[pvalue < 0.05] <- 1
    pvalue[is.na(pvalue)] <- 0
    res = slope * pvalue
    if(length(res) > 0){
     res <- cbind(slopeDate, res)
     colnames(res)[1] <- 'Date'
    }
    dfTrain <- rbind(dfTrain, res)
  }
    nc <- grep('Perm Conductivity', colnames(dfTrain), value = F)
  goodslope<- dfTrain[, nc][which(dfTrain[, nc] > 0)]
  #badslope <- na.omit(dfTrain$`T1 Feed Pressure`)
  if(length(goodslope) > 10){
    d <- density(goodslope)
  #e <- density(badslope)
  plot(d, main = paste('Perm Conductivity', TrainID, sep =''))
  #plot(e)
  }
  }
}
dev.off()  


Pvalue <- TrainPvalue15
Slope <- TrainSlope15
TrainSlopeCleanedValide15 <- TrainSlope15
for(TrainID in seq(1:11)){
  if(length(Slope[[TrainID]]) > 0){
    for(d in seq(1:length(Slope[[TrainID]]))) {
      slope <- Slope[[TrainID]][d][[1]][, -1]
      slopeDate <- Slope[[TrainID]][d][[1]][, 1]
      pvalue <- Pvalue[[TrainID]][d][[1]][, -1]
      pvalue[pvalue > 0.05] <- NA
      pvalue[pvalue < 0.05] <- 1
      res = slope * pvalue
      if(length(res) > 0){
        res <- cbind(slopeDate, res)
        colnames(res)[1] <- 'Date'
      } else{
        res <- as.data.frame(list())
      }
      TrainSlopeValide15[[TrainID]][d][[1]] <- res
    }
  }
}
save(TrainSlopeCleanedValide15, file = 'TrainSlopeCleanedValide15.RData')

#############################################################################
load('TrainSlopeValide15.RData')
load('ListSlopeTrain15d.RData')
load('TrainSlopeValide15.RData')
rawslopevalide <- TrainSlopeValide15
rawslope <- TrainSlope15
ListTrainSlope <- list()
ListTrainSlopeValide <- list()
for(TrainID in seq(1:11)){
  if(length(rawslope[[TrainID]]) > 0){
    TrainSlopeValide <- as.data.frame(list())
    TrainSlope <- as.data.frame(list())
    for(d in seq(1:length(rawslope[[TrainID]]))) {
      TrainSlopeValide <- rbind(TrainSlopeValide, rawslopevalide[[TrainID]][d][[1]])
      TrainSlope <- rbind(TrainSlope, rawslope[[TrainID]][d][[1]]) 
    }
    ListTrainSlopeValide[[TrainID]] <- TrainSlopeValide
    ListTrainSlope[[TrainID]] <- TrainSlope
  }
}

##########################################################################
ngroups <- 4
DeleteAnomaly <- T
pdf("PenteClustering15d(noclusteringwithpara).pdf", width = 15, height = 6 )
for (trainID in c(1, 2,3, 4, 5, 9, 10, 11)) {
  clusterdata <- ListTrainSlopeValide[[trainID]]
  clusterdata[is.na(clusterdata)] <- 0
  clusterdata <- na.omit(clusterdata[, c(-6, -7)])
  datetemp <- clusterdata$Date
  pamClust <- pam(dist(clusterdata[, -1]), k = ngroups, cluster.only = F)
  groups <- pamClust$cluster
  #groups <- pam(apply(clusterdata[, -1], 2, FUN =  paracluster, ngroups = 2), k = 4)$cluster
  colorClust <- scales::hue_pal()(ngroups)
  
  trainIDT = paste('T', trainID, sep = '')
  colNum <- grep(paste(trainIDT, '', sep = ''), colnames(data), perl = TRUE, value = FALSE)
  traindata <- data[c(1, colNum)]
  traindatacluster <- na.omit(traindata)
  
  
  pamClusttrain <- pam(dist(traindatacluster[, -1]), k = ngroups, cluster.only = F)
  groupstrain <- pamClusttrain$cluster
  traindatacluster <- cbind(traindatacluster, groupstrain)
  colnames(traindatacluster)[length(traindatacluster)] <- 'groups'
  #hiClust <- hclust(dist(clusterdata))
  #groups <- cutree(hiClust, ngroups)
  
  #clusterdata <- clusterdata[, c(-1, -6, -7)]
  #clusterdata.som <- som(scale(clusterdata), grid = somgrid(2, 1, "rectangular"))
  #plot(clusterdata.som, type = "mapping", pchs = 20, main = "Mapping Type SOM")
  #plot(clusterdata.som, main = "Default SOM Plot")
  
  datac <- filter(CleanDataPlot, Train == paste('T', trainID, sep = ''))
  #datac <- filter(datac, Date <= dateend)
  #datac <- filter(datac, Date >= datebegin)
  for(j in seq(from = nrow(datac), to = 1)){
    if((datac[j,'Date'] - 24 * 60 * 60) %in% datac$Date || 
       (datac[j,'Date'] - 2 * 24 * 60 * 60) %in% datac$Date ||
       (datac[j,'Date'] - 24 * 60 * 60) %in% datac$Date ){
      datac[j,'Date'] <- NA
    }
  }
  
  #plotdata <- ListTrainSlope[[trainID]]
  plotdata <- data[c('Date', grep(paste('T', trainID, ' ', sep = ''), colnames(data), value = T))]
  plotdata <- filter(plotdata, Date %in% clusterdata$Date)
  if (DeleteAnomaly) {
    for( j in 2:length(plotdata)) {
      if(table(is.na(plotdata[, j]))['TRUE'] < length(plotdata[, j])){
      plotdata[, j] <- DeleteAnomalies(plotdata[, c(1, j)])[, 2]
      #plotdata[, j] <- DeleteAnomalies(plotdata[, c(1, j)])[, 2]
      }
    }
  }
  plotdata <- cbind(plotdata, groups)
  #plotdata <- plotdata[, c(1, 3, 8)]
  plotdata <- melt(plotdata, id=c("Date", "groups"))
  
  p <- ggplot(data = plotdata, aes(x = Date, y = value)) +
    geom_point(aes( alpha = 0.1, group = plotdata$groups, col = colorClust[plotdata$groups])) +
    geom_line(color = 'gray34') +
    theme_bw() + 
    facet_grid(variable~., scales="free") +
    ggtitle(paste("Clustering of slope for T", trainID, sep = '')) +
    geom_vline(data = datac, aes(xintercept = as.Date(Date)),color = 'blue', size = 0.5) +
    theme(strip.text.y = element_text(angle = 90)) +
    theme(legend.position = "none")

  plotdatatrain <- melt(traindatacluster, id=c("Date", "groups"))
  ptrain <- ggplot(data = plotdatatrain, aes(x = Date, y = value)) +
    geom_point(aes( alpha = 0.1, group = plotdatatrain$groups, col = colorClust[plotdatatrain$groups])) +
    geom_line(color = 'gray34') +
    theme_bw() + 
    facet_grid(variable~., scales="free") +
    ggtitle(paste("Clustering of variable for T", trainID, sep = '')) +
    geom_vline(data = datac, aes(xintercept = as.Date(Date)),color = 'blue', size = 0.5) +
    theme(strip.text.y = element_text(angle = 90)) +
    theme(legend.position = "none")
  
  
  PCDB <- prcomp(clusterdata[, -1], center = T, scale. = T)
  PCDBtrain <- prcomp(traindatacluster[, -1], center = T, scale. = T)
  
  q <- fviz_pca_ind(PCDB, geom = "point", habillage = groups) +
             #scale_color_gradient2(low = "green", mid = "blue", high = "red", midpoint = 0.5, space = "Lab", guide = guide_legend(direction = "vertical"))+
             #geom_text(aes(label = plotDBMA['Date']), hjust = 0, vjust = 0)+
             labs(title = paste('Clustering of slope for T', trainID, sep = '')) +
             theme_minimal( )
  
  qtrain <- fviz_pca_ind(PCDBtrain, geom = "point", col.ind = traindatacluster$Date) +
    #scale_color_gradient(as.numeric(traindatacluster$Date), low = "green",  high = "red", space = "Lab", guide = guide_legend(direction = "vertical"))+
    #geom_text(aes(label = plotDBMA['Date']), hjust = 0, vjust = 0)+
    labs(title = paste('Clustering of variable for T', trainID, sep = '')) +
    theme_minimal( )
  fviz_eig(PCDBtrain)+labs(title=paste('Scree plot of variable for ',trainID,sep=''))
  
  
  tsne <- Rtsne(as.matrix(clusterdata[, -1]), check_duplicates = FALSE, pca = FALSE, perplexity=30, theta=0.5, dims=2)
  ggplot(data = as.data.frame(tsne$Y)) +
    geom_point(aes(x = V1, y = V2, alpha = 0.1,colour = clusterdata$Date, shape = as.factor(groups)))+
    scale_color_gradient(
      low="green", high="blue", 
      breaks = as.integer(as.Date(c('2015-09-09', '2015-12-31', '2016-06-01','2016-12-31'))),
      labels = as.Date(c('2015-09-09', '2015-12-31', '2016-06-01','2016-12-31')))+
    labs(colour = 'Date')+
    labs(shape = 'Group')
  
  
  tsnetrain <- Rtsne(as.matrix(traindatacluster[, -1]), check_duplicates = FALSE, pca = FALSE, perplexity=30, theta=0.5, dims=2)
  ggplot(data = as.data.frame(tsnetrain$Y)) +
    geom_point(aes(x = V1, y = V2, alpha = 0.1,colour = traindatacluster$Date, shape = as.factor(groupstrain)))+
    scale_color_gradient(
      low="green", high="blue", 
      breaks = as.integer(as.Date(c('2015-09-09', '2015-12-31', '2016-06-01','2016-12-31'))),
      labels = as.Date(c('2015-09-09', '2015-12-31', '2016-06-01','2016-12-31')))+
    labs(colour = 'Date')+
    labs(shape = 'Group')
  
  ggplot(data = as.data.frame(tsnetrain$Y)) +
    geom_point(aes(x = V1, y = V2, alpha = 0.1,colour = colorClust[traindatacluster$groups]))+
    labs(colour = 'Groups')
  
}
dev.off()
########################################################################################
pdf("Biofouling15d.pdf", width = 15, height = 6 )
for (trainID in c(1, 2,3, 4, 5, 9, 10, 11)) {
  sfdata <- ListTrainSlope[[trainID]]
  reddata <- na.omit(sfdata[sfdata[, grep('Perm Conductivity', colnames(sfdata), value = F)] <= 0, ]$Date)
  
  
  datac <- filter(CleanDataPlot, Train == paste('T', trainID, sep = ''))
  #datac <- filter(datac, Date <= dateend)
  #datac <- filter(datac, Date >= datebegin)
  for(j in seq(from = nrow(datac), to = 1)){
    if((datac[j,'Date'] - 24 * 60 * 60) %in% datac$Date || 
       (datac[j,'Date'] - 2 * 24 * 60 * 60) %in% datac$Date ||
       (datac[j,'Date'] - 24 * 60 * 60) %in% datac$Date ){
      datac[j,'Date'] <- NA
    }
  }
  
  #plotdata <- ListTrainSlope[[trainID]]
  plotdata <- data[c('Date', grep(paste('T', trainID, ' ', sep = ''), colnames(data), value = T))]
  plotdata <- filter(plotdata, Date %in% sfdata$Date)
  if (DeleteAnomaly) {
    for( j in 2:length(plotdata)) {
      
      plotdata[, j] <- DeleteAnomalies(plotdata[, c(1, j)])[, 2]
      #plotdata[, j] <- DeleteAnomalies(plotdata[, c(1, j)])[, 2]
    }
  }
  
  groups <- rep(1, nrow(plotdata))
  groups[which(plotdata$Date %in% reddata)] <- 2
  
  plotdata <- cbind(plotdata, groups)
  #plotdata <- plotdata[, c(1, 3, 8)]
  plotdata <- melt(plotdata, id=c("Date", "groups"))
  
  p <- ggplot(data = plotdata, aes(x = Date, y = value)) +
    geom_point(aes( alpha = 0.1, group = plotdata$groups, col = colorClust[plotdata$groups])) +
    geom_line(color = 'gray34') +
    theme_bw() + 
    facet_grid(variable~., scales="free") +
    ggtitle(paste("Clustering of slope for T", trainID, sep = '')) +
    geom_vline(data = datac, aes(xintercept = as.Date(Date)),color = 'blue', size = 0.5) +
    theme(strip.text.y = element_text(angle = 90)) +
    theme(legend.position = "none")
  
  print(p)
}
dev.off()
#####################################################################
param <- 'Feed Pressure'
for (trainID in c(1, 2,3, 4, 5, 9, 10, 11)) {
  if (! is.null(ListTrainSlopeValide[[trainID]])) {
    sfdata <- ListTrainSlopeValide[[trainID]]
    groups <- pam(na.omit(sfdata[, grep(param, colnames(sfdata), value = F)]), k = 2)$cluster
    colorClust <- scales::hue_pal()(2)
  
    datac <- filter(CleanDataPlot, Train == paste('T', trainID, sep = ''))
    #datac <- filter(datac, Date <= dateend)
    #datac <- filter(datac, Date >= datebegin)
    for(j in seq(from = nrow(datac), to = 1)){
      if((datac[j,'Date'] - 24 * 60 * 60) %in% datac$Date || 
        (datac[j,'Date'] - 2 * 24 * 60 * 60) %in% datac$Date ||
        (datac[j,'Date'] - 24 * 60 * 60) %in% datac$Date ){
        datac[j,'Date'] <- NA
      }
    }
  
    #plotdata <- ListTrainSlope[[trainID]]
    plotdata <- data[c('Date', grep(paste('T', trainID, ' ', sep = ''), colnames(data), value = T))]
    datenona <- na.omit(sfdata[, c(1, grep(param, colnames(sfdata), value = F))])$Date
    plotdata <- plotdata[, c(1, grep(param, colnames(plotdata), value = F))]
    plotdata <- filter(plotdata, Date %in% datenona)
    if (DeleteAnomaly) {
      for( j in 2:length(plotdata)) {
        plotdata[, j] <- DeleteAnomalies(plotdata[, c(1, j)])[, 2]
        #plotdata[, j] <- DeleteAnomalies(plotdata[, c(1, j)])[, 2]
      }
    }
  
    #plotdata <- na.omit(sfdata[, c(1, grep(param, colnames(sfdata), value = F))])
    plotdata <- cbind(plotdata, groups)
    #plotdata <- plotdata[, c(1, 3, 8)]
    plotdata <- melt(plotdata, id=c("Date", "groups"))
  
    p <- ggplot(data = plotdata, aes(x = Date, y = value)) +
      geom_point(aes( alpha = 0.1, group = plotdata$groups, col = colorClust[plotdata$groups])) +
      geom_line(color = 'gray34') +
      theme_bw() + 
      facet_grid(variable~., scales="free") +
      ggtitle(paste("Clustering of slope for T", trainID, sep = '')) +
      geom_vline(data = datac, aes(xintercept = as.Date(Date)),color = 'blue', size = 0.5) +
      theme(strip.text.y = element_text(angle = 90)) +
      theme(legend.position = "none")
  
    print(p)
  }
  
}








