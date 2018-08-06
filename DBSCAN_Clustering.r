#### Necessary environlent
#
#Agrs:
# DBscaleMA
# DBscalePT
# DateScaleMA
# paramerersdf
setwd("C:/Users/NM5555/Desktop/WestBasin")
load('DBscaleMA.RData')
load('DbscalePT.RData')
load('parametersdf.RData')
load('DateScaleMA.RData')
#### Necessary packages
library('mclust')
library('reshape2')
library('dbscan')
library('ggplot2')
library('plotly')
#### Parameter for the function
#
#CluPrin: Whether to execute the clustering on the four pricipal parameters
#NaOmit: Whether to omit the NA in the data
#Prinpara: The principal parameters
CluPrin <- F
NaOmit <- T
Prinpara <- c("Feed Pressure",
             "Norm Inter stage1 dP",
             "Norm Inter stage2 dP",
             "Norm Inter stage3 dP",
             "Specific Flux actual",
             "Permeate Conductivity"
             )
#### DBSCAN Clustering 
listTrain <- unique(parametersdf$Group)
listPlotly <- list()
for (trainID in listTrain) {
  if (CluPrin) {
    DBscaleMATrain <- DBscaleMA[, which(parametersdf$Type %in% Prinpara & parametersdf$Group == trainID), drop = F]
    DBscalePTTrain <- DBscalePT[, which(parametersdf$Type %in% Prinpara & parametersdf$Group == trainID), drop = F]
  } else {
    DBscaleMATrain <- DBscaleMA[, which(parametersdf$Group == trainID), drop = F]
    DBscalePTTrain <- DBscalePT[, which(parametersdf$Group == trainID), drop = F]
  }
  if (NaOmit) { 
    DBscaleMATrain <- na.omit(DBscaleMATrain)
    DBscalePTTrain <- na.omit(DBscalePTTrain)
  }
  groups <- dbscan(DBscaleMATrain, eps = 0.8)$cluster + 1
  ngroups <- max(groups)
  colorClust <- scales::hue_pal()(ngroups)
  
  groupsPT <- dbscan(DBscalePTTrain, eps = 6)$cluster + 1
  ngroupsPT <- max(groupsPT)
  colorClustPT <- scales::hue_pal()(ngroupsPT)
  
  if (NaOmit) { 
    if (any(na.action(DBscaleMATrain))) {
      plotDBMA <- cbind(as.data.frame(DateScaleMA[-na.action(DBscaleMATrain), ]), DBscaleMATrain, groups)
      colnames(plotDBMA)[1] <- "Date"

      plotDBPT <- cbind(as.data.frame(DateScaleMA[-na.action(DBscalePTTrain), ]), DBscalePTTrain, groupsPT)
      colnames(plotDBPT)[1] <- "Date"
    } else {
      plotDBMA <- cbind(DateScaleMA, DBscaleMATrain, groups)
      
      plotDBPT <- cbind(as.data.frame(DateScaleMA[-na.action(DBscalePTTrain), ]), DBscalePTTrain, groupsPT)
      colnames(plotDBPT)[1] <- "Date"
    }
  } else {
    plotDBMA <- cbind(DateScaleMA, DBscaleMATrain, groups)
    
    plotDBPT <- cbind(DateScaleMA, DBscalePTTrain, groupsPT)
  }
  
  tempParametersdf <- parametersdf[, c(1, 2)]
  colnames(tempParametersdf)[1] <- "variable"
  
  plotnum <- as.numeric(substr(trainID, 2, 3))
  plotDBMA <- melt(plotDBMA, id=c("Date", "groups"))
  dateTemp <- plotDBMA$Date
  plotDBMA <- merge(plotDBMA, tempParametersdf, by=c("variable"))
  plotDBMA$Date <- dateTemp
  plotDBMA$Date <- as.POSIXct(strptime(strftime(plotDBMA$Date, "%Y%m%d"), "%Y%m%d"))#keep the POSIXct format for the purpose of use 'plotly'
  p <- ggplot(data = plotDBMA, aes(x = Date, y= value, group = groups, col = colorClust[plotDBMA$groups]))+
       geom_point() +
       theme_bw() +
       facet_grid(Type~., scales = "free") +
       ggtitle(paste("Clustering of variable for ", trainID, sep='')) +
       theme(strip.text.y = element_text(angle = 90))+
       theme(legend.position = "none")
  listPlotly[[2*plotnum - 1]] <- ggplotly(p, width = 1000, height = 1000)
  
  
  plotDBPT <- melt(plotDBPT, id=c("Date", "groupsPT"))
  dateTemp <- plotDBPT$Date
  plotDBPT <- merge(plotDBPT, tempParametersdf, by=c("variable"))
  plotDBPT$Date <- dateTemp
  plotDBPT$Date <- as.POSIXct(strptime(strftime(plotDBPT$Date, "%Y%m%d"), "%Y%m%d"))#keep the POSIXct format for the purpose of use 'plotly'
  pPT <- ggplot(data = plotDBPT, aes(x = Date, y = value, group = groupsPT, col = colorClust[groupsPT + 1]))+
         geom_point() +
         theme_bw() +
         facet_grid(Type~., scales="free") +
         ggtitle(paste("Clustering of slope for ", trainID, sep='')) +
         theme(strip.text.y = element_text(angle = 90))+
         theme(legend.position="none")
  listPlotly[[2*plotnum]] <- ggplotly(pPT, width = 1000, height = 1000)
}
