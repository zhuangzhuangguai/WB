quality.para.name <- c('Magnesium', 'Calcium', 'Silica', 'TOC', 'Alkalinity')
quality.proce <- 'Feed'
TrainID <- '2'
datebegin <- '2015-09-07'
dateend <- '2016-12-31'



colnum <- 1
for(para in quality.para.name){
  colnum <- c(colnum, grep(para, colnames(testdata), perl = TRUE, value = FALSE))
}
quality.data <- testdata[c(1, colnum)]
colnum <- grep(quality.proce, colnames(quality.data), perl = TRUE, value = FALSE)
quality.data <- quality.data[c(1, colnum)]
colnum <- grep(paste('Df_', TrainID, sep = ''), colnames(quality.data), perl = TRUE, value = FALSE)
quality.data <- quality.data[c(1, colnum)]

for(j in 2:ncol(quality.data)){
  startindex <- min(which(!is.na(quality.data[j])))
  endindex <- max(which(!is.na(quality.data[j])))
  res <- approx(x = quality.data$Date[startindex:endindex], 
                y = quality.data[startindex:endindex, j], 
                xout = quality.data$Date[is.na(quality.data[startindex:endindex, j])]
  )
  quality.data[quality.data$Date %in% res$x, j] <- res$y 
}
quality.data$Date <- as.Date(quality.data$Date)
quality.data <- filter(quality.data, Date >= datebegin)
quality.data <- filter(quality.data, Date <= dateend)
