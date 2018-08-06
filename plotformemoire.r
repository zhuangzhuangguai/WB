inddf = data.frame(c(4,5,6,7,8,9,10,11,12), rep(0,9))
colnames(inddf) <- c('num','av_diss')
for(i in c(4,5,6,7,8,9,10,11,12)){
  test <- pam(dist(clusterdata[, -1]), k = i, cluster.only = F)$clusinfo
  ind <- sum(test[,'av_diss'])/i
  inddf[i-3,2] <- ind
}
ggplot(data = inddf, aes(x = num, y = av_diss))+
  geom_point()+
  geom_line(color = 'gray34')+
  ggtitle("av_diss of clusters")
