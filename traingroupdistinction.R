setwd('C:/Users/NM5555/Desktop/WestBasin')
getwd()
load('dataWB.Rdata')
coln = colnames(temp)
coln = sub('.9.11.',fixed=T,replacement = '.9.10.11.',coln)
coln = sub('.3.5.',fixed=T,replacement = '.3.4.5.',coln)
coln = sub('.stg.',fixed=T,replacement = '.Stage.',coln)
coln = sub('.Stg.',fixed=T,replacement = '.Stage.',coln)
coln = sub('.St.',fixed=T,replacement = '.Stage.',coln)
coln = sub('RO.',fixed=T,replacement = '',coln)
coln = sub('WB.',fixed=T,replacement = '',coln)
coln = sub('...',fixed=T,replacement = '.',coln)
coln = sub('..',fixed=T,replacement = '.',coln)
coln = sub('Differential.Press',fixed=T,replacement = 'Delta.Pressure',coln)
coln = sub('Delta.Press',fixed=T,replacement = 'Delta.Pressure',coln)
coln = sub('DeltaPressure',fixed=T,replacement = 'Delta.Pressure',coln)
coln = sub('DP',fixed=T,replacement = 'Delta.Pressure',coln)
coln = sub('Filter',fixed=T,replacement = 'Filte',coln)
coln = sub('Permeate',fixed=T,replacement = 'Perm',coln)
coln = sub('Scale.Inhibitor',fixed=T,replacement = 'Antiscalant',coln)
colnames(temp) = coln
#----------------------------------------------------------------------------------
traingp1 = temp[,c("Date",coln[grepl('.1.',coln,fixed=T)|grepl('T1.',coln,fixed=T)])]
qua = colnames(traingp1)
qua = sub('T1.',replacement = '',qua)
qua = sub('Train.1.2.',replacement = '',qua)
qua = sub('.Train.1.',replacement = '',qua)
colnames(traingp1) = qua
traingp1 = cbind(traingp1,'Train'='1')
save(traingp1,file = 'traingp1.Rdata')
#------------------------------------------------------------------------------------
traingp2 = temp[,c("Date",coln[grepl('.2.',coln,fixed=T)|grepl('T2.',coln,fixed=T)])]
qua = colnames(traingp2)
qua = sub('T2.',replacement = '',qua)
qua = sub('Train.1.2.',replacement = '',qua)
qua = sub('.Train.2.',replacement = '',qua)
qua
colnames(traingp2) = qua
traingp2 = cbind(traingp2,'Train'='2')
save(traingp2,file = 'traingp2.Rdata')
#------------------------------------------------------------------------------------
traingp3 = temp[,c("Date",coln[grepl('.3.',coln,fixed=T)|grepl('T3.',coln,fixed=T)])]
qua = colnames(traingp3)
qua = sub('T3.',replacement = '',qua)
qua = sub('Train.3.4.5.',replacement = '',qua)
qua = sub('.Train.3.',replacement = '',qua)
qua
colnames(traingp3) = qua
traingp3 = cbind(traingp3,'Train'='3')
save(traingp3,file = 'traingp3.Rdata')
#-----------------------------------------------------------------------------------
traingp4 = temp[,c("Date",coln[grepl('.4.',coln,fixed=T)|grepl('T4.',coln,fixed=T)])]
qua = colnames(traingp4)
qua = sub('T4.',replacement = '',qua)
qua = sub('Train.3.4.5.',replacement = '',qua)
qua = sub('.Train.4.',replacement = '',qua)
qua
colnames(traingp4) = qua
traingp4 = cbind(traingp4,'Train'='4')
save(traingp4,file = 'traingp4.Rdata')
#----------------------------------------------------------------------------------
traingp5 = temp[,c("Date",coln[grepl('.5.',coln,fixed=T)|grepl('T5.',coln,fixed=T)])]
qua = colnames(traingp5)
qua = sub('T5.',replacement = '',qua)
qua = sub('Train.3.4.5.',replacement = '',qua)
qua = sub('.Train.5.',replacement = '',qua)
qua
colnames(traingp5) = qua
traingp5 = cbind(traingp5,'Train'='5')
save(traingp5,file = 'traingp5.Rdata')
#---------------------------------------------------------------------------------
traingp6 = temp[,c("Date",coln[grepl('.6.',coln,fixed=T)|grepl('T6.',coln,fixed=T)])]
qua = colnames(traingp6)
qua = sub('T6.',replacement = '',qua)
qua = sub('Train.6.7.8.',replacement = '',qua)
qua = sub('.Train.6.',replacement = '',qua)
qua
colnames(traingp6) = qua
traingp6 = cbind(traingp6,'Train'='6')
save(traingp6,file = 'traingp6.Rdata')
#--------------------------------------------------------------------------------
traingp7 = temp[,c("Date",coln[grepl('.7.',coln,fixed=T)|grepl('T7.',coln,fixed=T)])]
qua = colnames(traingp7)
qua = sub('T7.',replacement = '',qua)
qua = sub('Train.6.7.8.',replacement = '',qua)
qua = sub('.Train.7.',replacement = '',qua)
qua
colnames(traingp7) = qua
traingp7 = cbind(traingp7,'Train'='7')
save(traingp7,file = 'traingp7.Rdata')
#----------------------------------------------------------------------------
traingp8 = temp[,c("Date",coln[grepl('.8.',coln,fixed=T)|grepl('T8.',coln,fixed=T)])]
qua = colnames(traingp8)
qua = sub('T8.',replacement = '',qua)
qua = sub('Train.6.7.8.',replacement = '',qua)
qua = sub('.Train.8.',replacement = '',qua)
qua
colnames(traingp8) = qua
traingp8 = cbind(traingp8,'Train'='8')
save(traingp8,file = 'traingp8.Rdata')
#---------------------------------------------------------------------------
traingp9 = temp[,c("Date",coln[grepl('.9.',coln,fixed=T)|grepl('T9.',coln,fixed=T)])]
qua = colnames(traingp9)
qua = sub('T9.',replacement = '',qua)
qua = sub('Train.9.10.11.',replacement = '',qua)
qua = sub('.Train.9.',replacement = '',qua)
qua
colnames(traingp9) = qua
traingp9 = cbind(traingp9,'Train'='9')
save(traingp9,file = 'traingp9.Rdata')
#----------------------------------------------------------------------------
traingp10 = temp[,c("Date",coln[grepl('.10.',coln,fixed=T)|grepl('T10.',coln,fixed=T)])]
qua = colnames(traingp10)
qua = sub('T10.',replacement = '',qua)
qua = sub('Train.9.10.11.',replacement = '',qua)
qua = sub('.Train.10.',replacement = '',qua)
qua = sub('Train.10.',replacement = '',qua)
qua
colnames(traingp10) = qua
traingp10 = cbind(traingp10,'Train'='10')
save(traingp10,file = 'traingp10.Rdata')
#----------------------------------------------------------------------------
traingp11 = temp[,c("Date",coln[grepl('.11.',coln,fixed=T)|grepl('T11.',coln,fixed=T)])]
qua = colnames(traingp11)
qua = sub('T11.',replacement = '',qua)
qua = sub('Train.9.10.11.',replacement = '',qua)
qua = sub('.Train.11.',replacement = '',qua)
qua = sub('Train.11.',replacement = '',qua)
qua
colnames(traingp11) = qua
traingp11 = cbind(traingp11,'Train'='11')
save(traingp11,file = 'traingp11.Rdata')
#--------------------------------------------------------------------------
mij = matrix(ncol=11,nrow=11)
for(i in 1:11){
  for(j in 1:11){
      stri = paste('traingp',i,sep='')
      strj = paste('traingp',j,sep='')
      mij[i,j] = length(setdiff(colnames(eval(parse(text=stri))),colnames(eval(parse(text=strj)))))
      print(c(i,j))
      print(length(setdiff(colnames(eval(parse(text=stri))),colnames(eval(parse(text=strj))))))
      print(setdiff(colnames(eval(parse(text=stri))),colnames(eval(parse(text=strj)))))
  }
}
mij
setdiff(colnames(traingp11),colnames(traingp3))
coln[grepl('Inhibitor',coln)]
