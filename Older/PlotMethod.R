
source('ImportCS.R')
scans.raw<-importCSdata('CR1000XSeries_Quantum.csv')
scans.dat<-scans.raw[,5:10]


splits<-read.csv('flow.csv', header=FALSE)[2:11,]; splits[,3]<-as.POSIXct(splits[,3])

times<-which(scans.raw$TIMESTAMP >= splits[1,3] & scans.raw$TIMESTAMP <= splits[10,3]+30)


scans.sub<-scans.dat[times,]
scans.prop<-scans.sub/scans.sub$PPF_above_Avg; scans.prop$TS<-scans.raw$TIMESTAMP[times]


splits1<-splits$V3

timesf<-which(scans.prop$TS >= splits[1,3] & scans.prop$TS <= splits[5,3]+10)
timess<-which(scans.prop$TS >= splits[6,3] & scans.prop$TS <= splits[10,3]+10)

par(mfrow=c(2,1))
plot(scans.prop$PPF5_Avg[timesf],type='l', col='red', ylim=c(0,1), xlim=c(0,170))
lines(scans.prop$PPF4_Avg[timesf], type='l', col='orange')
lines(scans.prop$PPF3_Avg[timesf], type='l', col='green')
lines(scans.prop$PPF2_Avg[timesf], type='l', col='blue')
for(i in 1:5){abline(v=which(scans.prop$TS==splits1[i]))}

plot(scans.prop$PPF5_Avg[timess],type='l', col='red', ylim=c(0,1),xlim=c(0,170))
lines(scans.prop$PPF4_Avg[timess], type='l', col='orange')
lines(scans.prop$PPF3_Avg[timess], type='l', col='green')
lines(scans.prop$PPF2_Avg[timess], type='l', col='blue')
for(i in 6:10){
  abline(v=which(scans.prop$TS==splits1[i])-which(scans.prop$TS==splits1[6]))
  abline(v=which(scans.prop$TS==splits1[i]+5)-which(scans.prop$TS==splits1[6]))
  }





par(mfrow=c(1,1))
for(i in 1:5){ #
  chunkflow<-which(scans.prop$TS>=splits1[i]-2&scans.prop$TS<=splits1[i]+5)
  plot(scans.prop$PPF5_Avg[chunkflow], type='l', col='red', ylim=c(0,1), main=splits1[i])
  lines(scans.prop$PPF4_Avg[chunkflow], type='l', col='orange')
  lines(scans.prop$PPF3_Avg[chunkflow], type='l', col='green')
  lines(scans.prop$PPF2_Avg[chunkflow], type='l', col='blue')
  
  chunkstop<-which(scans.prop$TS>=splits1[i+5]-2&scans.prop$TS<=splits1[i+5]+5)
  lines(scans.prop$PPF5_Avg[chunkstop], type='l', col='red', ylim=c(0,1), main=splits1[i], lty=2)
  lines(scans.prop$PPF4_Avg[chunkstop], type='l', col='orange', lty=2)
  lines(scans.prop$PPF3_Avg[chunkstop], type='l', col='green', lty=2)
  lines(scans.prop$PPF2_Avg[chunkstop], type='l', col='blue', lty=2)
}





