source('ImportCS.R') #TOA5 importer
setwd('Soilvue')
library('readxl')

dat.sv<-importCSdata('MxG_Con_SoilVUEdata_MG_Con.dat')#('ZM_SoilVUEdata_ZM.dat')#('MxG_SoilVUEdata_MG.dat')#('MxG_Con_SoilVUEdata_MG_Con.dat')#('SB_SoilVUEdata_SB.dat') #Raw data
dat.hp<-read_excel('MiscanthusNoBasalt_2020_L6.xls', sheet=2, skip=2)#read_excel('MiscanthusNoBasalt_2020_L6.xls', sheet=2, skip=2)#read.csv('Sorghum_L6_full.csv',skip=2)

#Make timestamp for hydraprobe data
Y=as.numeric(substr(dat.hp$xlDateTime, 7,11));DOY<-as.numeric(rep(c(1:305),each=48))
H<-as.numeric(substr(dat.hp$xlDateTime, 12,13));M<-as.numeric(substr(dat.hp$xlDateTime, 15,16))
dectime<-H+(M/60);decday<-DOY+(dectime/24)
hp.ts<-data.frame(cbind(Y,DOY,H, M, dectime, decday))

breaktime<-function(timestamp){
  Y<-as.numeric(format(timestamp, "%Y"))
  H<-as.numeric(format(timestamp, "%H"))
  M<-as.numeric(format(timestamp, "%M"))
  S<-as.numeric(format(timestamp, "%S"))
  DOY<-as.numeric(strftime(timestamp, "%j"))
  dectime<-H+(M/60)+(S/3600)
  decday<-DOY+(dectime/24)
  decyr<-Y+(DOY/365)+(dectime/8760)
  ts<-data.frame(cbind(Y,DOY,H, M, S, dectime, decday, decyr))
  return(ts)
}

dat.ts<-breaktime(dat.sv$TIMESTAMP)

col<-rep("black", nrow(dat.sv));col[dat.ts$Y==2020]<-"red"

sub2020<-which(dat.ts$Y==2020)
col<-'black'

#All year####
par(mfrow=c(2,2))
plot(dat.sv$EC_10cm[sub2020]~dat.ts$DOY[sub2020], ylim=c(0,1.5), col=col, main="10", ylab="EC", xlab="Day of Year 2020");#abline(v=c(142,311), col='red') #abline(v=c(142,192,302,311), col='red')
plot(dat.sv$EC_20cm[sub2020]~dat.ts$DOY[sub2020], ylim=c(0,1.5), col=col, main="20", ylab="EC", xlab="Day of Year 2020");#abline(v=c(142,311), col='red') #abline(v=c(142,192,302,311), col='red')
plot(dat.sv$EC_30cm[sub2020]~dat.ts$DOY[sub2020], ylim=c(0,1.5), col=col, main="30", ylab="EC", xlab="Day of Year 2020");#abline(v=c(142,311), col='red') #abline(v=c(142,192,302,311), col='red')
plot(dat.sv$EC_40cm[sub2020]~dat.ts$DOY[sub2020], ylim=c(0,1.5), col=col, main="40", ylab="EC", xlab="Day of Year 2020");#abline(v=c(142,311), col='red') #abline(v=c(142,192,302,311), col='red')
plot(dat.sv$EC_50cm[sub2020]~dat.ts$DOY[sub2020], ylim=c(0,1.5), col=col, main="50", ylab="EC", xlab="Day of Year 2020");#abline(v=c(142,311), col='red') #abline(v=c(142,192,302,311), col='red')
plot(dat.sv$EC_60cm[sub2020]~dat.ts$DOY[sub2020], ylim=c(0,1.5), col=col, main="60", ylab="EC", xlab="Day of Year 2020");#abline(v=c(142,311), col='red') #abline(v=c(142,192,302,311), col='red')
plot(dat.sv$EC_75cm[sub2020]~dat.ts$DOY[sub2020], ylim=c(0,1.5), col=col, main="75", ylab="EC", xlab="Day of Year 2020");#abline(v=c(142,311), col='red') #abline(v=c(142,192,302,311), col='red')
plot(dat.sv$EC_100cm[sub2020]~dat.ts$DOY[sub2020], ylim=c(0,1.5), col=col, main="100", ylab="EC", xlab="Day of Year 2020");#abline(v=c(142,311), col='red') #abline(v=c(142,192,302,311), col='red')


plot(dat.sv$VWC_10cm~dat.ts$DOY, ylim=c(0,1), col=col, main="10")
plot(dat.sv$VWC_20cm~dat.ts$DOY, ylim=c(0,1), col=col, main="20")
plot(dat.sv$VWC_30cm~dat.ts$DOY, ylim=c(0,1), col=col, main="30")
plot(dat.sv$VWC_40cm~dat.ts$DOY, ylim=c(0,1), col=col, main="40")
plot(dat.sv$VWC_50cm~dat.ts$DOY, ylim=c(0,1), col=col, main="50")
plot(dat.sv$VWC_60cm~dat.ts$DOY, ylim=c(0,1), col=col, main="60")
plot(dat.sv$VWC_75cm~dat.ts$DOY, ylim=c(0,1), col=col, main="75")
plot(dat.sv$VWC_100cm~dat.ts$DOY, ylim=c(0,1), col=col, main="100")
#####

doys<-c(1:365)

subset<-which(dat.ts$Y==2020&dat.ts$DOY%in%c(doys))
hp.subset<-which(hp.ts$DOY%in%c(doys))

#Selected week(s)

par(mfrow=c(2,1), mar=c(4,4,3,1))
col<-rep("blue", length=length(subset)); col[dat.sv$EC_10cm[subset]==0]<-"red"
plot(dat.sv$EC_10cm[subset]~dat.ts$decday[subset], ylim=c(0,0.5), main="10", pch='.', xlab="day of year 2020", ylab="VWC")
points(dat.sv$VWC_10cm[subset]~dat.ts$decday[subset], col=col)
points(dat.hp$Sws_10cma[hp.subset]~hp.ts$decday[hp.subset], col='purple')
points(dat.hp$Sws_10cmb[hp.subset]~hp.ts$decday[hp.subset], col='pink')
#points(dat.hp$Sws_10cmc[hp.subset]~hp.ts$decday[hp.subset], col='light blue')
#abline(v=c(142,192,302,311), col='yellow')
legend(doys[1], 0.5, legend=c("EC", "Soilvue VWC, EC>0","Soilvue VWC, EC=0", "Hydraprobe VWC1", "Hydraprobe VWC2"), pch=c(1), col=c('black', 'blue','red', 'pink', 'purple'), cex=0.5)

col<-rep("blue", length=length(subset)); col[dat.sv$EC_20cm[subset]==0]<-"red"
plot(dat.sv$EC_20cm[subset]~dat.ts$decday[subset], ylim=c(0,0.5), main="20", pch='.', xlab="day of year 2020", ylab="VWC")
points(dat.sv$VWC_20cm[subset]~dat.ts$decday[subset], col=col)
points(dat.hp$Sws_20cma[hp.subset]~hp.ts$decday[hp.subset], col='purple')
points(dat.hp$Sws_20cmb[hp.subset]~hp.ts$decday[hp.subset], col='pink')
#abline(v=c(142,192,302,311), col='yellow')
legend(doys[1], 0.5, legend=c("EC", "Soilvue VWC, EC>0","Soilvue VWC, EC=0", "Hydraprobe VWC1", "Hydraprobe VWC2"), pch=c(1), col=c('black', 'blue','red', 'pink', 'purple'), cex=0.5)

col<-rep("blue", length=length(subset)); col[dat.sv$EC_30cm[subset]==0]<-"red"
plot(dat.sv$EC_30cm[subset]~dat.ts$decday[subset], ylim=c(0,1), main="30", pch='.')
points(dat.sv$VWC_30cm[subset]~dat.ts$decday[subset], col=col)
abline(v=c(142,192,302,311), col='yellow')
legend(doys[1], 0.9, legend=c("EC", "Soilvue VWC, EC>0","Soilvue VWC, EC=0", "Hydraprobe VWC1", "Hydraprobe VWC2"), pch=c(1), col=c('black', 'blue','red', 'pink', 'purple'), cex=0.5)

col<-rep("blue", length=length(subset)); col[dat.sv$EC_40cm[subset]==0]<-"red"
plot(dat.sv$EC_40cm[subset]~dat.ts$decday[subset], ylim=c(0,1), main="40", pch='.', xlab="day of year 2020", ylab="VWC")
points(dat.sv$VWC_40cm[subset]~dat.ts$decday[subset], col='blue')
points(dat.hp$Sws_40cma[hp.subset]~hp.ts$decday[hp.subset], col='purple')
points(dat.hp$Sws_40cmb[hp.subset]~hp.ts$decday[hp.subset], col='pink')
points(dat.hp$Sws_50cmc[hp.subset]~hp.ts$decday[hp.subset], col='pink')
#abline(v=c(142,192,302,311), col='yellow')
legend(doys[1], 1, legend=c("EC", "Soilvue VWC, EC>0","Soilvue VWC, EC=0", "Hydraprobe VWC1", "Hydraprobe VWC2"), pch=c(1), col=c('black', 'blue','red', 'pink', 'purple'), cex=0.5)


col<-rep("blue", length=length(subset)); col[dat.sv$EC_50cm[subset]==0]<-"red"
plot(dat.sv$EC_50cm[subset]~dat.ts$decday[subset], ylim=c(0,1),main="50", pch='.', xlab="day of year 2020", ylab="VWC")
points(dat.sv$VWC_50cm[subset]~dat.ts$decday[subset], col=col)
points(dat.hp$Sws_50cma[hp.subset]~hp.ts$decday[hp.subset], col='purple')
points(dat.hp$Sws_50cmb[hp.subset]~hp.ts$decday[hp.subset], col='pink')
#abline(v=c(142,192,302,311), col='red')
legend(doys[1], 1, legend=c("EC", "Soilvue VWC, EC>0","Soilvue VWC, EC=0", "Hydraprobe VWC1", "Hydraprobe VWC2"), pch=c(1), col=c('black', 'blue','red', 'pink', 'purple'), cex=0.5)


col<-rep("blue", length=length(subset)); col[dat.sv$EC_60cm[subset]==0]<-"red"
plot(dat.sv$EC_60cm[subset]~dat.ts$decday[subset], ylim=c(0,1), main="60", pch='.', xlab="day of year 2020", ylab="VWC")
points(dat.sv$VWC_60cm[subset]~dat.ts$decday[subset], col=col)
abline(v=c(142,192,302,311), col='red')
legend(doys[1], 1, legend=c("EC", "Soilvue VWC, EC>0","Soilvue VWC, EC=0", "Hydraprobe VWC1", "Hydraprobe VWC2"), pch=c(1), col=c('black', 'blue','red', 'pink', 'purple'), cex=0.5)


col<-rep("blue", length=length(subset)); col[dat.sv$EC_75cm[subset]==0]<-"red"
plot(dat.sv$EC_75cm[subset]~dat.ts$decday[subset], ylim=c(0,1), main="75", pch='.', xlab="day of year 2020", ylab="VWC")
points(dat.sv$VWC_75cm[subset]~dat.ts$decday[subset], col=col)
points(dat.hp$Sws_75cma[hp.subset]~hp.ts$decday[hp.subset], col='purple')
points(dat.hp$Sws_75cmb[hp.subset]~hp.ts$decday[hp.subset], col='pink')
abline(v=c(142,192,302,311), col='red')
legend(doys[1], 1, legend=c("EC", "Soilvue VWC, EC>0","Soilvue VWC, EC=0", "Hydraprobe VWC1", "Hydraprobe VWC2"), pch=c(1), col=c('black', 'blue','red', 'pink', 'purple'), cex=0.5)


col<-rep("blue", length=length(subset)); col[dat.sv$EC_100cm[subset]==0]<-"red"
plot(dat.sv$EC_100cm[subset]~dat.ts$decday[subset], ylim=c(0,1), main="100", pch='.', xlab="day of year 2020", ylab="VWC")
points(dat.sv$VWC_100cm[subset]~dat.ts$decday[subset], col=col)
abline(v=c(142,192,302,311), col='red')
legend(doys[1], 1, legend=c("EC", "Soilvue VWC, EC>0","Soilvue VWC, EC=0", "Hydraprobe VWC1", "Hydraprobe VWC2"), pch=c(1), col=c('black', 'blue','red', 'pink', 'purple'), cex=0.5)

#
par(mfrow=c(1,1))

dat.sv.10<-dat.sv$VWC_10cm[subset]; dat.sv.10[dat.sv$EC_10cm[subset]==0]<-NA
plot(dat.sv.10~dat.hp$Sws_10cma[hp.subset], main="10", pch='.', xlim=c(0,1), ylim=c(0,1))
abline(0,1)

dat.sv.20<-dat.sv$VWC_20cm[subset]; dat.sv.20[dat.sv$EC_20cm[subset]==0]<-NA
plot(dat.sv.20~dat.hp$Sws_20cma[hp.subset], main="20", pch='.', xlim=c(0,1), ylim=c(0,1))
abline(0,1)

dat.sv.40<-dat.sv$VWC_40cm[subset]; dat.sv.40[dat.sv$EC_40cm[subset]==0]<-NA
plot(dat.sv.40~dat.hp$Sws_40cma[hp.subset], main="40", pch='.', xlim=c(0,1), ylim=c(0,1))
abline(0,1)

dat.sv.50<-dat.sv$VWC_50cm[subset]; dat.sv.50[dat.sv$EC_50cm[subset]==0]<-NA
plot(dat.sv.50~dat.hp$Sws_50cmb[hp.subset], main="50", pch='.', xlim=c(0,1), ylim=c(0,1))
abline(0,1)

dat.sv.75<-dat.sv$VWC_75cm[subset]; dat.sv.75[dat.sv$EC_75cm[subset]==0]<-NA
plot(dat.sv.75~dat.hp$Sws_75cmb[hp.subset], main="75", pch='.', xlim=c(0,1), ylim=c(0,1))
abline(0,1)





