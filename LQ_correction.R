source('importCS.R')
check<-importCSdata('test2.dat')[3:1003,]

plot(check$PPF1_Avg, type='l', ylim=c(250,1400))
lines(check$PPF3_Avg, col='red');lines(check$PPF4_Avg, col='green');lines(check$PPF5_Avg, col='blue')

prop<-check$PPF1_Avg/check$PPF3_Avg;prop[prop<0.8]<-NA

par(mfrow=c(2,1), mar=c(1,1,1,1))
plot(prop)
mean(prop, na.rm=TRUE)

plot(check$PPF4_Avg, type='l')

par(mfrow=c(1,1))
plot(prop~check$PPF4_Avg)

agree<-rowMeans(check[7:9])

plot(check$PPF1_Avg~agree)
relate<-lm(check$PPF1_Avg~0+agree)
abline(0,1)
abline(0,relate$coefficients, col='red')
relate$coefficients

plot(check$PPF3_Avg, type='l', ylim=c(250,1400))
lines(check$PPF1_Avg, lty=2, col='red');lines(check$PPF4_Avg, col='green');lines(check$PPF5_Avg, col='blue')
lines(check$PPF1_Avg*(1/relate$coefficients), col='red')




