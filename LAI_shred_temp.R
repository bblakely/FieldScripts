#Temporary LAI Check
LAI<-read.table('results 2020-07-22 140403.txt', sep='', header=TRUE, row.names=NULL)

LAI$date<-strptime(substr(LAI$row.names, 1, 10), format="%m_%d_%Y")
LAI$row<-substr(LAI$row.names, 12,12);
LAI$file_name<-as.character(LAI$file_name)
LAI$range<-substr(LAI$file_name, 1,1)

LAI<-LAI[,3:7]

before<-LAI[1:36,]
after<-LAI[37:72,]



library(ggplot2)
library(scales)
rescale(before$lai, to=c(0,1), from=c(0,5))

ggplot(before) +
  aes(x = row, y = range, fill = lai) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "YlGn", direction=2, limits=c(0,5)) +
  theme_minimal()

ggplot(after) +
  aes(x = row, y = range, fill = lai) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "YlGn", direction=2, limits=c(0,5)) +
  theme_minimal()


before$LAI_change<-after$lai-before$lai

ggplot(before) +
  aes(x = row, y = range, fill = LAI_change) +
  geom_tile(size = 1L) +
  scale_fill_distiller(palette = "RdGy", direction=2, limits=c(-0.45, 0.45)) +
  theme_minimal()


#LAI data from Erin
licor<-read.csv("LAI Energy Farm and Bondville - 2016-2020 - 2020.csv")[,1:12]
licor<-licor[which(licor$Records!="1K"),]

licor$plot<-substr(licor$PLOT.ID, 1,3)
licor$rep<-substr(licor$PLOT.ID, 4,4)

subspec<-function(dat, plotname){
  subdat<-licor[licor$plot==plotname,c(1, 7:8, 10:12)]
  subagg<-aggregate(subdat, by=list(subdat$Day.Number), FUN='mean')
  return(subagg)
}

zmc.agg<-subspec(licor, "ZMC" )
plot(zmc.agg$LAI~zmc.agg$Day.Number, type='l', col='yellow', ylim=c(0,8)); abline(v=c(188, 195)); abline(v=193, lty=2)

zmb.agg<-subspec(licor, "ZMB" )
lines(zmb.agg$LAI~zmb.agg$Day.Number, type='l', col='orange')

mxb.agg<-subspec(licor, "MXB" )
lines(mxb.agg$LAI~mxb.agg$Day.Number, type='l', col='blue')

mxc.agg<-subspec(licor, "MXC" )
lines(mxc.agg$LAI~mxc.agg$Day.Number, type='l', col='light blue')

srg.agg<-subspec(licor, "SRG" )
lines(srg.agg$LAI~srg.agg$Day.Number, type='l', col='green')

sre.agg<-subspec(licor, "SRE" )
lines(sre.agg$LAI~sre.agg$Day.Number, type='l', col='forest green')

srw.agg<-subspec(licor, "SRW" )
lines(srw.agg$LAI~srw.agg$Day.Number, type='l', col='light green')


#GGPLOT for nicer plots

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
library(gplots)
library(gridExtra)

licor.ef<-licor[which(licor$plot!="BV1"&licor$plot!="BV2"&licor$plot!="BV3"),]



df.zm <- data_summary(licor[which(licor$plot=="ZMB"|licor$plot=="ZMC"),], varname="LAI", 
                    groupnames=c("plot", "Day.Number"))
#Maize
zm.plot<-ggplot(df.zm, aes(x=Day.Number, y=LAI, group=plot, color=plot)) + 
  #geom_line() +
  #geom_smooth() +
  geom_point()+
  geom_vline(xintercept = 193,linetype="dashed") +
  scale_color_manual(values=col2hex(c('yellow', 'orange'))) +
  geom_errorbar(aes(ymin=LAI-sd, ymax=LAI+sd), width=.2,
                position=position_dodge(1)) +
  ylim(0,8) +
  theme_minimal()

#Miscanthus
df.mxg <- data_summary(licor[which(licor$plot=="MXB"|licor$plot=="MXC"),], varname="LAI", 
                       groupnames=c("plot", "Day.Number"))
mxg.plot<-ggplot(df.mxg, aes(x=Day.Number, y=LAI, group=plot, color=plot)) + 
  #geom_line() +
  #geom_smooth() +
  geom_vline(xintercept = 193, linetype="dashed") +
  geom_point()+
  scale_color_manual(values=col2hex(c('blue', 'light blue'))) +
  geom_errorbar(aes(ymin=LAI-sd, ymax=LAI+sd), width=.2,
                position=position_dodge(1)) +
  ylim(0,8) +
  theme_minimal()


#Sorghum
#combo non-flood and original sorghum
licor.sorg<-licor[which(licor$plot=="SRG"|licor$plot=="SRE"|licor$plot=="SRW"),]
licor.sorg$plot[which(licor.sorg$plot=="SRE")]<-"SRG"
                  
df.sb <- data_summary(licor.sorg, varname="LAI", 
                       groupnames=c("plot", "Day.Number"))

#Add a fake zero so that lowess can work
#df.sb<-rbind(df.sb, c("SRW", 170,0,0));df.sb$LAI<-as.numeric(df.sb$LAI);df.sb$sd<-as.numeric(df.sb$sd);df.sb$Day.Number<-as.numeric(df.sb$Day.Number)

sb.plot<-ggplot(df.sb, aes(x=Day.Number, y=LAI, group=plot, color=plot)) + 
  #geom_line() +
  #geom_smooth() +
  geom_vline(xintercept = 193,linetype="dashed") +
  geom_point()+
  scale_color_manual(values=col2hex(c('forest green', 'gray'))) +
  geom_errorbar(aes(ymin=LAI-sd, ymax=LAI+sd), width=.2,
                position=position_dodge(1)) +
  ylim(0,8) +
  theme_minimal()

#Final plot
grid.arrange(zm.plot, mxg.plot, sb.plot)
