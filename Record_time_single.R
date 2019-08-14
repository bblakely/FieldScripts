#Records time  to a user-specified file when sourced. 
#Default hotkey is ctrl-shift-s

#Will prompt for file name on first run. Currently can only write to working directory

if(!exists('filename')){
  filename<-readline("Enter a file name to write, without extension: ")
  file<-paste(filename, ".csv", sep='');file.create(file)
  
  rows<-rep(seq(from=1, to=157, by=4), each=26); ranges<-rep(c(1:26, 26:1),length.out=length(rows))
  coord<-cbind(rows, ranges)
  count=0
  
  library(beepr)
  }

print(paste('Entering row ', rows[count],', range ',ranges[count], sep=''))
count=count+1
record<-data.frame(rows[count], ranges[count], (Sys.time()))
write.table(record,file, append=TRUE, row.names=FALSE, col.names=FALSE, sep=',')

beep(1)
