#Records time  to a user-specified file when sourced. 
#Default hotkey is ctrl-shift-s

#Will prompt for file name on first run. Currently can only write to working directory

#Create file name and row names
if(!exists('filename')){
  
  #Naming setup
  filename<-readline("Enter a file name to write, without extension: ")
  file<-paste(filename, ".csv", sep='');file.create(file)
  rows<-rep(rep(c(seq(from=-3, to=165, by=4), rev(seq(from=-3, to=165, by=4))), 13), each=2); ranges<-rep(c(1:26),each=86)
  coord<-cbind(rows, ranges)
  count=-1
  
  #Function in case you get lost
  coords<-data.frame(cbind(rows, ranges))
  findcount<- function(row, range, io="start"){
    lines<-which(coords$rows==row & coords$ranges==range)
    if(io=='start'){loc<-lines[lines%%2!=0]}
    return(loc)}
  
  library(beepr)
}

#Increment. Super important. Reset if you mess something up
count=count+1

#Records data to file
if (count!=0){ #if it's not the file-creating runthrough...
  
  if(count%%2!=0){
    print(paste('Entering row ', rows[count],', range ',ranges[count], sep=''))
    io<-'s'
    beep(1)
    }else{
      print(paste('Exiting row ', rows[count],', range ',ranges[count], sep=''))
      io<-'e'
      beep(2)}
  
  record<-data.frame(rows[count], ranges[count], io, (Sys.time()))
  write.table(record,file, append=TRUE, row.names=FALSE, col.names=FALSE, sep=',')
}




