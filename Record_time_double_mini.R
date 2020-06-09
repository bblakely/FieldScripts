#Records time  to a user-specified file when sourced. 
#Default hotkey is ctrl-shift-s

#Will prompt for file name on first run. Currently can only write to working directory

npass<-3 #number of passes in this session
start.direction<-"south.to.north" #direction of first pass. "south.to.north" or "north.to.south"

#Create file name and row names
if(!exists('filename')){
  
  #Naming setup
  dircheck<-askYesNo(paste("Confirm start direction is", start.direction), default=FALSE, prompts = getOption("askYesNo", gettext(c("y", "n", "c"))))
  if(dircheck==TRUE){
  
  filename<-readline("Enter a file name to write, without extension: ")
  file<-paste(filename, ".csv", sep='');file.create(file)
  rows<-rep(5, 22*2*npass) #row is always 5
  if(start.direction=="south.to.north"){ranges<-rep(rep(c(1:22,22:1),each=2),length.out=length(rows))}
  if(start.direction=="north.to.south"){ranges<-rep(rep(c(22:1,1:22),each=2),length.out=length(rows))}
  coord<-cbind(rows, ranges)
  count=-1
  
  #Function in case you get lost
  coords<-data.frame(cbind(rows, ranges))
  findcount<- function(row, range, pass, io="start"){
    lines.opt<-which(coords$rows==row & coords$ranges==range)
    lines<-lines.opt[pass]
    if(io=='start'){loc<-lines[lines%%2!=0]}
    return(loc-1)}
  
  ylibrary(beepr)
  
  }else{print("Script will error out. Change start.direction before proceeding");break()}
  
  }

#Increment. Super important. Reset if you mess something up
count=count+1

#Records data to file
if (count!=0){ #if it's not the file-creating runthrough...
  
  if(count%%2!=0){
    print(paste('Entering row ', rows[count],', range ',ranges[count], sep=''))
    io<-'s'}else{
    print(paste('Exiting row ', rows[count],', range ',ranges[count], sep=''))
    io<-'e'
    beep(1)}
  
  record<-data.frame(rows[count], ranges[count], io, (Sys.time()))
  write.table(record,file, append=TRUE, row.names=FALSE, col.names=FALSE, sep=',')
 
  }



