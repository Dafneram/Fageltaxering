##                         Written by Dafne Ram 2015-02-03                                 ##
##                Extracts Indices and Slopes from TRIM .out and .S1 files                 ##
##                 Uses EUROLIST.csv as a source for the species names                     ##
#############################################################################################
##  Put all (non-empty) .S1 and .out files and the EUROLIST.csv in the Temp folder first   ##
##             State the amount of species included in this run after ant=                 ##
#############################################################################################

rm(list=ls())  #empties the system
setwd('C:/Users/Dafne/Desktop/Svensk Fågeltaxering/Databases and TRIM/Temp') #set working directory

ant=98          #Number of species included  


OUT<-matrix(NA, nrow=ant, ncol=2) #creates empty matrix. nrow is number of species included, as defined before
ldf <- list() # creates an empty list
listout <- dir(pattern = "*.out") # creates a list of all the out files in the working directory
for (k in 1:length(listout)){ #starts loop
  ldf[[k]] <- read.table(listout[k], sep=',', dec=',') #read in all .out files

  ROW<-as.character(ldf[[k]][(nrow(ldf[[k]])-2),])  #selects 3rd last row
  OUT[k,]<-c(substr(ROW, 33, 38), substr(ROW, 45, 50)) #extract values from that row and puts them in the matrix 
}
OUT<-as.data.frame(cbind(OUT,substr(listout,1,3))) #add column with species numbers extracted from the filenames
OUT$V1<-as.numeric(as.character(OUT$V1)) #change variables into numeric
OUT$V2<-as.numeric(as.character(OUT$V2))
colnames(OUT) <- c("Slope", "SE", "art") #change column names
EUROLIST<-read.csv("EUROLIST.csv", sep = ",", colClasses="character")[ ,c('art', 'arthela')] #read in the arthela names from EUROLIST
OUT<- merge(OUT, EUROLIST, by="art") #merge data with arthela/speciesnames
OUT<-OUT[,c(1,4,2,3)] #reorder the columns
OUT$art<-as.character(OUT$art) #set "art" as character
write.csv2(OUT, "Slope.csv", row.names=FALSE) #export data as Slope.csv to working directory


S1list <- dir(pattern = "*.S1") # creates a list of all the S1 files in the directory
S1 <- do.call("rbind", lapply(S1list, read.table, sep=',', dec='.')) # read in the files and combine them 
S1 <- S1[ -c(2:12, 14:17,20) ] #take out unused columns
S1<-cbind(S1,substr(S1$V1,6,8)) #take out artnummer from filename
colnames(S1) <- c("Fil", "År", "Index", "SE", "art") #rename columns
S1<- merge(S1, EUROLIST, by="art") #merge data with arthela/speciesnames
write.csv2(S1, "Index.csv", row.names=FALSE) #export data as Index.csv to working directory
