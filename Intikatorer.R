##                         Written by Dafne Ram 2015-02-20                                 ##
##  Extracts data for specific species/indicator from Index-file and calculates indicators ##
##  -> Choose which indicators to run from popup box that comes up when running the code.  ##   
###    First install package "gWidgets" and "reshape2", if they are not installed yet!    ###

rm(list=ls())  #empties the system

#Set working directory /location of Index file
setwd('C:/DAFNE/Svensk Fågeltaxering/Databases and TRIM/Fredrik') 

t<-read.csv("East_AllSpecies_1991-2016.csv", sep=";", dec=",")#read in total Index file

#loads packages
require(gWidgets)
options("guiToolkit"="RGtk2")
library(reshape2)

#list of which species to include in each indicator
Indikator<-list(
  Levandeskog          =c("Tjäder","Järpe","Skogsduva","Gröngöling","Mindre hackspett","Tretåspett","Nötkråka","Lavskrika","Stjärtmes","Svartmes","Tofsmes","Lappmes","Entita","Talltita","Trädkrypare","Domherre"),
  Dödved               =c("Gröngöling","Mindre hackspett","Tretåspett","Entita","Talltita"),
  Lövrikskog           =c("Skogsduva","Gröngöling","Mindre hackspett","Tretåspett","Stjärtmes","Entita","Trädkrypare"),
  Gammalskog           =c("Tjäder","Tretåspett","Lavskrika","Svartmes","Tofsmes","Lappmes","Talltita","Trädkrypare","Domherre"),
  Skogsfåglar          =c("Bergfink","Björktrast","Blåmes","Bofink","Domherre","Dubbeltrast","Entita","Gransångare","Grå flugsnappare","Gråspett","Gröngöling","Grönsiska","Grönsångare","Gärdsmyg","Göktyta","Halsbandsflugsnappare","Härmsångare","Järnsparv","Järpe","Korp","Kungsfågel","Lappmes","Lavskrika","Lövsångare","Mindre flugsnappare","Mindre hackspett","Mindre korsnäbb","Morkulla","Näktergal","Nötkråka","Nötskrika","Nötväcka","Orre","Rödhake","Rödstjärt","Rödvingetrast","Sidensvans","Skogsduva","Skogssnäppa","Spillkråka","Stenknäck","Stjärtmes","Större hackspett","Större korsnäbb","Svarthätta","Svartmes","Svartvit flugsnappare","Talgoxe","Talltita","Taltrast","Tjäder","Tofsmes","Tretåspett","Trädgårdssångare","Trädkrypare","Trädlärka","Trädpiplärka","Videsparv"),
  SkogsfåglarNonInd    =c("Bergfink","Björktrast","Blåmes","Bofink","Dubbeltrast","Gransångare","Grå flugsnappare","Gråspett","Grönsiska","Grönsångare","Gärdsmyg","Göktyta","Halsbandsflugsnappare","Härmsångare","Järnsparv","Korp","Kungsfågel","Lövsångare","Mindre flugsnappare","Mindre korsnäbb","Morkulla","Näktergal","Nötskrika","Nötväcka","Orre","Rödhake","Rödstjärt","Rödvingetrast","Sidensvans","Skogssnäppa","Spillkråka","Stenknäck","Större hackspett","Större korsnäbb","Svarthätta","Svartvit flugsnappare","Talgoxe","Taltrast","Trädgårdssångare","Trädlärka","Trädpiplärka","Videsparv"),
  SkogsfåglarR         =c("Blåmes","Domherre","Entita","Gråspett","Gröngöling","Järpe","Korp","Lappmes","Lavskrika","Mindre hackspett","Mindre korsnäbb","Nötkråka","Nötskrika","Nötväcka","Orre","Sidensvans","Spillkråka","Stenknäck","Stjärtmes","Större hackspett","Större korsnäbb","Svartmes","Talgoxe","Talltita","Tjäder","Tofsmes","Tretåspett","Trädkrypare"),
  SkogsfåglarM         =c("Bergfink","Björktrast","Bofink","Dubbeltrast","Gransångare","Grå flugsnappare","Grönsiska","Grönsångare","Gärdsmyg","Göktyta","Halsbandsflugsnappare","Härmsångare","Järnsparv","Kungsfågel","Lövsångare","Mindre flugsnappare","Morkulla","Näktergal","Rödhake","Rödstjärt","Rödvingetrast","Skogsduva","Skogssnäppa","Svarthätta","Svartvit flugsnappare","Taltrast","Trädgårdssångare","Trädlärka","Trädpiplärka","Videsparv"),
  Odlingslandskap      =c("Tofsvipa","Storspov","Sånglärka","Ladusvala","Stenskvätta","Buskskvätta","Törnsångare","Gulärla","Törnskata","Stare","Hämpling","Gulsparv","Pilfink"),
  Ängsbetesmarker      =c("Tofsvipa","Storspov","Ladusvala","Stenskvätta","Buskskvätta","Törnsångare","Gulärla","Törnskata","Stare","Hämpling","Gulsparv"),
  Småbiotoper          =c("Stenskvätta","Buskskvätta","Törnsångare","Törnskata","Stare","Hämpling","Gulsparv"),
  Kalfjäll             =c("Fjällripa","Ljungpipare","Fjällabb","Stenskvätta","Ängspiplärka","Lappsparv","Snösparv"),
  Fjällskog            =c("Dalripa","Rödvingetrast","Rödstjärt","Blåhake","Lövsångare","Svartvit flugsnappare","Gråsiska","Bergfink"),
  LevandeSjöar         =c("Storlom","Smålom","Skäggdopping","Vigg","Knipa","Småskrake","Storskrake","Fiskgjuse","Sothöna","Drillsnäppa","Fisktärna","Silvertärna","Forsärla"),
  MyllrandevåtmarkerS  =c("Rördrom","Kricka","Sångsvan","Brun kärrhök","Trana","Sothöna","Enkelbeckasin","Grönbena"),
  MyllrandevåtmarkerN  =c("Smålom","Kricka","Bläsand","Sångsvan","Trana","Ljungpipare","Enkelbeckasin","Småspov","Grönbena","Svartsnäppa","Gluttsnäppa","Kärrsnäppa","Brushane","Smalnäbbad simsnäppa"),
  Miljömål16           =c("Tjäder","Järpe","Skogsduva","Gröngöling","Mindre hackspett","Tretåspett","Nötkråka","Lavskrika","Stjärtmes","Svartmes","Tofsmes","Lappmes","Entita","Talltita","Trädkrypare","Domherre","Tofsvipa","Storspov","Sånglärka","Ladusvala","Stenskvätta","Buskskvätta","Törnsångare","Gulärla","Törnskata","Stare","Hämpling","Gulsparv","Pilfink","Fjällripa","Ljungpipare","Fjällabb","Ängspiplärka","Lappsparv","Snösparv","Dalripa","Rödvingetrast","Rödstjärt","Blåhake","Lövsångare","Svartvit flugsnappare","Gråsiska","Bergfink","Storlom","Smålom","Skäggdopping","Vigg","Knipa","Småskrake","Storskrake","Fiskgjuse","Sothöna","Drillsnäppa","Fisktärna","Silvertärna","Forsärla","Rördrom","Kricka","Sångsvan","Brun kärrhök","Trana","Enkelbeckasin","Grönbena","Bläsand","Småspov","Svartsnäppa","Gluttsnäppa","Kärrsnäppa","Brushane","Smalnäbbad sim","Ejder","Strandskata","Silltrut","Glada","Havstrut"),                        
  EU_VanligaJordbruk       =c("Tofsvipa","Sånglärka","Ladusvala","Råka","Buskskvätta","Törnsångare","Ängspiplärka","Gulärla","Törnskata","Stare","Hämpling","Gulsparv","Ortolansparv","Pilfink"),
  EU_VanligaSkog           =c("Sparvhök","Skogsduva","Mindre Hackspett","Spillkråka","Nötkråka","Svartmes","Tofsmes","Entita","Talltita","Notväcka","Trädkrypare","Dubbeltrast","Rödstjärt","Gransångare","Grönsångare","Kungsfågel","Svartvit flugsnappare","Trädpiplärka","Grönsiska","Domherre"),
  EU_ÖvrigaVanliga         =c("Ormvråk","Enkelbeckasin","Drillsnäppa","Ringduva","Turkduva","Gök","Tornseglare","Gröngöling","Större hackspett","Göktyta","Trädlärka","Hussvala","Korp","Kråka","Kaja","Skata","Stjärtmes","Talgoxe","Blåmes","Gärdsmyg","Björktrast","Taltrast","Rödvingetrast","Koltrast","Stenskvätta","Näktergal","Rödhake","Gräshoppsångare","Rörsångare","Sävsångare","Härmsångare","Svarthätta","Trädgårdssångare","Ärtsångare","Lövsångare","Grå flugsnappare","Järnsparv","Sädesärla","Grönfink","Gråsiska","Bofink","Bergfink","Sävsparv","Gråsparv","Rosenfink"),  
  Norrbotten_Stann                =c("Dalripa","Fjällripa","Orre","Tjäder","Järpe","Större hackspett","Tretåspett","Spillkråka","Korp","Kråka","Skata","Lavskrika","Talgoxe","Blåmes","Lappmes","Talltita","Trädkrypare","Sidensvans","Grönfink","Gråsiska","Domherre","Tallbit","Mindre korsnäbb","Större korsnäbb","Korsnäbb obestämd","Gulsparv","Gråsparv"),
  Norrbotten_Europaflyttare       =c("Storlom","Smålom","Gräsand","Kricka","Bläsand","Vigg","Knipa","Alfågel","Sjöorre","Småskrake","Storskrake","Sångsvan","Fjällvråk","Stenfalk","Tornfalk","Trana","Tofsvipa","Ljungpipare","Enkelbeckasin","Storspov","Kärrsnäppa","Fiskmås","Dvärgmås","Skrattmås","Ringduva","Sånglärka","Dubbeltrast","Taltrast","Rödvingetrast","Ringtrast","Koltrast","Rödhake","Gransångare","Järnsparv","Ängspiplärka","Sädesärla","Grönsiska","Bofink","Bergfink","Sävsparv","Lappsparv","Snösparv","Björktrast","Kungsfågel"),
  Norrbotten_Långflyttare         =c("Fiskgjuse","Större strandpipare","Småspov","Skogssnäppa","Grönbena","Drillsnäppa","Rödbena","Svartsnäppa","Gluttsnäppa","Mosnäppa","Brushane","Smalnäbbad simsnäppa","Silvertärna","Gök","Tornseglare","Göktyta","Ladusvala","Hussvala","Stenskvätta","Buskskvätta","Rödstjärt","Blåhake","Sävsångare","Trädgårdssångare","Lövsångare","Grå flugsnappare","Svartvit flugsnappare","Trädpiplärka","Gulärla","Videsparv","Fjällabb","Rosenfink"),
  Norrbotten_Riktväxtdjur         =c("Tjäder","Järpe","Tretåspett","Lavskrika","Lappmes","Talltita","Trädkrypare","Domherre","Tofsvipa","Storspov","Sånglärka","Ladusvala","Buskskvätta","Gulsparv","Storlom","Smålom","Vigg","Knipa","Småskrake","Storskrake","Fiskgjuse","Drillsnäppa","Silvertärna","Kricka","Bläsand","Sångsvan","Trana","Ljungpipare","Enkelbeckasin","Småspov","Grönbena","Svartsnäppa","Gluttsnäppa","Kärrsnäppa","Brushane","Smalnäbbad simsnäppa","Fjällripa","Fjällabb","Stenskvätta","Ängspiplärka","Lappsparv","Snösparv","Dalripa","Rödvingetrast","Rödstjärt","Blåhake","Lövsångare","Gråsiska","Bergfink"),
  Norrbotten_Rödlistade           =c("Smålom","Fjällvråk","Brushane","Storspov","Drillsnäppa","Tornseglare","Göktyta","Tretåspett","Sånglärka","Lappmes","Lavskrika","Tallbit","Videsparv","Rosenfink"),
  Norrbotten_FDbilaga1            =c("Smålom","Storlom","Sångsvan","Fiskgjuse","Järpe","Orre","Tjäder","Trana","Ljungpipare","Brushane","Grönbena","Smalnäbbad simsnäppa","Dvärgmås","Silvertärna","Spillkråka","Tretåspett","Blåhake","Stenfalk"),
  Norrbotten_Vadare               =c("Tofsvipa","Större strandpipare","Ljungpipare","Enkelbeckasin","Storspov","Småspov","Skogssnäppa","Grönbena","Drillsnäppa","Rödbena","Gluttsnäppa","Svartsnäppa","Mosnäppa","Kärrsnäppa","Brushane","Smalnäbbad simsnäppa"),
  Norrbotten_Skogshöns            =c("Orre","Tjäder","Järpe"),
  Norrbotten_Fjällhöns            =c("Fjällripa","Dalripa"),  
  WestGrazing              =c("Knölsvan","Sångsvan","Bläsand","Kricka","Gräsand","Sothöna"),
  WestFishing              =c("Smålom","Skäggdopping","Storskarv","Storskrake","Småskrake","Salskrake"),
  WestBenthic              =c("Vigg","Brunand","Knipa","Alfågel","Svärta","Sjöorre","Ejder"),
  EastFishing              =c("Smålom","Skäggdopping","Svarthakedopping","Storskarv","Storskrake","Småskrake","Salskrake"),
  EastGrazing              =c("Knölsvan","Sångsvan","Bläsand","Kricka","Gräsand","Stjärtand","Sothöna"),
  EastBenthic              =c("Alfågel","Bergand","Brunand","Ejder","Knipa","Sjöorre","Svärta","Vigg"),
  EastAll                  =c("Knölsvan","Sångsvan","Bläsand","Kricka","Gräsand","Stjärtand","Sothöna","Smålom","Skäggdopping","Svarthakedopping","Storskarv","Storskrake","Småskrake","Salskrake","Alfågel","Bergand","Brunand","Ejder","Knipa","Sjöorre","Svärta","Vigg"),
  WestAll                  =c("Knölsvan","Sångsvan","Bläsand","Kricka","Gräsand","Sothöna","Smålom","Skäggdopping","Storskarv","Storskrake","Småskrake","Salskrake","Vigg","Brunand","Knipa","Alfågel","Svärta","Sjöorre","Ejder")
  )

#popup checkbox to make a selection of indicators
w   <- gbasicdialog("Välja indikatorer", do.buttons = FALSE)  #creates a modal dialog
gp  <- ggroup(container = w)
cbg <- gcheckboxgroup(names(Indikator), container=gp, handler = NULL) #Define checkbox values
gbutton("OK", container = gp, handler = function(h,...) {     #save chosen values in "ind" and close window when clicking OK
  ind <<-Indikator[svalue(cbg)]
  dispose(w) })
visible(w, TRUE) #Show popupwindow

gm_mean = function(x, na.rm=TRUE){exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))} #creates a function for calculating the geometric mean

for(i in 1:length(ind)){  #starts loop, running over every indicator on the list "ind"

#Selects index data for specific species:
Arter <- unlist(ind[i]) #Get list of species from indicator
In <- t[t$arthela %in% Arter,c("art","År","Index","arthela")]#filter data based on species names, and select columns
In <- dcast(In, art+arthela~År, value.var="Index") #change data from long format to wide format (one column per year)
rownames(In) <- In[,2] #Make species  names into row.names
In[,1:2] <- list(NULL) #delete original species number and names columns

#Selects SE data for specific species:
Se <- t[t$arthela %in% Arter,c("art","År","SE","arthela")]#filter data based on species names, and select columns
Se <- dcast(Se, art+arthela~År, value.var="SE")#change data from long format to wide format(one column per year)
rownames(Se) <- Se[,2] #Make species  names into row.names
Se[,1:2] <- list(NULL) #delete original species number and names columns

#Calculate indicators: follows exact same steps as in excel
a<-((1/In)^2)*(Se^2)  
asum<-colSums(a)
b<-(1/nrow(In))^2
c<-(apply(In,2, gm_mean))^2
c1<-apply(In,2, gm_mean)
d<-(c*b*asum)^0.5
g<-1.96*d
e<-c1+g
f<-c1-g

#Make table with resulting values:
out<-rbind(c1,e,f,g,d) #combine all calculated values
out<-melt(out) #make data into long format
out<-dcast(out, Var2~Var1) #make data into wide format listing years in the first column
colnames(out) <- c("År","Index","","","95%ko","SE") # add column names, so now it is the same table as in excel
write.csv2(out, file = (paste(names(ind[i]),".csv", sep='')),row.names=FALSE) #export table as .csv with indicator name as file name
} #close loop


