###                        Written by Dafne Ram 2015-02-20                                ###
##  Extracts data for specific species/indicator from Index-file and calculates indicators ##
##  -> Choose which indicators to run from popup box that comes up when running the code   ##
##  -> Choose alternative wheatear, yellow wagtail and/or fjäll data in second popup       ##
###   !First install packages "gWidgets" and "reshape2", if they are not installed yet!   ###

rm(list=ls())  #empties the system

#Set working directory /location of Index file
setwd('C:/Users/Dafne/Desktop/Svensk Fågeltaxering/Databases and TRIM/STD 2002-2014 M') 

#loads packages
require(gWidgets)
options("guiToolkit"="RGtk2")
library(reshape2)

Indikator<-list(
  Levandeskog          =c("Tjäder","Järpe","Skogsduva","Gröngöling","Mindre hackspett","Tretåig hackspett","Nötkråka","Lavskrika","Stjärtmes","Svartmes","Tofsmes","Lappmes","Entita","Talltita","Trädkrypare","Domherre"),
  Dödved               =c("Gröngöling","Mindre hackspett","Tretåspett","Entita","Talltita"),
  Lövrikskog           =c("Skogsduva","Gröngöling","Mindre hackspett","Tretåspett","Stjärtmes","Entita","Trädkrypare"),
  Gammalskog           =c("Tjäder","Tretåig hackspett","Lavskrika","Svartmes","Tofsmes","Lappmes","Talltita","Trädkrypare","Domherre"),
  Odlingslandskap      =c("Tofsvipa","Storspov","Sånglärka","Ladusvala","Stenskvätta","Buskskvätta","Törnsångare","Gulärla","Törnskata","Stare","Hämpling","Gulsparv","Pilfink"),
  Ängsbetesmarker      =c("Tofsvipa","Storspov","Ladusvala","Stenskvätta","Buskskvätta","Törnsångare","Gulärla","Törnskata","Stare","Hämpling","Gulsparv"),
  Småbiotoper          =c("Stenskvätta","Buskskvätta","Törnsångare","Törnskata","Stare","Hämpling","Gulsparv"),
  Kalfjäll             =c("Fjällripa","Ljungpipare","Fjällabb","Stenskvätta","Ängspiplärka","Lappsparv","Snösparv"),
  Fjällskog            =c("Dalripa","Rödvingetrast","Rödstjärt","Blåhake","Lövsångare","Svartvit flugsnappare","Gråsiska","Bergfink"),
  LevandeSjöar         =c("Storlom","Smålom","Skäggdopping","Vigg","Knipa","Småskrake","Storskrake","Fiskgjuse","Sothöna","Drillsnäppa","Fisktärna","Silvertärna","Forsärla"),
  MyllrandevåtmarkerS  =c("Rördrom","Kricka","Sångsvan","Brun kärrhök","Trana","Sothöna","Enkelbeckasin","Grönbena"),
  MyllrandevåtmarkerN  =c("Smålom","Kricka","Bläsand","Sångsvan","Trana","Ljungpipare","Enkelbeckasin","Småspov","Grönbena","Svartsnäppa","Gluttsnäppa","Kärrsnäppa","Brushane","Smalnäbbad simsnäppa"),
  Miljömål16           =c("Tjäder","Järpe","Skogsduva","Gröngöling","Mindre hackspett","Tretåig hackspett","Nötkråka","Lavskrika","Stjärtmes","Svartmes","Tofsmes","Lappmes","Entita","Talltita","Trädkrypare","Domherre","Tofsvipa","Storspov","Sånglärka","Ladusvala","Stenskvätta","Buskskvätta","Törnsångare","Gulärla","Törnskata","Stare","Hämpling","Gulsparv","Pilfink","Fjällripa","Ljungpipare","Fjällabb","Ängspiplärka","Lappsparv","Snösparv","Dalripa","Rödvingetrast","Rödstjärt","Blåhake","Lövsångare","Svartvit flugsnappare","Gråsiska","Bergfink","Storlom","Smålom","Skäggdopping","Vigg","Knipa","Småskrake","Storskrake","Fiskgjuse","Sothöna","Drillsnäppa","Fisktärna","Silvertärna","Forsärla","Rördrom","Kricka","Sångsvan","Brun kärrhök","Trana","Enkelbeckasin","Grönbena","Bläsand","Småspov","Svartsnäppa","Gluttsnäppa","Kärrsnäppa","Brushane","Smalnäbbad sim","Ejder","Strandskata","Silltrut","Glada","Havstrut"),                        
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
  Norrbotten_Fjällhöns            =c("Fjällripa","Dalripa")  
)

#popup checkbox to make a selection of indicators
w   <- gbasicdialog("Välja indikatorer", do.buttons = FALSE, horizontal=FALSE)  #creates a modal dialog
gp  <- ggroup(container = w, horizontal=FALSE)

tmpt   <- gframe("Index fil", container=gp, horizontal=FALSE)
filet  <- gfilebrowse("Välj index fil", container=tmpt, quote = FALSE, filter = list("CSV files"=list(patterns="*.csv")))
addSpace(tmps,4)

tmpi   <- gframe("Indikatorer", container=gp, horizontal=FALSE)
cbg    <- gcheckboxgroup(names(Indikator), columns=2, container=tmpi, handler = NULL) #Define checkbox values
addSpace(tmps,4)
gbutton("OK", container = gp, handler = function(h,...) {     #save chosen values in "ind" and close window when clicking OK
  ind   <<-Indikator[svalue(cbg)]
  t     <<-read.csv(file=paste(basename(svalue(filet))), sep=";", dec=",")
  selst <<- if(!"Odlingslandskap" %in% names(ind)&& !"Ängsbetesmarker" %in% names(ind)&& !"Småbiotoper" %in% names(ind)) TRUE
  selga <<- if(!"Odlingslandskap" %in% names(ind)&& !"Ängsbetesmarker" %in% names(ind)) TRUE
  selfj <<- if(!"Kalfjäll" %in% names(ind)&& !"Fjällskog" %in% names(ind)) TRUE
  dispose(w) })
visible(w, TRUE) #Show popupwindow

#popup checkbox to select which wheatears and wagtails to include
if ("Odlingslandskap" %in% names(ind)|"Ängsbetesmarker" %in% names(ind)|"Småbiotoper" %in% names(ind)|"Kalfjäll" %in% names(ind)|"Fjällskog" %in% names(ind)){
  w  <- gbasicdialog("Välj alternativ data", do.buttons = FALSE)  #creates a modal dialog
  gp <- ggroup(container = w, horizontal=FALSE)
  
  if ("Odlingslandskap" %in% names(ind)|"Ängsbetesmarker" %in% names(ind)|"Småbiotoper" %in% names(ind)){
    tmps    <- gframe("Stenskvätta", container=gp, horizontal=FALSE)
    glabel ("   Odlingslandskap, Ängs-Betesmarker och Småbiotoper   ", container=tmps)
    cbst    <- gcheckbox("Inkludera alla stenskvätta", handler = NULL, container=tmps) #Define checkbox values
    filest  <- gfilebrowse("Välj alternativ fil för stenskvätte-data", container=tmps, quote = FALSE, filter = list("CSV files"=list(patterns="*.csv")))
    addSpace(tmps,4)
  }
  if ("Odlingslandskap" %in% names(ind)|"Ängsbetesmarker" %in% names(ind)){
    tmpg    <- gframe("Gulärla", container=gp, horizontal=FALSE)
    glabel ("Odlingslandskap och Ängs-Betesmarker", container = tmpg)
    cbga    <- gcheckbox("Inkludera alla gulärla", container=tmpg, handler = NULL) #Define checkbox values
    filega  <- gfilebrowse("Välj alternativ fil för gulärle-data", quote = FALSE, filter = list("CSV files"=list(patterns="*.csv")), container = tmpg)
    addSpace(tmpg,4)
  }
  if ("Kalfjäll" %in% names(ind)|"Fjällskog" %in% names(ind)){
    tmpf    <- gframe("Fjällen", container=gp, horizontal=FALSE)
    glabel ("Kalfjäll och Fjällskog", container = tmpf)
    cbfj    <- gcheckbox("Inkludera alla data", container=tmpf, handler = NULL) #Define checkbox values
    filefj  <- gfilebrowse("Välj alternativ fil för fjäll-data", quote = FALSE, filter = list("CSV files"=list(patterns="*.csv")), container = tmpf)
    addSpace(tmpf,4)
  }
  gbutton("OK", container = gp, handler = function(h,...) {     #save chosen values in "sel" and close window when clicking OK
    selst <<- if("Odlingslandskap" %in% names(ind)|"Ängsbetesmarker" %in% names(ind)|"Småbiotoper" %in% names(ind)) svalue(cbst)
    selga <<- if("Odlingslandskap" %in% names(ind)|"Ängsbetesmarker" %in% names(ind)) svalue(cbga) 
    selfj <<- if("Kalfjäll" %in% names(ind)|"Fjällskog" %in% names(ind)) svalue(cbfj) 
    if(selst==FALSE) st    <<-read.csv(file=paste(basename(svalue(filest))), sep=";", dec=",")
    if(selga==FALSE) ga    <<-read.csv(file=paste(basename(svalue(filega))), sep=";", dec=",")
    if(selfj==FALSE) fjäll <<-read.csv(file=paste(basename(svalue(filefj))), sep=";", dec=",")
    dispose(w) })
  visible(w, TRUE) #Show popupwindow
}

gm_mean = function(x, na.rm=TRUE){exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))}

for(i in 1:length(ind)){  
  
  Arter <- unlist(ind[i])
  
  if (selst==FALSE && selga==TRUE && (names(ind[i])=="Odlingslandskap"|names(ind[i])=="Ängsbetesmarker"|names(ind[i])=="Småbiotoper")) { 
    In <- t[t$arthela %in% Arter,c("art","År","Index","arthela")]
    In <- In[!In$arthela == "Stenskvätta",]
    Se <- t[t$arthela %in% Arter,c("art","År","SE","arthela")]                                                      
    Se <- Se[!Se$arthela == "Stenskvätta",]
    
    st<-st[,c(5,2,3,4,6)]
    colnames(st) <- c("art","År","Index","SE","arthela")
    st <- st[st$arthela == "Stenskvätta",]
    
    In <- rbind(In,st[,c(1,2,3,5)])
    Se <- rbind(Se,st[,c(1,2,4,5)])  
    
  } else if (selga==FALSE && selst==TRUE && (names(ind[i])=="Odlingslandskap"|names(ind[i])=="Ängsbetesmarker"|names(ind[i])=="Småbiotoper")) { 
    In <- t[t$arthela %in% Arter,c("art","År","Index","arthela")]
    In <- In[!In$arthela == "Gulärla",]
    Se <- t[t$arthela %in% Arter,c("art","År","SE","arthela")]                                                      
    Se <- Se[!Se$arthela == "Gulärla",]
    
    ga<-ga[,c(5,2,3,4,6)]
    colnames(ga) <- c("art","År","Index","SE","arthela")
    ga <- ga[ga$arthela == "Gulärla",]
    
    In <- rbind(In,ga[,c(1,2,3,5)])
    Se <- rbind(Se,ga[,c(1,2,4,5)])  
    
  } else if (selst==FALSE && selga==FALSE && (names(ind[i])=="Odlingslandskap"|names(ind[i])=="Ängsbetesmarker"|names(ind[i])=="Småbiotoper")){
    In <- t[t$arthela %in% Arter,c("art","År","Index","arthela")]
    In <- In[!In$arthela == "Gulärla" | "Stenskvätta",]
    Se <- t[t$arthela %in% Arter,c("art","År","SE","arthela")]                                                      
    Se <- Se[!Se$arthela == "Gulärla" | "Stenskvätta",]
    
    ga <-ga[,c(5,2,3,4,6)]
    colnames(ga) <- c("art","År","Index","SE","arthela")
    ga <- ga[ga$arthela == "Gulärla",]
    
    st <- st[,c(5,2,3,4,6)]
    colnames(st) <- c("art","År","Index","SE","arthela")
    st <- st[st$arthela == "Stenskvätta",]
    
    In <- rbind(In,ga[,c(1,2,3,5)],st[,c(1,2,3,5)])
    In <- In[In$arthela %in% Arter]
    Se <- rbind(Se,ga[,c(1,2,4,5)],st[,c(1,2,4,5)])
    Se <- Se[Se$arthela %in% Arter] 
    
  } else if (selfj==FALSE && (names(ind[i])=="Kalfjäll"|names(ind[i])=="Fjällskog"))
    In <- fjäll[fjäll$arthela %in% Arter,c("art","År","Index","arthela")] 
  Se <- fjäll[fjäll$arthela %in% Arter,c("art","År","SE","arthela")]    
  
}else{ 
  In <- t[t$arthela %in% Arter,c("art","År","Index","arthela")] 
  Se <- t[t$arthela %in% Arter,c("art","År","SE","arthela")] 
}


In <- dcast(In, art+arthela~År, value.var="Index")
rownames(In) <- In[,2]
In[,1:2] <- list(NULL)

Se <- dcast(Se, art+arthela~År, value.var="SE")
rownames(Se) <- Se[,2]
Se[,1:2] <- list(NULL)

a<-((1/In)^2)*(Se^2)
asum<-colSums(a)
b<-(1/nrow(In))^2
c<-(apply(In,2, gm_mean))^2
c1<-apply(In,2, gm_mean)
d<-(c*b*asum)^0.5
g<-1.96*d
e<-c1+g
f<-c1-g

out<-rbind(c1,e,f,g,d)
out<-melt(out)
out<-dcast(out, Var2~Var1)
colnames(out) <- c("År","Index","","","95%ko","SE")
write.csv2(out, file = (paste(names(ind[i]),".csv", sep='')),row.names=FALSE)
} 

## END