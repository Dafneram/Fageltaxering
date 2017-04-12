setwd("C:/DAFNE/Svensk Fågeltaxering/Skogstaxering/Skogsdata3")
library(reshape2)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(zoo)
library(grid)


#Regions
Def_Region <- function(x){
  "Norra Norrland" -> x$Region[x$Län == "Norrbottens län"|x$Län=="Västerbottens län"|x$Län=="Jämtlands län"]
  "Södra Norrland" -> x$Region[x$Län=="Dalarnas län"|x$Län=="Gävleborgs län"|x$Län=="Västernorrlands län"]
  "Östra Svealand" -> x$Region[x$Län=="Södermanlands län"|x$Län=="Örebro län"|x$Län=="Västmanlands län"|x$Län=="Stockholms län"|x$Län=="Uppsala län"]
  "Västra Göta- och Svealand" -> x$Region[x$Län=="Västra Götalands län"|x$Län=="Värmlands län"]
  "Östra Götaland" -> x$Region[x$Län=="Kronobergs län"|x$Län=="Kalmar län"|x$Län=="Jönköpings län"|x$Län=="Gotlands län"|x$Län=="Östergötlands län"]
  "Södra Götaland" -> x$Region[x$Län=="Skåne län"|x$Län=="Hallands län"|x$Län=="Blekinge län"]
  return(x$Region)}


#### sorting data #####
gs120        <- read.csv("gammalskog120.csv", sep=",", dec=",")
gs120$Region <- Def_Region(gs120)
gs120 <- gs120[!(gs120$Region=="Norra Norrland"|gs120$Region=="Södra Norrland"|gs120$Län=="Örebro län"|gs120$Län=="Värmlands län"),]
gs120[is.na(gs120)] <- 0
gs120$Län<-NULL
gs120$Region<-as.factor(gs120$Region)
gs120<-melt(gs120, id = c("Beståndstyp","Region"))
gs120 = group_by(gs120, Region, variable)
gs120 = summarise(gs120, sum_area = sum(value))
colnames(gs120) <- c("Region", "Year", "Area")
gs120$Year<-as.character(gs120$Year)
gs120$Year<- substring(gs120$Year,2,5)
gs120$Year<- as.integer(gs120$Year)
gs120 <- dcast(gs120, Year~Region)
ma <- rollapply(gs120[1:32,2:5],    width= 5, mean, by.column= TRUE, fill=NA)
gs120[,2:5]<- ma
gs120$HS <- rowSums(gs120[2:5])
gs120HS <- data.frame(gs120$Year, gs120$HS)
colnames(gs120HS) <- c("Year", "Area")

gs140        <- read.csv("gammalskog140.csv", sep=",", dec=",")
gs140$Region <- Def_Region(gs140)
gs140 <- gs140[(gs140$Region=="Norra Norrland"|gs140$Region=="Södra Norrland"|gs140$Län=="Örebro län"|gs140$Län=="Värmlands län"),]
gs140[is.na(gs140)] <- 0
gs140$Län<-NULL
gs140$Region<-as.factor(gs140$Region)
gs140<-melt(gs140, id = c("Beståndstyp","Region"))
gs140 = group_by(gs140, Region, variable)
gs140 = summarise(gs140, sum_area = sum(value))
colnames(gs140) <- c("Region", "Year", "Area")
gs140$Year<-as.character(gs140$Year)
gs140$Year<- substring(gs140$Year,2,5)
gs140$Year<- as.integer(gs140$Year)
gs140 <- dcast(gs140, Year~Region)
ma <- rollapply(gs140[1:32,2:5],    width= 5, mean, by.column= TRUE, fill=NA)
gs140[,2:5]<- ma
gs140$HS <- rowSums(gs140[2:5])
gs140HS <- data.frame(gs140$Year, gs140$HS)
colnames(gs140HS) <- c("Year", "Area")

OldForestR<-merge(gs120,gs140, by="Year")
OldForestR$WGS<-OldForestR$`Västra Göta- och Svealand.x` + OldForestR$`Västra Göta- och Svealand.y`
OldForestR$ES<-OldForestR$`Östra Svealand.x` + OldForestR$`Östra Svealand.y`
OldForestR<-OldForestR[ ,-c(3,5,6,9,10,11)]
OldForestR<-OldForestR[,c(1,4,2,5,6,3,7)]

OldForest<-merge(gs140HS,gs120HS, by="Year", all=TRUE)
OldForest$HS<-(OldForest$Area.x + OldForest$Area.y)/100000
OldForest$Area.x<-NULL
OldForest$Area.y<-NULL
colnames(OldForest)<-c("Year","value")

areal        <- read.csv("arealer.csv", sep=",", dec=",")
areal$Region <- Def_Region(areal)

arealAB1 <- subset(areal, Huggningsklass=="A"|Huggningsklass=="B1")
#arealAB1 <- subset(areal, Huggningsklass=="A"|Huggningsklass=="B1"|Huggningsklass=="B2"|Huggningsklass=="B3")
arealAB1[is.na(arealAB1)] <- 0
arealAB1$Län<-NULL
arealAB1$Region<-as.factor(arealAB1$Region)
arealAB1<-melt(arealAB1, id = c("Beståndstyp","Region", "Huggningsklass"))
arealAB1HS = group_by(arealAB1, Region, variable)
arealAB1HS = summarise(arealAB1HS, sum_area = sum(value))
arealAB1HS$variable<-as.character(arealAB1HS$variable)
arealAB1HS$variable<- substring(arealAB1HS$variable,2,5)
arealAB1HS$variable<- as.integer(arealAB1HS$variable)
arealAB1HS <- dcast(arealAB1HS, variable~Region)
arealAB1HS$Hela_Sverige <- rowSums(arealAB1HS[2:7])
arealAB1HS <- data.frame(arealAB1HS$variable,arealAB1HS$Hela_Sverige)
colnames(arealAB1HS)<-c("Year","Area")
arealAB1 = group_by(arealAB1, Region, Beståndstyp, variable)
arealAB1 = summarise(arealAB1, sum_area = sum(value))
arealAB1$Klass<-"A+B1"

arealB <- subset(areal, Huggningsklass=="B2"|Huggningsklass=="B3")
arealB[is.na(arealB)] <- 0
arealB$Län<-NULL
arealB$Region<-as.factor(arealB$Region)
arealB<-melt(arealB, id = c("Beståndstyp","Region", "Huggningsklass"))
arealB = group_by(arealB, Region, Beståndstyp, variable)
arealB = summarise(arealB, sum_area = sum(value))
arealB$Klass<-"B2+B3"

arealC <- subset(areal, Huggningsklass=="C+E")
arealC[is.na(arealC)] <- 0
arealC$Län<-NULL
arealC$Region<-as.factor(arealC$Region)
arealC<-melt(arealC, id = c("Beståndstyp","Region", "Huggningsklass"))
arealC = group_by(arealC, Region, Beståndstyp, variable)
arealC = summarise(arealC, sum_area = sum(value))
arealC$Klass<-"C+E"

arealD <- subset(areal, Huggningsklass=="D")
arealD[is.na(arealD)] <- 0
arealD$Län<-NULL
arealD$Region<-as.factor(arealD$Region)
arealD<-melt(arealD, id = c("Beståndstyp","Region", "Huggningsklass"))
arealD = group_by(arealD, Region, Beståndstyp, variable)
arealD = summarise(arealD, sum_area = sum(value))
arealD$Klass<-"D"

arealTot<-rbind(arealAB1,arealB,arealC,arealD)
arealTot$Klass<-as.factor(arealTot$Klass)
colnames(arealTot) <- c("Region","Beståndstyp","Year","Area","Huggningsklass")
arealTot$Year<-as.character(arealTot$Year)
arealTot$Year<- substring(arealTot$Year,2,5)
arealTot$Year<- as.integer(arealTot$Year)
arealTot          <- dcast(arealTot, Beståndstyp+Huggningsklass+Year~Region, value.var= "Area")
arealTot[is.na(arealTot)] <- 0
arealTot$Hela_Sverige <- rowSums(arealTot[4:9])
arealTot <- melt(arealTot, id=c("Year","Beståndstyp","Huggningsklass"))

arealTotD <- subset(arealTot, Huggningsklass=="D"& variable=="Hela_Sverige")
arealTotD <- dcast(arealTotD, Year~Beståndstyp)
ma <- rollapply(arealTotD[1:32,2:7],    width= 5, mean, by.column= TRUE, fill=NA)
arealTotD[,2:7]<- ma
colnames(arealTotD)<- c("Year","Temperate broadl.","Mixed coniferous","Mixed","Spruce","Broadleaved","Pine")
arealTotD <- melt(arealTotD, id="Year")
arealTotD$value <- arealTotD$value/100000
arealTotD$variable <- factor(arealTotD$variable, levels = c("Spruce","Pine","Mixed coniferous","Mixed","Broadleaved","Temperate broadl."))

arealTotDR <- subset(arealTot, Beståndstyp=="Lövskog exkl ädellövskog"|Beståndstyp=="Ädellövskog")
arealTotDR = group_by(arealTotDR, Year, variable)
arealTotDR = summarise(arealTotDR, sum_area = sum(value))
arealTotDR <- dcast(arealTotDR, Year~variable)
ma <- rollapply(arealTotDR[1:32,2:8],    width= 5, mean, by.column= TRUE, fill=NA)
arealTotDR[,2:8]<- ma

arealTotH <- arealTot
arealTotH <-arealTotH[(arealTotH$variable=="Hela_Sverige"),]
arealTotH$variable<-NULL
arealTotH = group_by(arealTotH, Year, Huggningsklass)
arealTotH = summarise(arealTotH, sum_area = sum(value))
arealTotH <- dcast(arealTotH, Year~Huggningsklass)
ma <- rollapply(arealTotH[1:32,2:5],    width= 5, mean, by.column= TRUE, fill=NA)
arealTotH[,2:5]<- ma
arealTotH <- melt(arealTotH, id="Year")
arealTotHdiv<-data.frame(arealTotH$Year, arealTotH$variable, (arealTotH$value/100000))
colnames(arealTotHdiv)<- c("Year","Huggningsklass","value")

arealTotR <- arealTot
arealTotR = group_by(arealTotR, Year, variable)
arealTotR = summarise(arealTotR, sum_area = sum(value))
colnames(arealTotR)<- c("Year","Region","Area")

bskt         <- read.csv("Busktäckning.csv", sep=",", dec=",")
bskt$Region  <- Def_Region(bskt)
bskt[is.na(bskt)] <- 0
bskt$Län<-NULL
bskt$Region<-as.factor(bskt$Region)
bskt<-melt(bskt, id = c("Huggningsklass","Busktäckning","Region"))
bskt = group_by(bskt, Region, variable, Huggningsklass, Busktäckning)
bskt = summarise(bskt, sum_area = sum(value))
colnames(bskt) <- c("Region", "Year","Huggningsklass","Busktäckning","Area")
bskt$Year<-as.character(bskt$Year)
bskt$Year<- substring(bskt$Year,2,5)
bskt$Year<- as.integer(bskt$Year)
bskt        <- bskt[!(bskt$Busktäckning==">0%<=6,25%"),]
bskt        <- bskt[!(bskt$Busktäckning=="0%"),]
bskt        = group_by(bskt, Region, Year, Huggningsklass)
bskt        = summarise(bskt, Area=sum(Area))
bskt        <- bskt[bskt$Huggningsklass=="C+E"|bskt$Huggningsklass=="D",]
bskt        = group_by(bskt, Region, Year)
bskt        = summarise(bskt, Area=sum(Area))  #more than 6,25% bskt, klass C+E & D!!
bskt <- dcast(bskt, Year~Region)
bskt$Hela_Sverige <- rowSums(bskt[2:7])
colnames(bskt)<-c("Year","var2","var7","var3","var5","var6","var4","var1")

skn          <- read.csv("flerskiktad.csv", sep=",", dec=",")
skn          <- skn[(skn$Flerskiktadskog=="JA"),]
skn[is.na(skn)] <- 0
skn$Region   <- Def_Region(skn)
skn$Län      <- NULL
skn          <-melt(skn, id = c("Region", "Huggningsklass","Flerskiktadskog"))
skn          <- skn[skn$Huggningsklass=="C+E"|skn$Huggningsklass=="D",] ##only C&D&E!!
skn          = group_by(skn, Region,variable, Flerskiktadskog)
skn            = summarise(skn, sum_area = sum(value))
colnames(skn)<- c("Region", "Year", "Flerskiktadskog", "Area")
skn$Year     <- as.character(skn$Year)
skn$Year     <- substring(skn$Year,2,5)
skn$Year     <- as.integer(skn$Year)
skn$Flerskiktadskog<-NULL
skn <- dcast(skn, Year~Region)
skn$Hela_Sverige <- rowSums(skn[2:7])
colnames(skn)<-c("Year","var2","var7","var3","var5","var6","var4","var1")

BushStrat<-merge(bskt,skn, by="Year", all=TRUE)
BushStrat<- data.frame(BushStrat$Year, BushStrat$var1.x, BushStrat$var1.y)
colnames(BushStrat)<-c("Year","> 6.25% shrub cover","Stratified forest")
ma <-  rollapply(BushStrat[1:32,2:3],    width= 5, mean, by.column= TRUE, fill=NA) 
BushStrat[,2:3]<- ma
BushStrat <- melt(BushStrat, id="Year")
BushStrat$value<-BushStrat$value/100000


sa350 <- read.csv("stamantal350.csv", sep=",", dec=",")
sa350$Region  <- Def_Region(sa350)
sa350         <- sa350[(sa350$Huggningsklass=="A"|sa350$Huggningsklass=="B1"),]
#sa350         <- sa350[(sa350$Huggningsklass=="A"|sa350$Huggningsklass=="B1"|sa350$Huggningsklass=="B2"|sa350$Huggningsklass=="B3"),]
sa350[is.na(sa350)] <- 0
sa350         <-melt(sa350, id = c("Region", "Län","Beståndstyp","Huggningsklass"))
sa350          = group_by(sa350, Region, variable)
sa350            = summarise(sa350, sum_area = sum(value))
colnames(sa350)<- c("Region", "Year", "Area")
sa350$Year     <- as.character(sa350$Year)
sa350$Year     <- substring(sa350$Year,2,5)
sa350$Year     <- as.integer(sa350$Year)
sa350          <- dcast(sa350, Year~Region)
sa350$Hela_Sverige <- rowSums(sa350[2:7])
sa350 <- data.frame(sa350$Year, sa350$Hela_Sverige)
colnames(sa350)<- c("Year","Antal")
sa350AB<-merge(sa350,arealAB1HS, by="Year")
ma <-  rollapply(sa350AB[1:32,2:3],    width= 5, mean, by.column= TRUE, fill=NA) 
sa350AB[,2:3]<- ma
sa350AB[,4] <- (sa350AB[,2] / sa350AB[,3])

sa200 <- read.csv("stamantal200.csv", sep=",", dec=",")
sa200$Region  <- Def_Region(sa200)
sa200         <- sa200[(sa200$Huggningsklass=="A"|sa200$Huggningsklass=="B1"),]
#sa200         <- sa200[(sa200$Huggningsklass=="A"|sa200$Huggningsklass=="B1"|sa200$Huggningsklass=="B2"|sa200$Huggningsklass=="B3"),]
sa200[is.na(sa200)] <- 0
sa200         <-melt(sa200, id = c("Region", "Län","Beståndstyp","Huggningsklass"))
sa200          = group_by(sa200, Region, variable)
sa200            = summarise(sa200, sum_area = sum(value))
colnames(sa200)<- c("Region", "Year", "Area")
sa200$Year     <- as.character(sa200$Year)
sa200$Year     <- substring(sa200$Year,2,5)
sa200$Year     <- as.integer(sa200$Year)
sa200          <- dcast(sa200, Year~Region)
sa200$Hela_Sverige <- rowSums(sa200[2:7])
sa200 <- data.frame(sa200$Year, sa200$Hela_Sverige)
colnames(sa200)<- c("Year","Antal")
sa200AB<-merge(sa200,arealAB1HS, by="Year")
ma <-  rollapply(sa200AB[1:32,2:3],    width= 5, mean, by.column= TRUE, fill=NA) 
sa200AB[,2:3]<- ma
sa200AB[,4] <- (sa200AB[,2] / sa200AB[,3])

TreeCount<- data.frame(sa200AB$Year, sa200AB$V4, sa350AB$V4)
TreeCount$sa200AB.V4<-TreeCount$sa200AB.V4-TreeCount$sa350AB.V4
colnames(TreeCount)<-c("Year","Trees >20 & <35cm","Trees >35cm")
TreeCount <- melt(TreeCount, id="Year")


adv          <- read.csv("dödved.csv", sep=",", dec=",")
adv$Region   <- Def_Region(adv)
adv          <-melt(adv, id = c("Region", "Län"))
adv          = group_by(adv, Region, variable)
adv            = summarise(adv, sum_area = sum(value))
colnames(adv)<- c("Region", "Year", "Volume")
adv$Year     <- as.character(adv$Year)
adv$Year     <- substring(adv$Year,2,5)
adv$Year     <- as.integer(adv$Year)
adv          <- dcast(adv, Year~Region)
adv$Hela_Sverige <- rowSums(adv[2:7])
adv <- melt(adv, id="Year")
colnames(adv)<- c("Year","Region","Volume")
adv<-merge(adv,arealTotR, by=c("Region","Year"))
ma <- rbind(
  rollapply(adv[1:21,3:4],    width= 5, mean, by.column= TRUE, fill=NA), 
  rollapply(adv[22:42,3:4],   width= 5, mean, by.column= TRUE, fill=NA), 
  rollapply(adv[43:63,3:4],   width= 5, mean, by.column= TRUE, fill=NA), 
  rollapply(adv[64:84,3:4],   width= 5, mean, by.column= TRUE, fill=NA),
  rollapply(adv[85:105,3:4],  width= 5, mean, by.column= TRUE, fill=NA),
  rollapply(adv[106:126,3:4], width= 5, mean, by.column= TRUE, fill=NA),
  rollapply(adv[127:147,3:4], width= 5, mean, by.column= TRUE, fill=NA)
)
adv[,3:4]<- ma
adv[,5] <- (adv[,3] / adv[,4])
adv[,3:4]<- list(NULL)
advHS <- adv[(adv$Region=="Hela_Sverige"),]
adv          <- dcast(adv, Year~Region)

hdv          <- read.csv("hårddödved.csv", sep=",", dec=",")
hdv$Region   <- Def_Region(hdv)
hdv          <-melt(hdv, id = c("Region", "Län"))
hdv          = group_by(hdv, Region, variable)
hdv            = summarise(hdv, sum_area = sum(value))
colnames(hdv)<- c("Region", "Year", "Volume")
hdv$Year     <- as.character(hdv$Year)
hdv$Year     <- substring(hdv$Year,2,5)
hdv$Year     <- as.integer(hdv$Year)
hdv          <- dcast(hdv, Year~Region)
hdv$Hela_Sverige <- rowSums(hdv[2:7])
hdv <- melt(hdv, id="Year")
colnames(hdv)<- c("Year","Region","Volume")
hdv<-merge(hdv,arealTotR, by=c("Region","Year"))
ma <- rbind(
  rollapply(hdv[1:21,3:4],    width= 5, mean, by.column= TRUE, fill=NA), 
  rollapply(hdv[22:42,3:4],   width= 5, mean, by.column= TRUE, fill=NA), 
  rollapply(hdv[43:63,3:4],   width= 5, mean, by.column= TRUE, fill=NA), 
  rollapply(hdv[64:84,3:4],   width= 5, mean, by.column= TRUE, fill=NA),
  rollapply(hdv[85:105,3:4],  width= 5, mean, by.column= TRUE, fill=NA),
  rollapply(hdv[106:126,3:4], width= 5, mean, by.column= TRUE, fill=NA),
  rollapply(hdv[127:147,3:4], width= 5, mean, by.column= TRUE, fill=NA)
)
hdv[,3:4]<- ma
hdv[,5] <- (hdv[,3] / hdv[,4])
hdv[,3:4]<- list(NULL)
hdvHS <- hdv[(hdv$Region=="Hela_Sverige"),]
hdv          <- dcast(hdv, Year~Region)

tvf          <- read.csv("torravindfällen.csv", sep=",", dec=",")
tvf$Region   <- Def_Region(tvf)
tvf          <-melt(tvf, id = c("Region", "Län"))
tvf          = group_by(tvf, Region, variable)
tvf            = summarise(tvf, sum_area = sum(value))
colnames(tvf)<- c("Region", "Year", "Volume")
tvf$Year     <- as.character(tvf$Year)
tvf$Year     <- substring(tvf$Year,2,5)
tvf$Year     <- as.integer(tvf$Year)
tvf          <- dcast(tvf, Year~Region)
tvf$Hela_Sverige <- rowSums(tvf[2:7])
tvf <- melt(tvf, id="Year")
colnames(tvf)<- c("Year","Region","Volume")
tvf<-merge(tvf,arealTotR, by=c("Region","Year"))
ma <- rbind(
  rollapply(tvf[1:32,3:4],    width= 5, mean, by.column= TRUE, fill=NA), 
  rollapply(tvf[33:64,3:4],   width= 5, mean, by.column= TRUE, fill=NA), 
  rollapply(tvf[65:96,3:4],   width= 5, mean, by.column= TRUE, fill=NA), 
  rollapply(tvf[97:128,3:4],  width= 5, mean, by.column= TRUE, fill=NA),
  rollapply(tvf[129:160,3:4], width= 5, mean, by.column= TRUE, fill=NA),
  rollapply(tvf[161:192,3:4], width= 5, mean, by.column= TRUE, fill=NA),
  rollapply(tvf[193:224,3:4], width= 5, mean, by.column= TRUE, fill=NA)
)
tvf[,3:4]<- ma
tvf[,5] <- (tvf[,3] / tvf[,4])
tvf[,3:4]<- list(NULL)
tvfHS <- tvf[(tvf$Region=="Hela_Sverige"),]
tvf          <- dcast(tvf, Year~Region)

DeadWood<-merge(advHS,hdvHS, by="Year", all=TRUE)
DeadWood<-merge(DeadWood,tvfHS, by="Year", all=TRUE)
DeadWood<- data.frame(DeadWood$Year, DeadWood$V5.x, DeadWood$V5.y, DeadWood$V5)
colnames(DeadWood)<-c("Year","All dead wood","Hard dead wood","Dry and wind-felled trees")
DeadWood <- melt(DeadWood, id="Year")

#indices
OldForest1<-OldForest
OldForest1$Index<-(OldForest1$value*1)/10.80971
OldForest1$variable<-"Old forest"
colnames(OldForest1)<- c("År","value","Index","variable")

DeadWood1<-subset(DeadWood,Year > 1995 & variable=="All dead wood")
DeadWood1$Index<-(DeadWood1$value*1)/6.300765
colnames(DeadWood1)<- c("År","variable","value","Index")

arealDdec<-subset(arealTotD, variable=="Broadleaved"|variable=="Temperate broadl.")
arealDdec  = group_by(arealDdec, Year)
arealDdec  = summarise(arealDdec, value = sum(value))
arealDdec$Index<-(arealDdec$value*1)/3.010800
arealDdec$variable<-"Broadleaved forest"
colnames(arealDdec)<- c("År","value","Index","variable")

#### move 5 year averages ####
arealTotD$Year    <-arealTotD$Year   +2
arealTotHdiv$Year <-arealTotHdiv$Year+2
OldForest$Year    <-OldForest$Year   +2
BushStrat$Year    <-BushStrat$Year   +2
TreeCount$Year    <-TreeCount$Year   +2
DeadWood$Year     <-DeadWood$Year    +2

levels(arealTotHdiv$Huggningsklass)[levels(arealTotHdiv$Huggningsklass)=="A+B1"] <- "class 1"
levels(arealTotHdiv$Huggningsklass)[levels(arealTotHdiv$Huggningsklass)=="B2+B3"] <- "class 2"
levels(arealTotHdiv$Huggningsklass)[levels(arealTotHdiv$Huggningsklass)=="C+E"] <- "class 3"
levels(arealTotHdiv$Huggningsklass)[levels(arealTotHdiv$Huggningsklass)=="D"] <- "class 4"

#### figures #####

grob_a=grobTree(textGrob("(a)",x=0.03, y=0.95,hjust=0,gp=gpar(fontsize=8)))
grob_b=grobTree(textGrob("(b)",x=0.03, y=0.95,hjust=0,gp=gpar(fontsize=8)))
grob_c=grobTree(textGrob("(c)",x=0.03, y=0.95,hjust=0,gp=gpar(fontsize=8)))
grob_d=grobTree(textGrob("(d)",x=0.03, y=0.95,hjust=0,gp=gpar(fontsize=8)))
grob_e=grobTree(textGrob("(e)",x=0.03, y=0.95,hjust=0,gp=gpar(fontsize=8)))
grob_f=grobTree(textGrob("(f)",x=0.03, y=0.95,hjust=0,gp=gpar(fontsize=8)))


PLOTA<-ggplot(arealTotD, aes(x=Year, y=value, group=variable, shape=variable)) + 
  annotate("rect", xmin=1998,xmax=2014,ymin=-Inf,ymax=Inf,fill="light grey", alpha=0.6)+
  geom_line() +
  geom_point(data=subset(arealTotD, Year %% 2 == 1), size=1)+  #subset for less points
  geom_vline(xintercept=1993, linetype="dotted") +
  xlab("Year") + ylab("Mature forest (100 000 ha)") +
  scale_shape_manual(values=c(15,16,17,0,1,2))+
  scale_x_continuous(limits=c(1985,2015),breaks=seq(1985,2015,5),expand = c(0.02, 0.01)) +
  scale_y_continuous(limits=c(0,40),breaks=seq(0,40,5)) +
  theme_bw(base_size = 9) +
  theme(panel.grid.major = element_line( color="grey85"),panel.grid.minor = element_line( color="grey93"),axis.title.y=element_text(margin=margin(0,10,0,0),size=8),axis.title.x=element_text(margin=margin(8,0,0,0),size=8),plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"),legend.justification=c(1,1.1), legend.position=c(1.08,1.14),legend.title=element_blank(),legend.background = element_rect(fill="transparent"),legend.key = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.width=unit(1,"line"))+
  guides(shape=guide_legend(ncol=2))+
  annotation_custom(grob_a)

PLOTB<-ggplot(arealTotHdiv, aes(x=Year, y=value, group=Huggningsklass, shape=Huggningsklass)) + 
  annotate("rect", xmin=1998,xmax=2014,ymin=-Inf,ymax=Inf,fill="light grey", alpha=0.6)+
  geom_point(data=subset(arealTotHdiv, Year %% 2 == 1),size=1)+
  geom_line() +
  geom_vline(xintercept=1993, linetype="dotted") +
  xlab("Year") + ylab("Maturity class (100 000 ha)") +
  scale_shape_manual(values=c(15,16,17,18))+
  scale_x_continuous(limits=c(1985,2015),breaks=seq(1985,2015,5),expand = c(0.02, 0.01)) +
  scale_y_continuous(limits=c(0,93),breaks=seq(0,100,10)) +
  theme_bw(base_size = 9) +
  theme(panel.grid.major = element_line( color="grey85"),panel.grid.minor = element_line( color="grey93"),axis.title.y=element_text(margin=margin(0,10,0,0),size=8),axis.title.x=element_text(margin=margin(8,0,0,0),size=8),legend.background = element_rect(fill="transparent"),legend.justification=c(1,0.7), legend.position=c(1,0.7), legend.title=element_blank(),legend.key.height=unit(0.5,"line"),legend.key = element_blank(),legend.key.width=unit(1,"line"),plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"))+
  annotation_custom(grob_b)

PLOTC<-ggplot(OldForest, aes(x=Year, y=value)) +
  annotate("rect", xmin=1998,xmax=2014,ymin=-Inf,ymax=Inf,fill="light grey", alpha=0.6)+
  geom_point(size=1) +
  geom_line() +
  geom_vline(xintercept=1993, linetype="dotted") +
  xlab("Year") + ylab("Old forest (100 000 ha)") +
  scale_x_continuous(limits=c(1985,2015),breaks=seq(1985,2015,5),expand = c(0.02, 0.01)) +
  scale_y_continuous(limits=c(0,20),breaks=seq(0,30,2)) +
  theme_bw(base_size = 9) +
  theme(panel.grid.major = element_line( color="grey85"),panel.grid.minor = element_line( color="grey93"),axis.title.y=element_text(margin=margin(0,10,0,0),size=8),axis.title.x=element_text(margin=margin(8,0,0,0),size=8),plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"))+
  annotation_custom(grob_c)

PLOTD<-ggplot(BushStrat, aes(x=Year, y=value, group=variable, shape=variable)) + 
  annotate("rect", xmin=1998,xmax=2014,ymin=-Inf,ymax=Inf,fill="light grey", alpha=0.6)+
  geom_point(size=1) +
  geom_line() +
  geom_vline(xintercept=1993, linetype="dotted") +
  xlab("Year") + ylab("Stratification and shrub cover (100 000 ha)") +
  scale_x_continuous(limits=c(1985,2015),breaks=seq(1985,2015,5),expand = c(0.02, 0.01)) +
  scale_y_continuous(limits=c(0,70),breaks=seq(0,80,10)) +
  theme_bw(base_size = 9) +
  theme(panel.grid.major = element_line( color="grey85"),panel.grid.minor = element_line( color="grey93"),axis.title.y=element_text(margin=margin(0,10,0,0),size=8),axis.title.x=element_text(margin=margin(8,0,0,0),size=8),legend.justification=c(0,1.1), legend.position=c(0.105,1.13),legend.background = element_rect(fill="transparent"),legend.title=element_blank(),legend.key.height=unit(0.5,"line"),legend.key = element_blank(),legend.key.width=unit(1,"line"),plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"))+
  annotation_custom(grob_d)

PLOTE<-ggplot(TreeCount, aes(x=Year, y=value, group=variable, shape=variable)) + 
  annotate("rect", xmin=1998,xmax=2014,ymin=-Inf,ymax=Inf,fill="light grey", alpha=0.6)+
  geom_point(size=1)+
  geom_line() +
  geom_vline(xintercept=1993, linetype="dotted") +
  xlab("Year") + ylab(expression("Large trees on clear cuts (N "*ha^-1*")")) +
  scale_x_continuous(limits=c(1985,2015),breaks=seq(1985,2015,5),expand = c(0.02, 0.01)) +
  scale_y_continuous(limits=c(0,25),breaks=seq(0,25,5)) +
  theme_bw(base_size = 9) +
  theme(panel.grid.major = element_line( color="grey85"),panel.grid.minor = element_line( color="grey93"),axis.title.y=element_text(margin=margin(0,10,0,0),size=8),axis.title.x=element_text(margin=margin(8,0,0,0),size=8),legend.background = element_rect(fill="transparent"),legend.justification=c(0,1.1), legend.position=c(0.105,1.13), legend.title=element_blank(),legend.key.height=unit(0.5,"line"),legend.key = element_blank(),legend.key.width=unit(1,"line"),plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"))+
  annotation_custom(grob_e)

PLOTF<-ggplot(DeadWood, aes(x=Year, y=value, group=variable, shape=variable)) + 
  annotate("rect", xmin=1998,xmax=2014,ymin=-Inf,ymax=Inf,fill="light grey", alpha=0.6)+
  geom_line() +
  geom_point(size=1) +
  geom_vline(xintercept=1993, linetype="dotted") +
  xlab("Year") + ylab(expression("Dead wood ("*m^3*" "*ha^-1*")")) +
  scale_x_continuous(limits=c(1985,2015),breaks=seq(1985,2015,5),expand = c(0.02, 0.01)) +
  scale_y_continuous(limits=c(0,10),breaks=seq(-2,10,2)) +
  theme_bw(base_size = 9) +
  theme(panel.grid.major = element_line( color="grey85"),panel.grid.minor = element_line( color="grey93"),axis.title.y=element_text(margin=margin(0,10,0,0),size=8),axis.title.x=element_text(margin=margin(8,0,0,0),size=8),legend.justification=c(0,1.1), legend.position=c(0.105,1.13),legend.background = element_rect(fill="transparent"), legend.title=element_blank(),legend.key.height=unit(0.5,"line"),legend.key = element_blank(),legend.key.width=unit(1,"line"),plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"))+
  annotation_custom(grob_f)

win.metafile("forest_resized_panel.wmf",width = 5.5, height = 7.5)
grid.arrange(PLOTA,PLOTB,PLOTC,PLOTD,PLOTE,PLOTF, nrow=3, ncol=2)
dev.off()

tiff("forest_panel.tiff", width = 5.5, height = 7.5, units = 'in', res = 1000)
grid.arrange(PLOTA,PLOTB,PLOTC,PLOTD,PLOTE,PLOTF, nrow=3, ncol=2)
dev.off()

