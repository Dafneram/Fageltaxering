setwd("C:/DAFNE/Svensk Fågeltaxering/Databases and TRIM/Fågeldata")
library(ggplot2)
library(gridExtra)
library(segmented)
library(reshape2)
library(grid)
library(dplyr)
library(zoo)


nondw  <- read.csv("nondw.csv", sep=";", dec=",")
nondec <- read.csv("nondec.csv", sep=";", dec=",")
nonold <- read.csv("nonold.csv", sep=";", dec=",")

###### t-tests ####################

sHS <- read.csv("Slopes and Significance STD 1998-2015 Hela Sverige Ej fjäll.csv", sep=",", dec=",")
tHS<-sHS[ ,c(2,3)]

Levandeskog <- c("Tjäder","Järpe","Skogsduva","Gröngöling","Mindre hackspett","Tretåspett","Nötkråka","Lavskrika","Stjärtmes","Svartmes","Tofsmes","Lappmes","Entita","Talltita","Trädkrypare","Domherre")
SkogsfåglarNonInd <- c("Bergfink","Björktrast","Blåmes","Bofink","Dubbeltrast","Gransångare","Grå flugsnappare","Gråspett","Grönsiska","Grönsångare","Gärdsmyg","Göktyta","Halsbandsflugsnappare","Härmsångare","Järnsparv","Korp","Kungsfågel","Lövsångare","Mindre flugsnappare","Mindre korsnäbb","Morkulla","Näktergal","Nötskrika","Nötväcka","Orre","Rödhake","Rödstjärt","Rödvingetrast","Sidensvans","Skogssnäppa","Spillkråka","Stenknäck","Större hackspett","Större korsnäbb","Svarthätta","Svartvit flugsnappare","Talgoxe","Taltrast","Trädgårdssångare","Trädlärka","Trädpiplärka","Videsparv")
DW<-c("Gröngöling","Mindre hackspett","Tretåspett","Entita","Talltita")
LÖS<-c("Skogsduva","Gröngöling","Mindre hackspett","Tretåspett","Stjärtmes","Entita","Trädkrypare")
GS<-c("Tjäder","Tretåspett","Lavskrika","Svartmes","Tofsmes","Lappmes","Talltita","Trädkrypare","Domherre")

StHS <-tHS[tHS$arthela %in% Levandeskog,c("arthela","Slope")]
GtHS <-tHS[tHS$arthela %in% SkogsfåglarNonInd,c("arthela","Slope")]
DWtHS <-tHS[tHS$arthela %in% DW,c("arthela","Slope")]
LÖStHS <-tHS[tHS$arthela %in% LÖS,c("arthela","Slope")]
GStHS <-tHS[tHS$arthela %in% GS,c("arthela","Slope")]

t.test(StHS$Slope, mu=1)
t.test(GtHS$Slope, mu=1)
t.test(DWtHS$Slope, mu=1)
t.test(LÖStHS$Slope, mu=1)
t.test(GStHS$Slope, mu=1)


###### combi plots OLD ##############################
##uses Skogsdata_3.R dataframes!

DödvedHS$X95.ko<-NULL
DödvedHS$SE<-NULL
DödvedHS$variable<-"Indicator"
nondw$X95.ko<-NULL
nondw$SE<-NULL
nondw$variable<-"Non-Indicators"
DeadWood1$X<-NA
DeadWood1$X.1<-NA
DeadWood1$value<-NULL
DW<-rbind(DödvedHS, DeadWood1)
DW2<-rbind(nondw, DödvedHS, DeadWood1)

LövrikskogHS$X95.ko<-NULL
LövrikskogHS$SE<-NULL
LövrikskogHS$variable<-"Indicator"
#LövrikskogHS$variable2<-"Indicators"
nondec$X95.ko<-NULL
nondec$SE<-NULL
nondec$variable<-"Non-Indicators"
arealDdec$X<-NA
arealDdec$X.1<-NA
arealDdec$value<-NULL
#arealDdec$variable2<-"Deciduous forest"
#arealDädel$X<-NA
#arealDädel$X.1<-NA
#arealDädel$value<-NULL
#arealDädel$variable2<-"Deciduous forest"
DEC<-rbind(LövrikskogHS, arealDdec)
DEC2<-rbind(nondec, LövrikskogHS, arealDdec)


GammalskogHS$X95.ko<-NULL
GammalskogHS$SE<-NULL
GammalskogHS$variable<-"Indicator"
nonold$X95.ko<-NULL
nonold$SE<-NULL
nonold$variable<-"Non-Indicators"
OldForest1$X<-NA
OldForest1$X.1<-NA
OldForest1$total<-NULL
OldForest1$value<-NULL
OldForest1<-OldForest1[-c(1,2,30,31), ]
OLD2<-rbind(nonold, GammalskogHS,OldForest1)
OLD2$variable <- factor(OLD2$variable, levels = c("Old forest","Indicator"))
OLD<-rbind(OldForest1, GammalskogHS)
OLD$variable<-factor(OLD$variable, levels=c("Old forest","Indicator"))

Total<-merge(arealDdec, OldForest1, by="År", all=FALSE)
Total<-merge(Total, DeadWood1, by="År", all=FALSE)
Total<-Total[ ,-c(3,4,5,7,8,9,10,12,13,14)]

Total$Index<-exp((log(Total$Index.x)+log(Total$Index.y)+log(Total$Index))/3)

LevandeskogHS$X95.ko<-NULL
LevandeskogHS$SE<-NULL
LevandeskogHS$variable<-"Indicator"
Total$X<-NA
Total$X.1<-NA
Total$Index.x<-NULL
Total$Index.y<-NULL
Total$variable<-"Combined forest index"
TOT<-rbind(Total, LevandeskogHS)
TOT$variable <- factor(TOT$variable, levels = c("Combined forest index", "Indicator"))



p1 <- ggplot(DW, aes(År, Index))+
  geom_ribbon(data=DödvedHS,aes(ymin=X,ymax=X.1),fill="grey")+
  geom_line(data=DödvedHS)+
  geom_line(data=DeadWood1)+
  xlab("Year")+
  scale_x_continuous(limits=c(1995,2015),breaks=seq(1995, 2015, 5),expand = c(0, 0.5))+
  scale_y_continuous(limits=c(0.4,4),breaks=seq(0, 4, 1))+
  theme_bw(base_size = 10) +
  theme(axis.title.y=element_text(margin=margin(0,10,0,0)),axis.title.x=element_text(margin=margin(8,0,0,0)))+
  ggtitle("Dead wood trend and indicator")+
  facet_grid(variable~., scale = "free_y")

p2 <- ggplot(DEC, aes(År, Index))+
  geom_ribbon(data=LövrikskogHS,aes(ymin=X,ymax=X.1),fill="grey")+
  geom_line(data=LövrikskogHS)+
  geom_line(data=arealDdec, linetype="solid")+
  xlab("Year")+ 
  scale_x_continuous(limits=c(1985,2015),breaks=seq(1985, 2015, 5),expand = c(0, 0.8))+
  scale_y_continuous(limits=c(0.4,3.5),breaks=seq(0, 3, 1), expand = c(0,0))+
  theme_bw(base_size = 10) +
  theme(axis.title.y=element_text(margin=margin(0,10,0,0)),axis.title.x=element_text(margin=margin(8,0,0,0)))+
  ggtitle("Broadleaved forest trend and indicator")+
  facet_grid(variable~., scale="free")

p3 <- ggplot(OLD, aes(År, Index))+
  geom_ribbon(data=OLD,aes(ymin=X,ymax=X.1),fill="grey")+
  geom_line(data=OLD)+
  xlab("Year")+
  scale_y_continuous(limits=c(0.5,2),breaks=seq(0.5, 2, 0.5))+
  scale_x_continuous(limits=c(1985,2015),breaks=seq(1985, 2015, 5),expand = c(0, 0.8))+
  theme_bw(base_size = 10) +
  theme(axis.title.y=element_text(margin=margin(0,10,0,0)),axis.title.x=element_text(margin=margin(8,0,0,0)))+
  ggtitle("Old forest trend and indicator")+
  facet_grid(variable~., scale = "free_y")

p4 <- ggplot(TOT, aes(År, Index))+
  geom_ribbon(data=LevandeskogHS,aes(ymin=X,ymax=X.1),fill="grey")+
  geom_line(data=LevandeskogHS)+
  geom_line(data=Total)+
  xlab("Year")+
  scale_x_continuous(limits=c(1995,2015),breaks=seq(1995, 2015, 5),expand = c(0, 0.5))+
  scale_y_continuous(limits=c(0.5,2),breaks=seq(0.5, 2, 0.5))+
  theme_bw(base_size = 10) +
  theme(axis.title.y=element_text(margin=margin(0,10,0,0)),axis.title.x=element_text(margin=margin(8,0,0,0)))+
  ggtitle("Combined forest trend and indicator")+
  facet_grid(variable~., scale = "fixed")


win.metafile("panelplots3.wmf",width = 8, height = 8)
grid.arrange(p2, p3, p1, p4, nrow=2, ncol=2)
dev.off()



win.metafile("deadwood3.wmf",width = 4, height = 3)
ggplot(DödvedHS, aes(År, Index))+
  geom_ribbon(data=DödvedHS,aes(ymin=X,ymax=X.1),fill="grey")+
  geom_line(data=DödvedHS, size=1)+
  geom_line(data=nondw, size=1, color="red")+
  geom_line(data=DeadWood1, linetype="dashed")+
  xlab("Year")+
  scale_x_continuous(breaks=seq(1994, 2014, 2))+
  scale_y_continuous(breaks=seq(0, 4, 1),expand = c(0, 0))+    
  theme_classic(base_size = 11) +
  theme(axis.title.y=element_text(margin=margin(0,10,0,0)),axis.title.x=element_text(margin=margin(8,0,0,0)))+
  geom_hline(aes(yintercept=4), colour="#000000")+
  geom_hline(aes(yintercept=3), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  ggtitle("Dead wood and non-dead wood species")
dev.off()

win.metafile("deciduous3.wmf",width = 4, height = 3)
ggplot(LövrikskogHS, aes(År, Index))+
  geom_ribbon(data=LövrikskogHS,aes(ymin=X,ymax=X.1),fill="grey")+
  geom_line(data=LövrikskogHS, size=1)+
  geom_line(data=nondec, size=1, color="red")+
  geom_line(data=arealDdec, linetype="dashed")+
  xlab("Year")+
  scale_x_continuous(limits=c(1984,2014),breaks=seq(1984, 2014, 5))+
  scale_y_continuous(breaks=seq(0, 4, 1),expand = c(0, 0))+    
  theme_classic(base_size = 11) +
  theme(axis.title.y=element_text(margin=margin(0,10,0,0)),axis.title.x=element_text(margin=margin(8,0,0,0)))+
  geom_hline(aes(yintercept=4), colour="#000000")+
  geom_hline(aes(yintercept=3), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  ggtitle("Broadleaved forest and non-broadl. species")
dev.off()

win.metafile("old3.wmf",width = 4, height = 3)
ggplot(GammalskogHS, aes(År, Index))+
  geom_ribbon(data=GammalskogHS,aes(ymin=X,ymax=X.1),fill="grey")+
  geom_line(data=GammalskogHS, size=1)+
  geom_line(data=nonold, size=1, color="red")+
  geom_line(data=subset(OLD, variable=="Old forest"), linetype="dashed")+
  xlab("Year")+
  scale_x_continuous(limits=c(1984,2014),breaks=seq(1984, 2014, 5))+
  scale_y_continuous(breaks=seq(0, 4, 1),expand = c(0, 0))+    
  theme_classic(base_size = 11) +
  theme(axis.title.y=element_text(margin=margin(0,10,0,0)),axis.title.x=element_text(margin=margin(8,0,0,0)))+
  geom_hline(aes(yintercept=4), colour="#000000")+
  geom_hline(aes(yintercept=3), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  ggtitle("Old forest and non-old forest species")
dev.off()

win.metafile("total2.wmf",width = 4, height = 3)
ggplot(LevandeskogHS, aes(År, Index))+
  geom_ribbon(data=LevandeskogHS,aes(ymin=X,ymax=X.1),fill="grey")+
  geom_line(data=LevandeskogHS, size=1)+
  geom_line(data=Total, linetype="dashed")+
  ylim(0, 2.5) + 
  xlab("Year")+
  scale_x_continuous(breaks=seq(1984, 2014, 2))+
  theme_classic(base_size = 11) +
  geom_hline(aes(yintercept=2.5), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1.5), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0.5), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  ggtitle("Forest indicators")
dev.off()

win.metafile("indicators95CI.wmf",width = 6, height = 4.5)
grid.arrange(IND,DW,DEC,OLD,nrow=2, ncol=2)
dev.off()


areaDNN <-subset(read.csv("areadNN.csv", sep=";", dec=","),Year > 2001 & variable=="ädel Broad-leaved")
areaDSN <-subset(read.csv("areadSN.csv", sep=";", dec=","),Year > 2001 & variable=="ädel Broad-leaved")
areaDÖG <-subset(read.csv("areadÖG.csv", sep=";", dec=","),Year > 2001 & variable=="ädel Broad-leaved")
areaDVGS <-subset(read.csv("areadVGS.csv", sep=";", dec=","),Year > 2001 & variable=="ädel Broad-leaved")
areaDSG <-subset(read.csv("areadSG.csv", sep=";", dec=","),Year > 2001 & variable=="ädel Broad-leaved")
areaDÖS <-subset(read.csv("areadÖS.csv", sep=";", dec=","),Year > 2001 & variable=="ädel Broad-leaved")
deciduous<-cbind(areaDNN, areaDSN[,3],areaDÖG[,3],areaDVGS[,3],areaDSG[,3],areaDÖS[,3])
write.csv2(deciduous, "nobledeciduous.csv")

###### combi plots ##############################
##uses Skogsdata_3.R dataframes!


DödvedHS<-read.csv("Sweden_DW, Trend in populatieaantal_RESULTS.csv", sep=";",dec=",")
DödvedHS$simMSImean<-NULL
DödvedHS$variable<-"Dead wood MSI"
colnames(DödvedHS)<-c("År","Index","Lower","Upper","variable")
DödvedHS$Index<-DödvedHS$Index/100
DödvedHS$Lower<-DödvedHS$Lower/100
DödvedHS$Upper<-DödvedHS$Upper/100
DeadWood1$Lower<-NA
DeadWood1$Upper<-NA
DeadWood1$value<-NULL
DeadWood1$År<-DeadWood1$År+2 #MOVE MOVING AVERAGES!
DW<-rbind(DödvedHS, DeadWood1)


LövrikskogHS<-read.csv("Sweden_BL, Trend in populatieaantal_RESULTS.csv", sep=";",dec=",")
LövrikskogHS$simMSImean<-NULL
LövrikskogHS$variable<-"Broadleaved forest MSI"
colnames(LövrikskogHS)<-c("År","Index","Lower","Upper","variable")
LövrikskogHS$Index<-LövrikskogHS$Index/100
LövrikskogHS$Lower<-LövrikskogHS$Lower/100
LövrikskogHS$Upper<-LövrikskogHS$Upper/100
arealDdec$Lower<-NA
arealDdec$Upper<-NA
arealDdec$value<-NULL
arealDdec$År<-arealDdec$År+2 #MOVE MOVING AVERAGES!
DEC<-rbind(LövrikskogHS, arealDdec)

GammalskogHS<-read.csv("Sweden_OLD, Trend in populatieaantal_RESULTS.csv", sep=";",dec=",")
GammalskogHS$simMSImean<-NULL
GammalskogHS$variable<-"Old forest MSI"
colnames(GammalskogHS)<-c("År","Index","Lower","Upper","variable")
GammalskogHS$Index<-GammalskogHS$Index/100
GammalskogHS$Lower<-GammalskogHS$Lower/100
GammalskogHS$Upper<-GammalskogHS$Upper/100
OldForest1$Lower<-NA
OldForest1$Upper<-NA
OldForest1$total<-NULL
OldForest1$value<-NULL
OldForest1<-OldForest1[-c(1,2,31,32), ]
OldForest1$År<-OldForest1$År+2 #MOVE MOVING AVERAGES!
OLD<-rbind(OldForest1, GammalskogHS)
OLD$variable<-factor(OLD$variable, levels=c("Old forest MSI","Old forest"))

Total<-merge(arealDdec, OldForest1, by="År", all=FALSE)
Total<-merge(Total, DeadWood1, by="År", all=FALSE)
Total<-Total[ ,c(1,2,6,11)]

Total$Index<-exp((log(Total$Index.x)+log(Total$Index.y)+log(Total$Index))/3)

LevandeskogHS<-read.csv("Sweden_Spec, Trend in populatieaantal_RESULTS.csv", sep=";",dec=",")
LevandeskogHS$simMSImean<-NULL
LevandeskogHS$variable<-"Specialists MSI"
colnames(LevandeskogHS)<-c("År","Index","Lower","Upper","variable")
LevandeskogHS$Index<-LevandeskogHS$Index/100
LevandeskogHS$Lower<-LevandeskogHS$Lower/100
LevandeskogHS$Upper<-LevandeskogHS$Upper/100
Total$Lower<-NA
Total$Upper<-NA
Total$Index.x<-NULL
Total$Index.y<-NULL
Total$variable<-"Combined forest index"
TOT<-rbind(Total, LevandeskogHS)
TOT$variable <- factor(TOT$variable, levels = c("Combined forest index", "Specialists MSI"))

ann_texta <- data.frame(År = 1986,Index = 2.9,lab = "(a)",variable = factor("Broadleaved forest",levels = c("Broadleaved forest MSI","Broadleaved forest")))
ann_textb <- data.frame(År = 1986,Index = 1.93,lab = "(b)",variable = factor("Old forest",levels = c("Old forest MSI","Old forest")))
ann_textc <- data.frame(År = 1995.8,Index = 2.9,lab = "(c)",variable = factor("All dead wood",levels = c("Dead wood MSI","All dead wood")))
ann_textd <- data.frame(År = 1995.8,Index = 1.93,lab = "(d)",variable = factor("Combined forest index",levels = c("Specialists MSI","Combined forest index")))

p1 <- ggplot(DW, aes(År, Index))+
  geom_ribbon(data=DödvedHS,aes(ymin=Lower,ymax=Upper),fill="grey")+
  geom_line(data=DödvedHS)+
  geom_line(data=DeadWood1)+
  xlab("Year")+
  scale_x_continuous(limits=c(1995,2015),breaks=seq(1995, 2015, 5),expand = c(0, 0.5))+
  scale_y_continuous(limits=c(0.4,3),breaks=seq(0, 3, 1))+
  theme_bw(base_size = 9) +
  theme(axis.title.y=element_text(margin=margin(0,10,0,0),size=8),axis.title.x=element_text(margin=margin(8,0,0,0),size=8))+
  geom_text(data = ann_textc,label = "(c)", size=3.3)+
  facet_grid(variable~., scale = "free_y")

p2 <- ggplot(DEC, aes(År, Index))+
  geom_ribbon(data=LövrikskogHS,aes(ymin=Lower,ymax=Upper),fill="grey")+
  geom_line(data=LövrikskogHS)+
  geom_line(data=arealDdec, linetype="solid")+
  xlab("Year")+ 
  scale_x_continuous(limits=c(1985,2015),breaks=seq(1985, 2015, 5),expand = c(0, 0.8))+
  scale_y_continuous(limits=c(0.4,3),breaks=seq(0, 3, 1))+
  theme_bw(base_size = 9) +
  theme(axis.title.y=element_text(margin=margin(0,10,0,0),size=8),axis.title.x=element_text(margin=margin(8,0,0,0),size=8))+
  geom_text(data = ann_texta,label = "(a)", size=3.3)+
  facet_grid(variable~., scale="free")

p3 <- ggplot(OLD, aes(År, Index))+
  geom_ribbon(data=OLD,aes(ymin=Lower,ymax=Upper),fill="grey")+
  geom_line(data=OLD)+
  xlab("Year")+
  scale_y_continuous(limits=c(0.5,2),breaks=seq(0, 2, 1))+
  scale_x_continuous(limits=c(1985,2015),breaks=seq(1985, 2015, 5),expand = c(0, 0.8))+
  theme_bw(base_size = 9) +
  theme(axis.title.y=element_text(margin=margin(0,10,0,0),size=8),axis.title.x=element_text(margin=margin(8,0,0,0),size=8))+
  geom_text(data = ann_textb,label = "(b)", size=3.3)+
  facet_grid(variable~., scale = "free_y")

p4 <- ggplot(TOT, aes(År, Index))+
  geom_ribbon(data=LevandeskogHS,aes(ymin=Lower,ymax=Upper),fill="grey")+
  geom_line(data=LevandeskogHS)+
  geom_line(data=Total)+
  xlab("Year")+
  scale_x_continuous(limits=c(1995,2015),breaks=seq(1995, 2015, 5),expand = c(0, 0.5))+
  scale_y_continuous(limits=c(0.5,2),breaks=seq(0, 2, 1))+
  theme_bw(base_size = 9) +
  theme(axis.title.y=element_text(margin=margin(0,10,0,0),size=8),axis.title.x=element_text(margin=margin(8,0,0,0),size=8))+
  geom_text(data = ann_textd,label = "(d)", size=3.3)+
  facet_grid(variable~., scale = "fixed")

#new combi plot, no panels
grob_a=grobTree(textGrob("(a)",x=0.03, y=0.95,hjust=0,gp=gpar(fontsize=8)))
grob_b=grobTree(textGrob("(b)",x=0.03, y=0.95,hjust=0,gp=gpar(fontsize=8)))
grob_c=grobTree(textGrob("(c)",x=0.03, y=0.95,hjust=0,gp=gpar(fontsize=8)))
grob_d=grobTree(textGrob("(d)",x=0.03, y=0.95,hjust=0,gp=gpar(fontsize=8)))

p12<-ggplot(DW, aes(År, Index, linetype=variable))+
  geom_ribbon(data=DW, aes(ymin=Lower,ymax=Upper),alpha=0.5,fill="grey",show.legend = FALSE)+
  geom_line(data=DW)+
  xlab("Year")+
  scale_x_continuous(limits=c(1995,2015),breaks=seq(1995, 2015, 5),expand = c(0, 0.5))+
  scale_y_continuous(limits=c(0.4,2.5),breaks=seq(0, 3, 1))+
  scale_linetype_manual(values=c("dashed", "solid"))+
  theme_bw(base_size = 9) +
  theme(panel.grid.major = element_line( color="grey85"),
        panel.grid.minor = element_blank(),
        axis.title.y=element_text(margin=margin(0,10,0,0),size=8),
        axis.title.x=element_text(margin=margin(8,0,0,0),size=8),
        legend.position="none")
#        legend.justification=c(0,1.1), 
#        legend.position=c(0.105,1.13),
#        legend.background = element_rect(fill="transparent"), 
#        legend.title=element_blank(),
#        legend.key.height=unit(0.5,"line"),
#        legend.key = element_blank(),
#        legend.key.width=unit(1,"line"))+
#  annotation_custom(grob_c)
  

p22 <- ggplot(DEC, aes(År, Index, group=variable, linetype=variable))+
  geom_ribbon(data=LövrikskogHS,aes(ymin=Lower,ymax=Upper),fill="grey", alpha=0.5,show.legend = FALSE)+
  geom_line(data=DEC)+
  xlab("Year")+ 
  scale_x_continuous(limits=c(1985,2015),breaks=seq(1985, 2015, 5),expand = c(0, 0.8))+
  scale_y_continuous(limits=c(0.4,2.5),breaks=seq(0, 3, 1))+
  scale_linetype_manual(values=c("dashed", "solid"))+
  theme_bw(base_size = 9) +
  theme(panel.grid.major = element_line( color="grey85"),
        panel.grid.minor = element_blank(),
        axis.title.y=element_text(margin=margin(0,10,0,0),size=8),
        axis.title.x=element_text(margin=margin(8,0,0,0),size=8),
        legend.position="none")
#        legend.justification=c(0,1.1), 
#        legend.position=c(0.105,1.13),
#        legend.background = element_rect(fill="transparent"), 
#        legend.title=element_blank(),
#        legend.key.height=unit(0.5,"line"),
#        legend.key = element_blank(),
#        legend.key.width=unit(1,"line"))+
#  annotation_custom(grob_a)

p32 <- ggplot(OLD, aes(År, Index, group=variable, linetype=variable))+
  geom_ribbon(data=GammalskogHS,aes(ymin=Lower,ymax=Upper),fill="grey", alpha=0.5,show.legend = FALSE)+
  geom_line(data=OLD)+
  xlab("Year")+
  scale_y_continuous(limits=c(0.5,2.5),breaks=seq(0, 2, 1))+
  scale_x_continuous(limits=c(1985,2015),breaks=seq(1985, 2015, 5),expand = c(0, 0.8))+
  scale_linetype_manual(values=c("dashed", "solid"))+
  theme_bw(base_size = 9) +
  theme(panel.grid.major = element_line( color="grey85"),
        panel.grid.minor = element_blank(),
        axis.title.y=element_text(margin=margin(0,10,0,0),size=8),
        axis.title.x=element_text(margin=margin(8,0,0,0),size=8),
        legend.position="none")
#        legend.justification=c(0,1.1), 
#        legend.position=c(0.105,1.13),
#        legend.background = element_rect(fill="transparent"), 
#        legend.title=element_blank(),
#        legend.key.height=unit(0.5,"line"),
#        legend.key = element_blank(),
#        legend.key.width=unit(1,"line"))+
#  annotation_custom(grob_b)

p42 <- ggplot(TOT, aes(År, Index, group=variable, linetype=variable))+
  geom_ribbon(data=LevandeskogHS,aes(ymin=Lower,ymax=Upper),fill="grey", alpha=0.5,show.legend = FALSE)+
  geom_line(data=TOT)+
  xlab("Year")+
  scale_x_continuous(limits=c(1995,2015),breaks=seq(1995, 2015, 5),expand = c(0, 0.5))+
  scale_y_continuous(limits=c(0.5,2.5),breaks=seq(0, 2, 1))+
  scale_linetype_manual(values=c("dashed", "solid"))+
  theme_bw(base_size = 9) +
  theme(panel.grid.major = element_line( color="grey85"),
        panel.grid.minor = element_blank(),
        axis.title.y=element_text(margin=margin(0,10,0,0),size=8),
        axis.title.x=element_text(margin=margin(8,0,0,0),size=8),
        legend.position="none")
#        legend.justification=c(0,1.1), 
#        legend.position=c(0.105,1.13),
#        legend.background = element_rect(fill="transparent"), 
#        legend.title=element_blank(),
#        legend.key.height=unit(0.5,"line"),
#        legend.key = element_blank(),
#        legend.key.width=unit(1,"line"))+
#  annotation_custom(grob_d)

win.metafile("panelplots_smoothed_resized.wmf",width = 5.3, height = 5.3)
grid.arrange(p2, p3, p1, p4, nrow=2, ncol=2)
dev.off()

tiff("combi_poster.tiff", width = 5.3, height = 4.5, units = 'in', res = 1000)
grid.arrange(p22, p32, p12, p42, nrow=2, ncol=2)
dev.off()



###### 6 point plots #################
# 1-SG
# 2-EG
# 3-WGS
# 4-ES
# 5-SN
# 6-NN
# 7-SF
# 1-BL
# 2-OLD
# 3-DW


dwNN  <-read.csv("Sweden_63, Trend in populatieaantal_TRENDS.csv", sep=";", dec=",")[1,2]
dwSN  <-read.csv("Sweden_53, Trend in populatieaantal_TRENDS.csv", sep=";", dec=",")[1,2]
dwÖG  <-read.csv("Sweden_23, Trend in populatieaantal_TRENDS.csv", sep=";", dec=",")[1,2]
dwSG  <-read.csv("Sweden_13, Trend in populatieaantal_TRENDS.csv", sep=";", dec=",")[1,2]
dwVG  <-read.csv("Sweden_33, Trend in populatieaantal_TRENDS.csv", sep=";", dec=",")[1,2]
dwÖS  <-read.csv("Sweden_43, Trend in populatieaantal_TRENDS.csv", sep=";", dec=",")[1,2]
decNN <-read.csv("Sweden_61, Trend in populatieaantal_TRENDS.csv", sep=";", dec=",")[1,2]
decSN <-read.csv("Sweden_51, Trend in populatieaantal_TRENDS.csv", sep=";", dec=",")[1,2]
decÖG <-read.csv("Sweden_21, Trend in populatieaantal_TRENDS.csv", sep=";", dec=",")[1,2]
decSG <-read.csv("Sweden_11, Trend in populatieaantal_TRENDS.csv", sep=";", dec=",")[1,2]
decVG <-read.csv("Sweden_31, Trend in populatieaantal_TRENDS.csv", sep=";", dec=",")[1,2]
decÖS <-read.csv("Sweden_41, Trend in populatieaantal_TRENDS.csv", sep=";", dec=",")[1,2]
oldNN <-read.csv("Sweden_62, Trend in populatieaantal_TRENDS.csv", sep=";", dec=",")[1,2]
oldSN <-read.csv("Sweden_52, Trend in populatieaantal_TRENDS.csv", sep=";", dec=",")[1,2]
oldÖG <-read.csv("Sweden_22, Trend in populatieaantal_TRENDS.csv", sep=";", dec=",")[1,2]
oldSG <-read.csv("Sweden_12, Trend in populatieaantal_TRENDS.csv", sep=";", dec=",")[1,2]
oldVG <-read.csv("Sweden_32, Trend in populatieaantal_TRENDS.csv", sep=";", dec=",")[1,2]
oldÖS <-read.csv("Sweden_42, Trend in populatieaantal_TRENDS.csv", sep=";", dec=",")[1,2]

a<-read.csv("6pointtest 2002-2015.csv", sep=";", dec=",")
a[,3]<-NA
a[,4]<-NA

adv$Year<-adv$Year+2
arealTotDR$Year<-arealTotDR$Year+2
OldForestR$Year<-OldForestR$Year+2

a[1,3]<-((adv[19,2]/adv[7,2])*100)-100
a[2,3]<-((adv[19,3]/adv[7,3])*100)-100
a[3,3]<-((adv[19,4]/adv[7,4])*100)-100
a[4,3]<-((adv[19,5]/adv[7,5])*100)-100
a[5,3]<-((adv[19,6]/adv[7,6])*100)-100
a[6,3]<-((adv[19,7]/adv[7,7])*100)-100
a[7,3]<-((arealTotDR[30,2]/arealTotDR[18,2])*100)-100
a[8,3]<-((arealTotDR[30,3]/arealTotDR[18,3])*100)-100
a[9,3]<-((arealTotDR[30,4]/arealTotDR[18,4])*100)-100
a[10,3]<-((arealTotDR[30,5]/arealTotDR[18,5])*100)-100
a[11,3]<-((arealTotDR[30,6]/arealTotDR[18,6])*100)-100
a[12,3]<-((arealTotDR[30,7]/arealTotDR[18,7])*100)-100
a[13,3]<-((OldForestR[30,2]/OldForestR[18,2])*100)-100
a[14,3]<-((OldForestR[30,3]/OldForestR[18,3])*100)-100
a[15,3]<-((OldForestR[30,4]/OldForestR[18,4])*100)-100
a[16,3]<-((OldForestR[30,5]/OldForestR[18,5])*100)-100
a[17,3]<-((OldForestR[30,6]/OldForestR[18,6])*100)-100
a[18,3]<-((OldForestR[30,7]/OldForestR[18,7])*100)-100

a[1,4]<-dwNN
a[2,4]<-dwSG
a[3,4]<-dwSN
a[4,4]<-dwVG
a[5,4]<-dwÖG
a[6,4]<-dwÖS
a[7,4]<-decNN
a[8,4]<-decSG
a[9,4]<-decSN
a[10,4]<-decVG
a[11,4]<-decÖG
a[12,4]<-decÖS
a[13,4]<-oldNN
a[14,4]<-oldSG
a[15,4]<-oldSN
a[16,4]<-oldVG
a[17,4]<-oldÖG
a[18,4]<-oldÖS

a$reg<-c(6,1,5,3,2,4)

b <- dcast(a, region~indicator, value.var="perc_change")
b$t_index<-(b$DEC+b$DW+b$OLD)/3
b$spec <- c(1.0034,1.0117,0.995,1.033,1.0045,0.9851) #taken from regional trend calculations from Arco on 2006 breakpoint
b$reg<-c(2,4,6,1,5,3)


dw  <-lm(ind_slope~perc_change,a, indicator=="DW")
dec <-lm(ind_slope~perc_change,a, indicator=="DEC")
old <-lm(ind_slope~perc_change,a, indicator=="OLD")
tot <-lm(spec~t_index,b)


grob_dw=grobTree(textGrob("(c)",x=0.05, y=0.93,hjust=0,
                          gp=gpar(fontsize=8)))
grob_dec=grobTree(textGrob("(a)",x=0.05, y=0.93,hjust=0,
                          gp=gpar(fontsize=8)))
grob_old=grobTree(textGrob("(b)",x=0.05, y=0.93,hjust=0,
                          gp=gpar(fontsize=8)))
grob_tot=grobTree(textGrob("(d)",x=0.05, y=0.93,hjust=0,
                           gp=gpar(fontsize=8)))



tiff("DW6point_smoothed_resized.tiff",width = 2.79, height = 2, units="in", res=1000)
ggplot(subset(a, indicator=="DW"), aes(perc_change, ind_slope, label=region))+
  geom_point(data=subset(a, indicator=="DW"), size=1)+
  geom_abline(intercept= coef(dw)[1], slope= coef(dw)[2], linetype="dashed")+
  geom_text(aes(label=reg,hjust=1.6, vjust=-0.45),size=2.5)+
  theme_bw(base_size = 9)+
  theme(axis.title.y=element_text(margin=margin(0,10,0,0), size=8),axis.title.x=element_text(margin=margin(8,0,0,0),size=8),legend.position="none")+
  scale_x_continuous(limits= c(-5,100),breaks=seq(0, 100, 10))+
  scale_y_continuous(limits= c(0.96, 1.06),breaks=seq(0.96, 1.1, 0.02))+
  xlab("Change in amount of dead wood (%)")+
  ylab("Slope of dead wood MSI")+
  annotation_custom(grob_dw)
dev.off()

tiff("DEC6point_smoothed_resized.tiff",width = 2.79, height = 2, units="in", res=1000)
ggplot(subset(a, indicator=="DEC"), aes(perc_change, ind_slope, label=region))+
  geom_point(data=subset(a, indicator=="DEC"), size=1)+
  geom_abline(intercept= coef(dec)[1], slope= coef(dec)[2], linetype="dashed")+
  geom_text(aes(label=reg,hjust=1.9, vjust=0.1),size=2.5)+
  theme_bw(base_size = 9)+
  theme(axis.title.y=element_text(margin=margin(0,10,0,0), size=8),axis.title.x=element_text(margin=margin(8,0,0,0),size=8),legend.position="none")+
  scale_x_continuous(limits= c(-20, 40),breaks=seq(-20, 40, 10))+
  scale_y_continuous(limits= c(0.96, 1.06),breaks=seq(0.96, 1.06, 0.02))+
  xlab("Change in amount of broadleaved forest (%)")+
  ylab("Slope of broadleaved forest MSI")+
  annotation_custom(grob_dec)
dev.off()

tiff("OLD6point_smoothed_resized.tiff",width = 2.79, height = 2, units="in", res=1000)
ggplot(subset(a, indicator=="OLD"), aes(perc_change, ind_slope, label=region))+
  geom_point(data=subset(a, indicator=="OLD"), size=1)+
  geom_abline(intercept= coef(old)[1], slope= coef(old)[2], linetype="dashed")+
  geom_text(aes(label=reg,hjust=1.6, vjust=-0.45),size=2.5)+
  theme_bw(base_size = 9)+
  theme(axis.title.y=element_text(margin=margin(0,10,0,0), size=8),axis.title.x=element_text(margin=margin(8,0,0,0),size=8),legend.position="none")+
#  theme(legend.position = "bottom",legend.key = element_blank())+
#  guides(shape=guide_legend(ncol=2, title.position="top"))+
  scale_x_continuous(limits= c(20, 80),breaks=seq(20, 80, 10))+
  scale_y_continuous(limits= c(0.96, 1.06),breaks=seq(0.96, 1.06, 0.02))+
  xlab("Change in amount of old forest (%)")+
  ylab("Slope of old forest MSI")+
  annotation_custom(grob_old)
dev.off()

tiff("TOT6point_smoothed_resized.tiff",width = 2.79, height = 2, units="in", res=1000)
ggplot(b, aes(t_index, spec, label=region))+
  geom_point(size=1)+
  geom_abline(intercept= coef(tot)[1], slope= coef(tot)[2], linetype="dashed")+
  geom_text(aes(label=reg,hjust=1.9, vjust=0.1),size=2.5)+
  theme_bw(base_size = 9)+
  theme(axis.title.y=element_text(margin=margin(0,10,0,0), size=8),axis.title.x=element_text(margin=margin(8,0,0,0),size=8),legend.position="none")+
  scale_x_continuous(limits= c(0, 70),breaks=seq(0, 70, 10))+
  scale_y_continuous(limits= c(0.96, 1.06),breaks=seq(0.96, 1.06, 0.02))+
  xlab("Change in combined forest index (%)")+
  ylab("Slope of specialists MSI")+
  annotation_custom(grob_tot)
dev.off()

######## temp and precip ##########

PRECIP <- subset(rbind(cbind(read.csv("P at EG stations 4-8 avg.csv", sep = ";", dec= ","),Region = "EG"),
cbind(read.csv("P at SG stations 4-8 avg.csv", sep = ";", dec= ","),Region = "SG"),
cbind(read.csv("P at SN stations 4-8 avg.csv", sep = ";", dec= ","),Region = "SN"),
cbind(read.csv("P at NN stations 4-8 avg.csv", sep = ";", dec= ","),Region = "NN"),
cbind(read.csv("P at VG stations 4-8 avg.csv", sep = ";", dec= ","),Region = "WGS"),
cbind(read.csv("P at ES stations 4-8 avg.csv", sep = ";", dec= ","),Region = "ES")), YR > 2001)

TEMP <- subset( rbind(cbind(read.csv("T at EG stations 4-8 avg.csv", sep = ";", dec= ","),Region = "EG"),
cbind(read.csv("T at SG stations 4-8 avg.csv", sep = ";", dec= ","),Region = "SG"),
cbind(read.csv("T at SN stations 4-8 avg.csv", sep = ";", dec= ","),Region = "SN"),
cbind(read.csv("T at NN stations 4-8 avg.csv", sep = ";", dec= ","),Region = "NN"),
cbind(read.csv("T at VG stations 4-8 avg.csv", sep = ";", dec= ","),Region = "WGS"),
cbind(read.csv("T at ES stations 4-8 avg.csv", sep = ";", dec= ","),Region = "ES")), YR > 2002)

THS <- (read.csv2("T at 716 stations 4-8 HS.csv", sep=";",dec=","))

ma <- rollapply(THS[1:54,2],    width= 5, mean, align="right", fill=NA)
THS$Tma<- ma


nTHS<-data.frame(cbind(THS$YR,THS$Tma))
colnames(nTHS)<-c("YR","AvgOfTEMP")

nTHS$var<-"Moving 5 year avg. of temperature"
THS$Tma<-NULL
THS$var<-"April-August temperature"

THS<-rbind(THS, nTHS)

tiff("Temperatures_Sweden.tiff", width = 3.1, height = 2.5, unit = "in", res = 1000)
ggplot(THS, aes(YR, AvgOfTEMP, group=var, linetype=var, color=var, size=var))+
  geom_line()+
  theme_bw(base_size = 9) +
  theme(axis.title.y=element_text(margin=margin(0,10,0,0),size=8),
        axis.title.x=element_text(margin=margin(8,0,0,0),size=8),
        legend.justification=c(0,0), 
        legend.position=c(0.15,-0.05),
        legend.background = element_rect(fill="transparent"), 
        legend.title=element_blank(),
        legend.key.height=unit(0.5,"line"),
        legend.key = element_blank(),
        legend.key.width=unit(1.5,"line"))+
  scale_x_continuous(breaks=seq(1984, 2014, 5), limits=c(1983,2014))+
  scale_y_continuous(breaks=seq(8, 12, 0.5))+
  scale_linetype_manual(values=c("solid","dashed"))+
  scale_color_manual(values=c("black","dark grey"))+
  scale_size_manual(values=c(0.3,0.7))+
  ylab("Temperature (??C)")+
  xlab("Year")
dev.off()
 
Tcoef<-data.frame(matrix(NA, nrow=6, ncol=2))
lmTNN <- lm(TEMP~YR, data=subset(TEMP, Region=="NN"))
Tcoef[1,]<-c(Region="NN",coef(lmTNN)[2])
lmTSN <- lm(TEMP~YR, data=subset(TEMP, Region=="SN"))
Tcoef[2,]<-c(Region="SN",coef(lmTSN)[2])
lmTSG <- lm(TEMP~YR, data=subset(TEMP, Region=="SG"))
Tcoef[3,]<-c(Region="SG",coef(lmTSG)[2])
lmTVG <- lm(TEMP~YR, data=subset(TEMP, Region=="WGS"))
Tcoef[4,]<-c(Region="WGS",coef(lmTVG)[2])
lmTES <- lm(TEMP~YR, data=subset(TEMP, Region=="ES"))
Tcoef[5,]<-c(Region="ES",coef(lmTES)[2])
lmTEG <- lm(TEMP~YR, data=subset(TEMP, Region=="EG"))
Tcoef[6,]<-c(Region="EG",coef(lmTEG)[2])
colnames(Tcoef)<-c("region","Tslope")
Tcoef$Tslope<-as.numeric(Tcoef$Tslope)
Tcoef$Tslope<-round(Tcoef$Tslope, digits=6)

aT <- merge(a, Tcoef, by="region")

dw2  <-lm(ind_slope~Tslope,subset(aT,indicator=="DW"))
dec2  <-lm(ind_slope~Tslope,subset(aT,indicator=="DEC"))
old2  <-lm(ind_slope~Tslope,subset(aT,indicator=="OLD"))

dw3  <-lm(perc_change~Tslope,subset(aT,indicator=="DW"))
dec3  <-lm(perc_change~Tslope,subset(aT,indicator=="DEC"))
old3  <-lm(perc_change~Tslope,subset(aT,indicator=="OLD"))

win.metafile("Temp_forest02_6point.wmf",width = 4, height = 3)
ggplot(aT, aes(x=Tslope, y=perc_change, label=region, shape=indicator))+
  geom_point(aes(shape=indicator))+
  geom_abline(intercept= coef(dw3)[1], slope= coef(dw3)[2], color=1)+
  geom_abline(intercept= coef(dec3)[1], slope= coef(dec3)[2], color=2)+
  geom_abline(intercept= coef(old3)[1], slope= coef(old3)[2], color=3)+
  geom_text(aes(label=region,hjust=0.5, vjust=-0.7, size=11))+
  theme_bw(base_size = 11)+
  theme(axis.title.y=element_text(margin=margin(0,10,0,0)),axis.title.x=element_text(margin=margin(8,0,0,0)),legend.position="none")+
  scale_y_continuous(limits= c(-20, 100),breaks=seq(-20, 100, 20))+
  xlab("Slope of summer temperature")+
  ylab("% change in forest structure")+
  ggtitle("Temperature - forest change 2002-2014")
dev.off()

win.metafile("Temp_forest03_6point.wmf",width = 4, height = 3)
ggplot(aT, aes(x=Tslope, y=perc_change, label=region, shape=indicator))+
  geom_point(aes(shape=indicator))+
  geom_abline(intercept= coef(dw3)[1], slope= coef(dw3)[2], color=1)+
  geom_abline(intercept= coef(dec3)[1], slope= coef(dec3)[2], color=1)+
  geom_abline(intercept= coef(old3)[1], slope= coef(old3)[2], color=3)+
  geom_text(aes(label=region,hjust=0.5, vjust=-0.7, size=11))+
  theme_bw(base_size = 11)+
  theme(axis.title.y=element_text(margin=margin(0,10,0,0)),axis.title.x=element_text(margin=margin(8,0,0,0)),legend.position="none")+
  scale_y_continuous(limits= c(-20, 100),breaks=seq(-20, 100, 20))+
  xlab("Slope of summer temperature")+
  ylab("% change in forest structure")+
  ggtitle("Temperature - forest change 2003-2014")
dev.off()

win.metafile("TDW6point.wmf",width = 4, height = 3)
ggplot(subset(aT,indicator=="DW"), aes(x=Tslope, y=ind_slope, label=region))+
  geom_point()+
  geom_abline(intercept= coef(dw2)[1], slope= coef(dw2)[2])+
  geom_text(aes(label=region,hjust=0.5, vjust=-0.7, size=11))+
  theme_bw(base_size = 11)+
  theme(axis.title.y=element_text(margin=margin(0,10,0,0)),axis.title.x=element_text(margin=margin(8,0,0,0)),legend.position="none")+
#  scale_x_continuous(limits= c(-0.8, 0),breaks=seq(-0.8, 0, 0.2))+
  scale_y_continuous(limits= c(0.94, 1.06),breaks=seq(0.94, 1.06, 0.02))+
  xlab("Change in summer temperature")+
  ylab("Slope of indicator species")+
  ggtitle("Dead wood")
dev.off()

win.metafile("TDEC6point.wmf",width = 4, height = 3)
ggplot(subset(aT,indicator=="DEC"), aes(x=Tslope, y=ind_slope, label=region))+
  geom_point()+
  geom_abline(intercept= coef(dec2)[1], slope= coef(dec2)[2])+
  geom_text(aes(label=region,hjust=0.5, vjust=-0.7, size=11))+
  theme_bw(base_size = 11)+
  theme(axis.title.y=element_text(margin=margin(0,10,0,0)),axis.title.x=element_text(margin=margin(8,0,0,0)),legend.position="none")+
#  scale_x_continuous(limits= c(-0.8, 0),breaks=seq(-0.8, 0, 0.2))+
  scale_y_continuous(limits= c(0.94, 1.06),breaks=seq(0.94, 1.06, 0.02))+
  xlab("Change in summer temperature")+
  ylab("Slope of indicator species")+
  ggtitle("Broadleaved forest")
dev.off() 

win.metafile("TOLD6point.wmf",width = 4, height = 3)
ggplot(subset(aT,indicator=="OLD"), aes(x=Tslope, y=ind_slope, label=region))+
  geom_point()+
  geom_abline(intercept= coef(old2)[1], slope= coef(old2)[2])+
  geom_text(aes(label=region,hjust=0.5, vjust=-0.7, size=11))+
  theme_bw(base_size = 11)+
  theme(axis.title.y=element_text(margin=margin(0,10,0,0)),axis.title.x=element_text(margin=margin(8,0,0,0)),legend.position="none")+
#  scale_x_continuous(limits= c(-0.8, 0),breaks=seq(-0.8, 0, 0.2))+
  scale_y_continuous(limits= c(0.94, 1.06),breaks=seq(0.94, 1.06, 0.02))+
  xlab("Change in summer temperature")+
  ylab("Slope of indicator species")+
  ggtitle("Old forest")
dev.off()

win.metafile("T586point.wmf",width = 4, height = 3)
ggplot(Tcoef, aes(x=Tchange, y=ind_SF, label=region))+
  geom_point()+
  geom_abline(intercept= coef(sf)[1], slope= coef(sf)[2])+
  geom_text(aes(label=region,hjust=0.5, vjust=-0.7, size=11))+
  theme_bw(base_size = 11)+
  theme(axis.title.y=element_text(margin=margin(0,10,0,0)),axis.title.x=element_text(margin=margin(8,0,0,0)),legend.position="none")+
  scale_x_continuous(limits= c(-0.8, 0),breaks=seq(-0.8, 0, 0.2))+
  scale_y_continuous(limits= c(-0.06, 0.08),breaks=seq(-0.06, 0.08, 0.02))+
  xlab("Change in summer temperature")+
  ylab("Slope of species")+
  ggtitle("All forest species")
dev.off()

win.metafile("T426point.wmf",width = 4, height = 3)
ggplot(Tcoef, aes(x=Tchange, y=ind_NI, label=region))+
  geom_point()+
  geom_abline(intercept= coef(ni)[1], slope= coef(ni)[2])+
  geom_text(aes(label=region,hjust=0.5, vjust=-0.7, size=11))+
  theme_bw(base_size = 11)+
  theme(axis.title.y=element_text(margin=margin(0,10,0,0)),axis.title.x=element_text(margin=margin(8,0,0,0)),legend.position="none")+
  scale_x_continuous(limits= c(-0.8, 0),breaks=seq(-0.8, 0, 0.2))+
  scale_y_continuous(limits= c(-0.06, 0.08),breaks=seq(-0.06, 0.08, 0.02))+
  xlab("Change in summer temperature")+
  ylab("Slope of indicator species")+
  ggtitle("Generalists")
dev.off()

win.metafile("T166point.wmf",width = 4, height = 3)
ggplot(Tcoef, aes(x=Tchange, y=ind_LS, label=region))+
  geom_point()+
  geom_abline(intercept= coef(ls)[1], slope= coef(ls)[2])+
  geom_text(aes(label=region,hjust=0.5, vjust=-0.7, size=11))+
  theme_bw(base_size = 11)+
  theme(axis.title.y=element_text(margin=margin(0,10,0,0)),axis.title.x=element_text(margin=margin(8,0,0,0)),legend.position="none")+
  scale_x_continuous(limits= c(-0.8, 0),breaks=seq(-0.8, 0, 0.2))+
  scale_y_continuous(limits= c(-0.06, 0.08),breaks=seq(-0.06, 0.08, 0.02))+
  xlab("Change in summer temperature")+
  ylab("Slope of indicator species")+
  ggtitle("Specialists")
dev.off()


###### total plots ############################
ALL<-ggplot(LövrikskogHS, aes(År, Index))+
  geom_ribbon(data=LövrikskogHS,aes(ymin=X,ymax=X.1),fill="grey")+
  geom_line(data=LövrikskogHS, size=1)+
  ylim(0, 3) +  
  xlab("Year")+
  scale_x_continuous(breaks=seq(1998, 2014, 2))+
  theme_classic(base_size = 10) +
  theme(plot.title=element_text(size=10),plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))+
  geom_hline(aes(yintercept=3), colour="#000000")+
  geom_hline(aes(yintercept=2.5), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1.5), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0.5), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  ggtitle("Broad-leaved species")

win.metafile("indicator3.wmf",width = 3.8, height = 2.8)
ggplot(DödvedHS, aes(År, Index))+
  geom_ribbon(data=DödvedHS,aes(ymin=X,ymax=X.1),fill="grey")+
  geom_line(data=DödvedHS, size=1)+
  ylim(0, 4) +  
  xlab("Year")+
  scale_x_continuous(breaks=seq(1998, 2014, 2))+
  theme_classic(base_size = 10) +
  theme(plot.title=element_text(size=10),plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))+
  geom_hline(aes(yintercept=4), colour="#000000")+
  geom_hline(aes(yintercept=3.5), colour="#000000")+
  geom_hline(aes(yintercept=3), colour="#000000")+
  geom_hline(aes(yintercept=2.5), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1.5), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0.5), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  ggtitle("Dead wood species")
dev.off()

SkogsfåglarNonIndHS<-read.csv("Sweden_Gen, Trend in populatieaantal_RESULTS.csv", sep=";",dec=",")
SkogsfåglarNonIndHS$simMSImean<-NULL
colnames(SkogsfåglarNonIndHS)<-c("År","Index","Lower","Upper")
SkogsfåglarNonIndHS$Index<-SkogsfåglarNonIndHS$Index/100
SkogsfåglarNonIndHS$Lower<-SkogsfåglarNonIndHS$Lower/100
SkogsfåglarNonIndHS$Upper<-SkogsfåglarNonIndHS$Upper/100

win.metafile("Totalbirdindicators_smoothed_resized.wmf",width = 2.79, height = 2)
ggplot(LevandeskogHS, aes(År, Index))+
#   geom_ribbon(data=LevandeskogHS,aes(ymin=Lower,ymax=Upper), fill="grey80")+
#  geom_ribbon(data=SkogsfåglarNonIndHS,aes(ymin=X,ymax=X.1), fill="grey65")+
#  geom_line(data=LevandeskogHS, aes(År, X), linetype="solid", colour="grey")+
#  geom_line(data=LevandeskogHS, aes(År, X.1), linetype="solid", colour="grey")+
#  geom_line(data=SkogsfåglarNonIndHS, aes(År, X), linetype="dotted", colour="grey")+
#  geom_line(data=SkogsfåglarNonIndHS, aes(År, X.1), linetype="dotted", colour="grey")+
  geom_line(data=SkogsfåglarNonIndHS, aes(linetype="Generalists"))+
#  geom_point(data=SkogsfåglarNonIndHS, size=3, shape=18)+
  geom_line(data=LevandeskogHS, aes(linetype="Specialists"))+   
#  geom_point(data=LevandeskogHS, size=3, shape=17)+   
  xlab("Year")+
  scale_x_continuous(limits= c(1998,2015),breaks=seq(1995, 2015, 5))+
  scale_y_continuous(limits= c(0,2),breaks=seq(0,2, 0.5),expand = c(0, 0))+
  theme_bw(base_size = 9) +  
  theme(axis.title.y=element_text(margin=margin(0,10,0,0),size=8),axis.title.x=element_text(margin=margin(8,0,0,0),size=8), plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"),legend.title=element_blank(),legend.position=c(0.8,0.9),legend.background = element_rect(fill="transparent"),legend.key = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.width=unit(1,"line"))
dev.off()

tiff("Totalbirdindicators_poster.tiff",width = 4, height = 3, units = 'in',res=1000)
ggplot(LevandeskogHS, aes(År, Index))+
  geom_ribbon(data=SkogsfåglarNonIndHS,aes(ymin=Lower,ymax=Upper),alpha=0.6, fill="grey")+
  geom_ribbon(data=LevandeskogHS,aes(ymin=Lower,ymax=Upper),alpha=0.6, fill="grey")+
  geom_line(data=LevandeskogHS, aes(År, Upper), linetype="dashed", colour="dark grey", size=0.2)+
  geom_line(data=LevandeskogHS, aes(År, Lower), linetype="dashed", colour="dark grey", size=0.2)+
  geom_line(data=SkogsfåglarNonIndHS, aes(År, Upper), linetype="solid", colour="dark grey", size=0.2)+
  geom_line(data=SkogsfåglarNonIndHS, aes(År, Lower), linetype="solid", colour="dark grey", size=0.2)+
  geom_line(data=SkogsfåglarNonIndHS, aes(linetype="Generalists"))+
  geom_line(data=LevandeskogHS, aes(linetype="Specialists"))+   
  xlab("Year")+
  ylab("Population trend")+
  scale_x_continuous(limits= c(1998,2015),breaks=seq(1995, 2015, 5))+
  scale_y_continuous(limits= c(0,2),breaks=seq(0,2, 0.5),expand = c(0, 0))+
  theme_bw(base_size = 10) +  
  theme(axis.title.y=element_text(margin=margin(0,10,0,0),size=9),
        axis.title.x=element_text(margin=margin(8,0,0,0),size=9), 
        plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"),
        legend.title=element_blank(),
        legend.position=c(0.8,0.9),
        legend.background = element_rect(fill="transparent"),
        legend.key = element_blank(),
        legend.key.height=unit(1,"line"),
        legend.key.width=unit(1,"line"))
dev.off()

win.metafile("ResidentsMigrants.wmf",width = 3.8, height = 2.8)
ggplot(LevandeskogHS, aes(År, Index))+
  geom_line(data=Residents, size=1, linetype="dashed")+
  geom_line(data=Migrants, size=1)+   
  xlab("Year")+
  scale_x_continuous(limits= c(1997,2015),breaks=seq(1997, 2015, 2))+
  scale_y_continuous(limits= c(0,2.5),breaks=seq(0,2.5, 0.5),expand = c(0, 0))+
  theme_classic(base_size = 10) +  
  theme(axis.title.y=element_text(vjust=1.5),axis.title.x=element_text(vjust=0), plot.title=element_text(size=10),plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))+
  geom_hline(aes(yintercept=2.5), colour="#000000")+
  geom_hline(aes(yintercept=2), colour="#000000")+
  geom_hline(aes(yintercept=1.5), colour="#000000")+
  geom_hline(aes(yintercept=1), colour="#000000")+
  geom_hline(aes(yintercept=0.5), colour="#000000")+
  geom_hline(aes(yintercept=0), colour="#000000")+
  ggtitle("Resident species and migrant species indicators")
dev.off()
