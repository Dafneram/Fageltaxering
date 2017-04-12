setwd("C:/Users/Dafne/Desktop/Svensk Fågeltaxering/Databases and TRIM/Fågeldata")
library(dplyr)
library(lme4)  
library(effects) 

Years <- read.csv("Forest indicator routes surveyed 1998-2015.csv", sep=",", dec=",")
Routes <- read.csv("Forest_1998-2015_16.csv", sep=",", dec=",")
Routes$lind<-1
Routes <- merge(Routes, Years, by=c("karta", "yr","lan"), all.y=TRUE)
Routes$lind[is.na(Routes$lind)] <- 0
Routes      = group_by(Routes, karta, yr)
Richness    = summarise(Routes, richness=sum(lind))

##### TRIM files with -1 for not surveyed years #####
#karta<- data.frame(levels(Richness$karta))
#karta<- data.frame(rep(karta$levels.Richness.karta., each = 13))
#addYR<-data.frame(rep(2002:2014,times = 667))
#addYR<-(cbind(karta,addYR))

#colnames(addYR)<-c("karta", "yr")
#RichnessT<- merge(Richness, addYR, by=c("karta","yr"), all.y=TRUE)
#RichnessT$richness[is.na(RichnessT$richness)] <- -1
#RichnessT$N <- as.integer(RichnessT$karta)
#RichnessT<-RichnessT[,c(4,2,3)]
#write.table(RichnessT, "RichnessT.txt", sep="\t", row.names=FALSE, col.names=FALSE)

#SR.TRIM<-read.csv("RichnessOUT.csv", sep=",", dec=".")
#plot(SR.TRIM$yr,SR.TRIM$index)

#"Northern Norrland" -> Richness$Region[Richness$lan == "BD"|Richness$lan=="AC"|Richness$lan=="Z"]
#"Southern Norrland" -> Richness$Region[Richness$lan=="W"|Richness$lan=="X"|Richness$lan=="Y"]
#"Eastern Svealand" -> Richness$Region[Richness$lan=="D"|Richness$lan=="T"|Richness$lan=="U"|Richness$lan=="AB"|Richness$lan=="C"]
#"Western Göta- and Svealand" -> Richness$Region[Richness$lan=="O"|Richness$lan=="S"]
#"Eastern Götaland" -> Richness$Region[Richness$lan=="G"|Richness$lan=="H"|Richness$lan=="F"|Richness$lan=="I"|Richness$lan=="E"]
#"Southern Götaland" -> Richness$Region[Richness$lan=="M"|Richness$lan=="N"|Richness$lan=="K"]


########
colnames(Richness)<-c("SITES", "YR", "SR")
Richness$YR<-Richness$YR-2006
Richness$fYR<-factor(Richness$YR)


### Estimate the trend over time

# This can be done in two slightly different ways. The second is probably the most correct one. One can potentially also use AIC values to decide which is best.
# The first model is a mixed linear regression with SITES as random effect: a random intercept model (Vincent's suggestion).
# Devictor i mail 130826: In the R model we can also ask for random slope and intercept (year I site instead of 1 I site).

modSlope1<- lmer(SR~YR +(1|SITES), data=Richness)       # A random intercept model, where YR is a covariate, SITES is a random factor

# The second model is a mixed linear regression with SITES as random effect, a random intercept and random slope model (Henrik's suggestion).

modSlope2<- lmer(SR~YR +(1 + YR|SITES), data=Richness)  # A random intercept and random slope model, where YR is a covariate, SITES is a random factor

summary(modSlope1)                                        # The statistics output of the first model
summary(modSlope2)                                        # The statistics output of the second model

# You do not get a p-value... Use se of slope and multiply with 1.96 for 95% CI and so on. Alternatively, do the Likelihood test below

### Estimate the yearly means, with YR taken as a factor

modYearlyMean<- lmer(SR~fYR+(1|SITES), data=Richness)  # YR is a fixed factor (fYR), SITES is a random factor

summary(modYearlyMean)                                 # The statistics output of the model, but you can normally go directly to the last part of output (data.frame)

# The first year's mean is the value given by the intercept, say 12.020
# You then add each yearly value (e.g. 0.014) to the intercept (12.020) and get that year's value (e.g. 12.02 + 0.014 = 12.034)
# The first SE is the SE of the intercept. The avergage value of CTI for the first year.
# The following estimates are for the DIFFERENCE between the first year and the year considered. So is the SE.  
# So from this you can set the SE of the intercept to zero and directly use the SE of the following years as representing the SE of the yearly "changes".                                                                   

summary(effYR<- effect('fYR', modYearlyMean))             # More info than from the Summary command, through package Effects.
plot(effYR)                                               # Gives a nice graph with 95%CI
data.frame(effYR)                                         # Gives the crucial values, including yearly values and 95%CI (see the graph drawn). Copy and paste the wanted values into Excel
years_16<-data.frame(effYR)                                         
#years_42<-data.frame(effYR)                                         
#years_all<-data.frame(effYR)                                         


### Likelihood test, to test if YR has a significant effect on the CTI values
# Note that modeSlope0 and modSlope1 should ideally be run in ML mode (default, as above, is REML)

modSlope0<- lmer(SR~1 +(1|SITES), data=Richness)          # This is modelSlope1 of above, but with YR as covariate removed
summary(modSlope0)
anova(modSlope1, modSlope0)


#assumptions
plot(fitted(modSlope2),residuals(modSlope2))
hist(residuals(modSlope2))
qqnorm(residuals(modSlope2))

plot(fitted(modYearlyMean),residuals(modYearlyMean))
hist(residuals(modYearlyMean))
qqnorm(residuals(modYearlyMean))

#plot
win.metafile("SReffYR.wmf",width = 5, height = 3.5)
plot(effYR, cex=0.8)
dev.off()

options(digits = 10)

library(reshape2)
library(ggplot2)

#### index figures ####
years_16$index<-((years_16$fit*100)/3.0905965)/100
years_42$index<-((years_42$fit*100)/16.80821643)/100
years_all$index<-((years_all$fit*100)/19.88373746)/100
years_index<-data.frame(cbind(years_16$fYR,years_16$index, years_42$index, years_all$index))
years_index$X1<-1998:2015

colnames(years_index)<-c("Year","Specialist species","Generalist species","All forest species")
years_index<-melt(years_index, id="Year")
years_index$variable <- factor(years_index$variable, levels = c("Generalist species","All forest species","Specialist species"))

win.metafile("richness_comb.wmf",width = 6.5, height = 3.5)
ggplot(years_index, aes(x=Year, y=value, group=variable, shape=variable)) + 
  geom_abline(intercept= coef(lm16)[1], slope= coef(lm16)[2], linetype=2, colour="grey33")+
  geom_abline(intercept= coef(lm42)[1], slope= coef(lm42)[2], linetype=2, colour="grey33")+
  geom_abline(intercept= coef(lm58)[1], slope= coef(lm58)[2], linetype=2, colour="grey33")+
  geom_point() +
  geom_line() +
  xlab("Year") + ylab("Index") +
  scale_x_continuous(limits=c(1997,2015),breaks=seq(1997,2015,2)) +
  theme_bw(base_size = 11) +
  theme(axis.title.y=element_text(vjust=1.5),axis.title.x=element_text(vjust=0),legend.key = element_blank(),legend.title=element_blank())+
  ggtitle("Species richness")
dev.off()

#### absolute data figures ####
years_16$fYR<-1998:2015
years_42$fYR<-1998:2015
years_all$fYR<-1998:2015

tiff("richness_a_resized.tiff",width = 2.79, height = 2, units="in", res=1000)
ggplot(years_all, aes(x=fYR, y=fit)) + 
#  geom_abline(intercept= coef(lm58f)[1], slope= coef(lm58f)[2], linetype=2, colour="grey33")+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill="grey")+
#  geom_point() +
  geom_line() +
  xlab("Year") + ylab("Number of forest species") +
  scale_x_continuous(limits=c(1997,2015.3),breaks=seq(1997,2015,3)) +
  scale_y_continuous(limits=c(18.8,23.21),breaks=seq(19,23,1)) +
  theme_bw(base_size = 9) +
  theme(axis.title.y=element_text(margin=margin(0,10,0,0),size=8),axis.title.x=element_text(margin=margin(8,0,0,0),size=8),legend.key = element_blank(),legend.title=element_blank())+
  annotate("text", x = 1997.3, y=23.1, label = "(a)", size=3)
dev.off()

tiff("richness_b_resized.tiff",width = 2.79, height = 2, units="in", res=1000)
ggplot(years_16, aes(x=fYR, y=fit)) + 
#  geom_abline(intercept= coef(lm16f)[1], slope= coef(lm16f)[2], linetype=2, colour="grey33")+
#  geom_point() +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill="grey")+
  geom_line() +
  xlab("Year") + ylab("Number of specialist species") +
  scale_x_continuous(limits=c(1997,2015.3),breaks=seq(1997,2015,3)) +
  scale_y_continuous(limits=c(0.8,5.21),breaks=seq(1,5,1)) +
  theme_bw(base_size = 9) +
  theme(axis.title.y=element_text(margin=margin(0,10,0,0),size=8),axis.title.x=element_text(margin=margin(8,0,0,0),size=8),legend.key = element_blank(),legend.title=element_blank())+
  annotate("text", x = 1997.3, y=5.1, label = "(b)", size=3)
dev.off()

tiff("richness_c_resized.tiff",width = 2.79, height = 2, units="in", res=1000)
ggplot(years_42, aes(x=fYR, y=fit)) + 
#  geom_abline(intercept= coef(lm42f)[1], slope= coef(lm42f)[2], linetype=2, colour="grey33")+
#  geom_point() +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5, fill="grey")+
  geom_line() +
  xlab("Year") + ylab("Number of generalist species") +
  scale_x_continuous(limits=c(1997,2015.3),breaks=seq(1997,2015,3)) +
  scale_y_continuous(limits=c(15.8,20.21),breaks=seq(16,20,1)) +
  theme_bw(base_size = 9) +
  theme(axis.title.y=element_text(margin=margin(0,10,0,0),size=8),axis.title.x=element_text(margin=margin(8,0,0,0),size=8),legend.key = element_blank(),legend.title=element_blank())+
  annotate("text", x = 1997.3, y=20.1, label = "(c)", size=3)
dev.off()


