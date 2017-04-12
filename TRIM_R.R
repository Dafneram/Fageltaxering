library(RegionalGAM)
library(nlme)
library(MASS)

data("gatekeeper_CM")
head(gatekeeper_CM)
dataset1 <- gatekeeper_CM[,c("SPECIES","SITE","YEAR","MONTH","DAY","COUNT")]

# compute the annual flight curve, you might get yourself a coffee as this might take some time.
pheno <- flight_curve(dataset1)

# plot pheno for year 2005
plot(pheno$DAYNO[pheno$year==2005],pheno$nm[pheno$year==2005],pch=19,cex=0.7,type='o',col='red',xlab="day",ylab="relative abundance")

dataset2 <- gatekeeper_CM[gatekeeper_CM$TREND==1,c("SPECIES","SITE","YEAR","MONTH","DAY","COUNT")]
data.index <- abundance_index(dataset2, pheno)

# compute collated annual indices
glmm.mod_fullyear <- glmmPQL(regional_gam~ as.factor(YEAR)-1,data=data.index,family=quasipoisson,random=~1|SITE, correlation = corAR1(form = ~ YEAR | SITE),verbose = FALSE)
summary(glmm.mod_fullyear)

# extract collated index and plot against years
col.index <- as.numeric(glmm.mod_fullyear$coefficients$fixed)
year <- unique(data.index$YEAR)
plot(year,col.index,type='o', xlab="year",ylab="collated index")

# model temporal trend with a simple linear regression
mod1 <- gls(col.index ~ year)
summary(mod1)

# check for temporal autocorrelation in the residuals
acf(residuals(mod1,type="normalized"))

# adjust the model to account for autocorrelation in the residuals
mod2 <- gls(col.index ~ year, correlation = corARMA(p=2))
summary(mod2)

# check for remaining autocorrelation in the residuals
acf(residuals(mod2,type="normalized"))

# plot abundance with trend line
plot(year,col.index, type='o',xlab="year",ylab="collated index")
abline(mod2,lty=2,col="red")
