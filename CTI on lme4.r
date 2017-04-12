### This code calculates statistics for CTI data

# Make sure you have an Excel file that has the three columns: SITES, YR, CTI, that is, one CTI value per site and year. 
# CTI can be calculated using queries in Access.
# Include only the data from years when a given site was counted. There is no problem if there are missing data for a given site and year. 

rm(list=ls())						                  #Empties the system, always start a code like this

### Setting the directory for data

setwd('C:/Users/Ägaren/Documents/R/CTI')	# WORK: This is where the files are looked for
#setwd('C:/Users/Åke/Documents/R/CTI')		# HOME: This is where the files are looked for
getwd()							                      # Shows you that you got the right working directory

### Get data from Excel-file                                  

library(RODBC)				                          # Loads the necessary module to import data
channel <- odbcConnectExcel("CTI Fjäll.xls")          # Opens a channel to an Excel file
sqlTables(channel) 					                    # Shows which tables (worksheets) are in the database
sqlColumns(channel, 'Fjällen')				              # Shows which columns are in a certain table (worksheet)
CTI.dat <- sqlQuery(channel,				            # Creates an object (dataframe) with all rows of a given worksheet 
  "select * from \"Fjällen$\"")			              #   This is the object (dataframe) that R will work with
CTI.dat$SITES<- factor(CTI.dat$SITES)           # Make the variable SITES into a factor
CTI.dat$fYR<- factor(CTI.dat$YR)                # Make the variable YR into a factor
str(CTI.dat) 						                        # Shows object (dataframe) structure
close(channel) 						                      # You don't need this channel anymore, you have the data in the object


library(lme4)				                            # Load the module necessary for the statistics
#library(nlme)                                  # An alternative is the somewhat more basic nlme-package
library(effects)                                # Load the module necessary to plot model effects  

### Estimate the trend over time
  
# This can be done in two slightly different ways. The second is probably the most correct one. One can potentially also use AIC values to decide which is best.
# The first model is a mixed linear regression with SITES as random effect: a random intercept model (Vincent's suggestion).
# Devictor i mail 130826: In the R model we can also ask for random slope and intercept (year I site instead of 1 I site).
                                                                                                 
modSlope1<- lmer(CTI~YR +(1|SITES), data=CTI.dat)       # A random intercept model, where YR is a covariate, SITES is a random factor

# The second model is a mixed linear regression with SITES as random effect, a random intercept and random slope model (Henrik's suggestion).
 
modSlope2<- lmer(CTI~YR +(1 + YR|SITES), data=CTI.dat)  # A random intercept and random slope model, where YR is a covariate, SITES is a random factor

summary(modSlope1)                                        # The statistics output of the first model
summary(modSlope2)                                        # The statistics output of the second model

# You do not get a p-value... Use se of slope and multiply with 1.96 for 95% CI and so on. Alternatively, do the Likelihood test below

### Estimate the yearly means, with YR taken as a factor
                                                                                                 
modYearlyMean<- lmer(CTI~fYR+(1|SITES), data=CTI.dat)  # YR is a fixed factor (fYR), SITES is a random factor
                                                                    
summary(modYearlyMean)                                 # The statistics output of the model, but you can normally go directly to the last part of output (data.frame)

# The first year's mean is the value given by the intercept, say 12.020
# You then add each yearly value (e.g. 0.014) to the intercept (12.020) and get that year's value (e.g. 12.02 + 0.014 = 12.034)
# The first SE is the SE of the intercept. The avergage value of CTI for the first year.
# The following estimates are for the DIFFERENCE between the first year and the year considered. So is the SE.  
# So from this you can set the SE of the intercept to zero and directly use the SE of the following years as representing the SE of the yearly "changes".                                                                   

summary(effYR<- effect('fYR', modYearlyMean))             # More info than from the Summary command, through package Effects.
plot(effYR)                                               # Gives a nice graph with 95%CI
data.frame(effYR)                                         # Gives the crucial values, including yearly values and 95%CI (see the graph drawn). Copy and paste the wanted values into Excel

### Likelihood test, to test if YR has a significant effect on the CTI values
# Note that modeSlope0 and modSlope1 should ideally be run in ML mode (default, as above, is REML)

modSlope0<- lmer(CTI~1 +(1|SITES), data=CTI.dat)          # This is modelSlope1 of above, but with YR as covariate removed
summary(modSlope0)
anova(modSlope1, modSlope0)

