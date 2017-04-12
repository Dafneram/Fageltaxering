### This code draws graphs for the combined SommarPKT and Standard

rm(list=ls())						                   #Empties the system, always start like this

  ##Setting the directory for data##

setwd('C:/Users/Åke/Documents/R/SFT')		# HOME: This is where the files are looked for
#setwd('C:/Users/Ägaren/Documents/R/SFT')	# WORK: This is where the files are looked for
getwd()							                      # Shows you that you got the right working directory

  ##Get data from Excel-file##                                    

library(RODBC)				                          # Loads the necessary module 
channel <- odbcConnectExcel("Trimkomb2014.xls") # Opens a channel to a certain database
sqlTables(channel) 					                    # Shows which tables (worksheets) are in the database
sqlColumns(channel, 'TRIMKOMB')				          # Shows which columns are in a certain table (worksheet)
trim.dat <- sqlQuery(channel,				            # Creates an object with all rows of a given worksheet 
   "select * from \"TRIMKOMB$\"")			              # SUMMER
str(trim.dat) 						                      # Shows object structure
close(channel) 						                      # You don't need this channel anymore, you have the data in the object

artno <- unique(trim.dat$ART)				            # Creates an object with all unique ART ("unique" when integer)
artno								                            # Lets you see all the unique values (in this case, species no)
  
  #############################
  ##Creates individuals plots##
  #############################
  								
for(i in 1:length(artno)){				                           # Starts a loop to do all species in artno, from the first to the last. Note the "{"!
  onespecies <-						                                   # Creates an object with selected data for one species at a time, including rows 
	   trim.dat[trim.dat$ART==artno[i], ]		                   #   where ART is 'i' and all columns for these rows (the .. ",  " part at the end of the []	
  rubrik1 <- as.character(onespecies$ArtRad1[1])             # Takes the first value (they are all the same) in column ArtRad1 for this species
  rubrik2 <- as.character(onespecies$ArtRad2[38])            # Takes the last value of column ArtRad2 - the number has to be changed when new year is added
  pos1 <- regexpr(', ', rubrik1)                             # The objects assigned at the next five rows are to get scientific name in italics
  before <- substring(rubrik1, first=1, last=pos1+1)
  latin <- substring(rubrik1, first=pos1+2)
  maxindex <- max(c(onespecies$IndexS, onespecies$IndexT), na.rm=T)                                          # Takes the highest of both 
  

win.metafile(filename = paste(paste(paste('TrimSpecies_komb/',	# Saves a wmf-file in dir /TrimSpecies_standard/
							 				                                              # The paste function creates a string. 
	gsub('.', '', onespecies$Artnamn[1],				                      # gsub replaces "period" with "blank" (St. hackspett...)
											                                              # onespecies$Artnamn[1] means that it takes the first value in column Artnamn	
 fixed=T),sep=""), "_komb2014", sep=""), ".wmf", sep=""),	      #   sep="" removes the default "blank" when pasting strings 
	width = 4, 									                                      # The plot is 4 inches wide
	height = 3,									                                      # The plot os 3 inches high 
	pointsize = 12,
      restoreConsole = TRUE)


op <- par(las=1, mar = c(2, 2.5, 2.5, 1)+0.1,	                      # This line sets some general values for graphs and saves the old values in op
								                                                  # las=1 makes the y-axis labels sit horisontally in the coming plots 
                                                  								# mar sets the margins around the graph, 
	bg = 'white', mgp = c(3, 0.7, 0), cex.axis = 1.0)		          # mgp sets the distance	between ticks and axis labels (especially the 0.7)
                                                  								# cex.axis sets the proportional size of axis labels (to default = 1.0)
if (maxindex<=2.5){					                                      # This loop decides the value of the highest tick mark of the y-axis
	ymax <- ceiling((maxindex+0.0001)*2)/2	                        # If the highest (TRIM)Index is <=2.5, 
	ytix <- seq(0,ymax,0.5)				                                  #  a special set of y-axis settings is used
} else ymax <- ceiling(maxindex)			                            #  otherwise the default rounding upwards (ceiling) is used

plot(onespecies$Yr, onespecies$IndexS,		                        # Creates a basic xy-plot 
  type = 'n',						                                          #   with no data shown ('n'). Data is added through "lines()" below						
	bty = 'c',                                           						# The box (lines) around the plot has a C-shape
	yaxt = 'n',                                            						# Suppresses the y-axis plotting (we need to specify too many details, see below)
	xaxt = 'n',		                                            				# Suppresses the x-axis plotting (we need to specify too many details, see below)	
	ylim = c(0, ymax),                                             	# Sets the limits to the y-axis (from 0 to "max"). ymax is defined above,
	                                                   							#   differently depending on if highest Index is higher or lower than 2.5
								 
	xlim = c(1975,2015),				    # Sets the limits to the x-axis
	yaxs = 'i',						          # Removes the default "extra" space on the y-axis outside the set min/max
	xlab = '',						          # Removes x-axis legend
	ylab = '')						          # Removes y-axis legend

title(main= list(substitute(plain(before)		# Edits the main title, not least, put scientific name in italics 
	*italic(latin),		        	              #  strings before, latin and after are defined above directly after the for loop starts 
	list(before=before, latin=latin)), 
	font = 1, cex = 1), line = 2)		          # Default font type is 2 (=bold). "line" is position of title
                              							#   in no of lines from upper limit of plot
mtext(rubrik2, line = 0.5, font = 1)          # Adds a second line of title

lines(onespecies$Yr, onespecies$IndexS,	  	# Draws a line for SomPKT through the data points
	lwd = 3.0, col = 'Red')			              #  with double thickness and specified colour
lines(onespecies$Yr, onespecies$IndexT,	  	# Draws a line for Standard through the data points
	lwd = 3.0, col = 'Black')  			
axis(1, at=seq(1975,2015,5), labels = TRUE,	# The x-axis (=1)is added here and label sequence specified, 
	tick = TRUE)                     					#  (1975, 1980, 1985, ..., 2015)
if (maxindex>2.5){		                			# If the highest Index is above 2.5, 
	axis(2)					                         	#  default values for the y-axis (=2) ticks are used
	nyv <- NULL                    						#  NULL is default for grid lines (nyv is used below)
	} else {						                      # If the highest Index is below 2.5, we need special tick settings
 	axis(2, at=ytix,                 					# The y-axis ticks and labels' (ytix) sequence are specified above "seq(0,ymax,0.5)"
		labels = TRUE, tick = TRUE)
	nyv <- length(ytix)-1				              #  using the highest value of ytix (defined above) and minus 1 (why?)
	}
grid(nx = NA, ny = nyv,					            # Adds grid lines, where NA for x-axis is "none" and nyv for y-axis
                            								# nyv is defined further up, as either NULL (default)	or   
	  col = "black", lty = "solid",		        #  sets colour and type of grid line
     	  lwd = "1") 				                 	#  and line width

par(op)
dev.off()
}								                            # Closes the 'for loop'


  ###########################
  ## Creates group plots   ##
  ###########################

cix <- c(1,9,19,29,39,49,59,69,79,89,99,109,	# Here are defined when there should be a new graph drawn, 
	119,129,139,149,159,169,179,189,199,209)	  #   at the i-th species

k <- 1				                          			# k is a counter that refers to the k-th position in the cix vector
	                               							#   i.e. when k=1, i=1, when k=2, i=9, when k=3, i=19, and so on
                              								#   k is set to 1. When k=2 the first file (with 8 graphs) is closed

for(i in 1:length(artno)){		                	  	 # Starts a loop to do all species in artno, from the first to the last. Note the "{"!
  onespecies <-						                                   # Creates an object with selected data for one species at a time, including rows 
	   trim.dat[trim.dat$ART==artno[i], ]		                   #   where ART is 'i' and all columns for these rows (the .. ",  " part at the end of the []	
  rubrik1 <- as.character(onespecies$ArtRad1[1])             # Takes the first value (they are all the same) in column ArtRad1 for this species
  rubrik2 <- as.character(onespecies$ArtRad2[38])            # Takes the last value of column ArtRad2 - the number has to be changed when new year is added
  pos1 <- regexpr(', ', rubrik1)                             # The objects assigned at the next five rows are to get scientific name in italics
  before <- substring(rubrik1, first=1, last=pos1+1)
  latin <- substring(rubrik1, first=pos1+2)
  maxindex <- max(c(onespecies$IndexS, onespecies$IndexT), na.rm=T)                                          # Takes the highest of both 

if(i==cix[k]){			   			 # Here we set some important variables for number of species plots in a file
	hit <- 10.8					     	 # The height in inches for the 5 x 2 plot files ('hit' is found further down)
	rad <- 5						       # The number of rows of species plots in one plot file
		if (k==1){					     # Special case for the first plot with only 8 species plots (when k = 1)
			hit = 8.6
			rad <- 4
		}

	win.metafile(filename = paste('TrimGroup_komb/',	  # SUMMER: Saves a wmf-file in dir /TrimGroup_summer/
							 			                                    # The paste function creates a string, naming the file. 
	'Page', k,'.wmf', sep=""),					                  #  
	width = 7.2, 							                            # The plot is 7.2 inches wide
	height = hit,                                        	# The plot is 'hit' inches high ('hit' is defined above)
	pointsize = 12,
      restoreConsole = TRUE)							
      
	par(mfrow=c(rad,2),las=1, 				                    # This line sets some general values for graphs
	mar = c(2, 3, 4, 1)+0.1,			                        #   las=1 makes the y-axis labels sit horisontally in the coming plots 
                                								        #   mar sets the margins around the graph, 
	bg = 'white', mgp = c(3, 0.7, 0), cex.axis = 1.0)  		#   mgp sets the distance between ticks and axis labels (especially the 0.7)
	k <- k+1
}
                                								# cex.axis sets the proportional size of axis labels (to default = 1.0)
if (maxindex<=2.5){					                    # This loop decides the value of the highest tick mark of the y-axis
	ymax <- ceiling((maxindex+0.0001)*2)/2	      # If the highest (TRIM)Index is <=2.5, 
	ytix <- seq(0,ymax,0.5)				                #  a special set of y-axis settings is used
} else ymax <- ceiling(maxindex)			          #  otherwise the default rounding upwards (ceiling) is used

plot(onespecies$Yr, onespecies$IndexS,		        # Creates a basic xy-plot 
	type = 'n',                        						#   with no data shown ('n'). Data is added through "lines()" below						
	bty = 'c',						                        # The box (lines) around the plot has a C-shape
	yaxt='n',	                           					# Suppresses the y-axis plotting (we need to specify too many details, see below)
	xaxt='n',                          						# Suppresses the x-axis plotting (we need to specify too many details, see below)	
	ylim = c(0, ymax),				                    # Sets the limits to the y-axis (from 0 to "max"). ymax is defined above,
                              	   							#   differently depending on if highest Index is higher or lower than 2.5
								 
	xlim = c(1975,2015),				                  # Sets the limits to the x-axis
	yaxs = 'i',	                         					# Removes the default "extra" space on the y-axis outside the set min/max
	xlab = '',						                        # Removes x-axis legend
	ylab = '')	                        					# Removes y-axis legend

title(main= list(substitute(plain(before)		# Edits the main title, not least, put scientific name in italics 
	*italic(latin),		        	              #  strings before, latin and after are defined above directly after the for loop starts 
	list(before=before, 
	latin=latin)), 
	font = 1, cex = 1.3), line = 2.4)		              # Default font type is 2 (=bold). "line" is position of title
                              							    #   in no of lines from upper limit of plot
mtext(rubrik2, line = 0.5, font = 1, cex = 0.8)   # Adds a second line of title
lines(onespecies$Yr, onespecies$IndexS,		      # Draws a line through the data points
	lwd = 3.0, col = 'Red')				                #  with double thickness and specified colour
lines(onespecies$Yr, onespecies$IndexT,	  	# Draws a line for Standard through the data points
	lwd = 3.0, col = 'Black')  	
axis(1, at=seq(1975,2015,5), labels = TRUE,	    # The x-axis (=1)is added here and label sequence specified, 
	tick = TRUE)					                        #  (1975, 1980, 1985, ..., 2015)
if (maxindex>2.5){					                    # If the highest Index is above 2.5, 
	axis(2)						                            #  default values for the y-axis (=2) ticks are used
	nyv <- NULL						                        #  NULL is default for grid lines (nyv is used below)
	} else {						                          # If the highest Index is below 2.5, we need special tick settings
 	axis(2, at=ytix, 					                    # The y-axis ticks and labels' (ytix) sequence are specified above "seq(0,ymax,0.5)"
		labels = TRUE, tick = TRUE)
	nyv <- length(ytix)-1			                   	#  using the highest value of ytix (defined above) and minus 1 (why?)
	}
grid(nx = NA, ny = nyv,					                # Adds grid lines, where NA for x-axis is "none" and nyv for y-axis
                                								# nyv is defined further up, as either NULL (default)	or   
	  col = "black", lty = "solid",		            #  sets colour and type of grid line
     	  lwd = "1") 				                     	#  and line width

if(i==cix[k]-1) dev.off()                       # Closes the file when it has 10 (or 8) plots in it
}			
dev.off()                                       # Closes also the last file that may have less than 10 plots



