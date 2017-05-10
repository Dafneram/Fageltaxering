library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(raster)
library(RQGIS)
library(plyr)

setwd("C:/DAFNE/Sweden GIS data")

#load clear-cut shapefile and select after2006
cc_total <- readOGR("sksUtfordAvverk_20170220T054858Z", "sksUtfordAvverk") 
cc_total <- cc_total[!is.na(cc_total@data$Avvdatum) ,] 
cc_total@data$Avvdatum <- as.Date(cc_total@data$Avvdatum)  
cc_after2006 <- cc_total[cc_total@data$Avvdatum > "2007-01-01", ] 
cc_after2006 <- cc_after2006[!is.na(cc_after2006@data$Lannr) ,] 
cc_after2006WGS84 <- spTransform(cc_after2006, CRS("+proj=longlat +datum=WGS84")) # reproject
rm(cc_total)

#load standardroute lines shapefile and reproject
bbs_lines <- readOGR("Transects, points and buffers", "stdruttLinjer")  #load transects shapefile
proj4string(bbs_lines) <- NA_character_ # remove CRS information 
proj4string(bbs_lines) <- CRS("+init=epsg:3021") # assign a new CRS
bbs_linesWGS84 <- spTransform(bbs_lines, CRS("+proj=longlat +datum=WGS84")) # reproject
bbs_linesWGS84$KARTA <- as.character(bbs_linesWGS84$KARTA)

#load standardroute points shapefile and reproject
bbs_points <- readOGR("Transects, points and buffers", "stdruttPunkter")  #load points shapefile
proj4string(bbs_points) <- NA_character_ # remove CRS information 
proj4string(bbs_points) <- CRS("+init=epsg:3021") # assign a new CRS
bbs_pointsWGS84 <- spTransform(bbs_points, CRS("+proj=longlat +datum=WGS84")) # reproject
bbs_points_df <- data.frame(cbind(bbs_pointsWGS84@data, bbs_pointsWGS84@coords))
bbs_points_df$KARTA <- as.character(bbs_points_df$KARTA)

#get intersecting ccs per län, since running it for whole of sweden at once need to much working memory
cc_sublan <- lapply(levels(cc_after2006WGS84@data$Lannr),function(x){
  a <- cc_after2006WGS84[cc_after2006WGS84@data$Lannr == x,]
  b <- a[bbs_linesWGS84,]
}) 
cc_onrouteWGS84 <- do.call(bind, cc_sublan)
rm(cc_after2006, cc_after2006WGS84, cc_sublan)
cc_onrouteWGS84@data[,c(3:12,14:17)]<-NULL
BU <- cc_onrouteWGS84
cc_onrouteWGS84 <- BU

#get list of routes with ccs
ri <- intersect(bbs_linesWGS84,cc_onrouteWGS84) #intersect routes with ccs, only to get the numbers of the routes that have ccs
#ri3 <- intersect(cc_onrouteWGS84, bbs_linesWGS84)

bastard <- subset(cc_onrouteWGS84, cc_onrouteWGS84@data$OBJECTID == "68215")

route_ccs <- data.frame(cbind(ri@data$KARTA,ri@data$OBJECTID)) #create df with cc IDs for each route
colnames(route_ccs) <- c("karta","OBJECTID")

doubles <- route_ccs$OBJECTID[which(duplicated(route_ccs$OBJECTID))]

cc_onrouteWGS84@data$OBJECTID <- as.character(cc_onrouteWGS84@data$OBJECTID)
cc_onrouteWGS84@data$OBJECTID <- as.numeric(cc_onrouteWGS84@data$OBJECTID)
route_ccs$OBJECTID <- as.character(route_ccs$OBJECTID)
route_ccs$OBJECTID <- as.numeric(route_ccs$OBJECTID)

cc_onrouteWGS84@data <- data.frame(cc_onrouteWGS84@data, route_ccs[match(cc_onrouteWGS84@data[,1], route_ccs[,2]),])
cc_onrouteWGS84@data$karta <- as.character(cc_onrouteWGS84@data$karta)

#Take out doubles
cc_onrouteWGS84 <- cc_onrouteWGS84[!(cc_onrouteWGS84@data$OBJECTID %in% doubles),]
cc_onrouteWGS84 <- cc_onrouteWGS84[!(cc_onrouteWGS84@data$OBJECTID == "68215"),]

#list ccs that overlap with other ccs
ccs_count <- as.data.frame(table(route_ccs$karta))
ccs_count <- subset(ccs_count, Freq >1 )
kartor_mult <- as.list(as.character(ccs_count$Var1))

overlapccs <- matrix(NA, nrow=500, ncol=500)
for(i in 1:length(kartor_mult)){

    oneroute <- subset(cc_onrouteWGS84, karta == kartor_mult[i])
    combos <- combn(as.list(oneroute$OBJECTID),2)
    
    for(k in seq_along(combos[1,])){
     if(gIntersects(subset(cc_onrouteWGS84, OBJECTID == combos[1,k]), subset(cc_onrouteWGS84, OBJECTID == combos[2,k]), byid = FALSE)){
       overlapccs[[k,i]]<-(paste(combos[1,k]))
       overlapccs[[k,i]]<-(paste(combos[2,k]))
    }
  } 
}
overlapccs <- overlapccs[!is.na(overlapccs)] 

#exclude ccs that overlap with others
cc_onrouteWGS84 <- subset(cc_onrouteWGS84,!(cc_onrouteWGS84@data$OBJECTID %in% overlapccs))

#exclude polygons consisting of subpolygons
nrPoly<- sapply(cc_onrouteWGS84@polygons,function(polys) length(polys@Polygons))
cc_onrouteWGS84@data <- data.frame(cbind(cc_onrouteWGS84@data, nrPoly))
cc_onrouteWGS84 <- cc_onrouteWGS84[cc_onrouteWGS84@data$nrPoly < 2,]

#number ccs per route and add to data
ri2 <- intersect(bbs_linesWGS84,cc_onrouteWGS84) #intersect routes with ccs, only to get the numbers of the routes that have ccs
route_ccs2 <- data.frame(cbind(ri2@data$KARTA,ri2@data$OBJECTID)) #create df with cc IDs for each route
colnames(route_ccs2) <- c("karta","OBJECTID")

route_ccs2 <- ddply(route_ccs2, .(karta), mutate, id = seq_along(OBJECTID)) #number ccs along each route
cc_onrouteWGS84@data <- data.frame(cc_onrouteWGS84@data, route_ccs2[match(cc_onrouteWGS84@data[,1], route_ccs2[,2]),]) #add sequence numbers to cc df

#check which existing points are on ccs
cc_points <- intersect(bbs_pointsWGS84,cc_onrouteWGS84) 
cc_points_df <- cc_points@data
#cc_points_df <- cc_points_df[which(!duplicated(cc_points_df$OBJECTID)), ] # omit doubles; ccs that intersect more than once
#cc_points_df <- data.frame(cbind(cc_points@data, cc_points@coords))

#exclude all excessive ccs (more than 2 per route)
cc_onrouteWGS84 <- subset(cc_onrouteWGS84, cc_onrouteWGS84@data$id < 3)


#get list of routes with names
coord <- read.csv("public_standardrutter_koordinater.csv", sep=";", dec=",")
coord$karta <- as.character(coord$karta)
names <- read.csv("public_standardrutter_oversikt.csv", sep=";", dec=",")
names$karta <- as.character(names$karta)
names$namn <- as.character(names$namn)
coord <- merge(coord, names[,2:3], by="karta")  #add route names to coordinates df

kartor <- sort(unique(cc_onrouteWGS84@data$karta)) #get route numbers
coord_sel <- coord[coord$karta %in% kartor,] #select routes with ccs on them

#get cc centroids coords and add to data
cc_onrouteWGS84@data$OBJECTID <- as.character(cc_onrouteWGS84@data$OBJECTID)
cc_Centroids <- gCentroid(cc_onrouteWGS84, byid=TRUE, id=cc_onrouteWGS84@data$OBJECTID) #get centroid coordinates of ccs
cc_onrouteWGS84@data$OBJECTID <- as.numeric(cc_onrouteWGS84@data$OBJECTID)
df_centroids <- data.frame(cc_Centroids@coords, as.numeric(rownames(cc_Centroids@coords)))
colnames(df_centroids)<- c("cx", "cy", "OBJECTID")
cc_onrouteWGS84@data <- data.frame(cc_onrouteWGS84@data, df_centroids[match(cc_onrouteWGS84@data[,1], df_centroids[,3]),]) #add centroid coordinates to cc df


#loop over all routes
coordss <- coord_sel [1:2,]# to test loop
coordss <- coord_sel [1:220,]# with zoom 14
coordss <- coord_sel [221:454,]# with zoom 13

library(rmarkdown)
library(knitr)

for(i in 1:nrow(coordss)){
  lon <- coordss$mitt_wgs84_lon[i]
  lat <- coordss$mitt_wgs84_lat[i]
  route <- paste(coordss$karta[i],label=coordss$namn[i])
routemap <- qmap(c(lon = lon, lat = lat), zoom = 14, maptype = 'hybrid')+
  geom_polygon(aes(x = long, y = lat, group=group), data = cc_onrouteWGS84, fill="blue", alpha=.4, color="blue")+
  geom_line(aes(x = long, y = lat, group=group),data = bbs_linesWGS84, color="red", size=0.8)+
  geom_point(aes(x = coords.x1, y = coords.x2), data = subset(bbs_points_df, KARTA==coordss$karta[i]), shape=1, color = "red", size=3, stroke=1)+
#  geom_point(aes(x = coords.x1, y = coords.x2), data = subset(cc_points_df, KARTA==coordss$karta[i]), shape=1, color = "blue", size=3, stroke=1)+
  geom_text(label=route,vjust=1.3, hjust=-0.1, color="white", size=8)+
  geom_text(data = subset(cc_onrouteWGS84@data, karta==coordss$karta[i]) , aes(label = id,x = cx, y = cy), color = "white", size=6)
  render("siteselection.rmd", output_file = paste0("map.",coordss$karta[i],".pdf"))
}




