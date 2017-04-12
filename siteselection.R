library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(raster)
library(RQGIS)
library(plyr)

setwd("C:/DAFNE/Sweden GIS data")

cc_total <- readOGR("sksUtfordAvverk_20170220T054858Z", "sksUtfordAvverk") #load clear-cut(cc) shapefile
cc_total <- cc_total[!is.na(cc_total@data$Avvdatum) ,] #omit NA's in Avvdatum
cc_total@data$Avvdatum <- as.Date(cc_total@data$Avvdatum)  #transform Avvdatum into date
cc_after2006 <- cc_total[cc_total@data$Avvdatum > "2007-01-01", ] #omit all ccs before 2007
cc_after2006 <- cc_after2006[!is.na(cc_after2006@data$Lannr) ,] #omit NA's in Lannr
rm(cc_total)

bbs_lines <- readOGR("Transects, points and buffers", "bbs_lines")  #load transects shapefile
bbs_linesSWE99TM <- spTransform(bbs_lines, CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")) # reproject

#function to get intersecting ccs per län, since running it for whole of sweden at once need to much working memory?!
SubLan<-function(x){
  a <- cc_after2006[cc_after2006@data$Lannr == x,]
  b <- a[bbs_linesSWE99TM,]
}

cc_sublan <- lapply(levels(cc_after2006@data$Lannr),SubLan) 
cc_onroute <- do.call(bind, cc_sublan)
rm(cc_after2006, cc_sublan)
cc_onroute@data[,3:12]<-NULL
cc_onroute@data[,4:7]<-NULL

bbs_linesWGS84 <- spTransform(bbs_lines, CRS("+proj=longlat +datum=WGS84")) # reproject
bbs_linesWGS84$KARTA <- as.character(bbs_linesWGS84$KARTA)

coord <- read.csv("public_standardrutter_koordinater.csv", sep=";", dec=",")
coord$karta <- as.character(coord$karta)

names <- read.csv("public_standardrutter_oversikt.csv", sep=";", dec=",")
names$karta <- as.character(names$karta)
names$namn <- as.character(names$namn)
coord <- merge(coord, names[,2:3], by="karta")  #add route names to coordinates df


bla <- intersect(bbs_linesWGS84,cc_onrouteWGS84) #intersect routes with ccs, only to get the numbers of the routes that have ccs
kartor <- unique(bla@data$KARTA) #get route numbers
route_ccs <- data.frame(cbind(bla@data$KARTA,bla@data$OBJECTID)) #create df with cc IDs for each route
colnames(route_ccs) <- c("karta","OBJECTID")
route_ccs <- route_ccs[which(!duplicated(route_ccs$OBJECTID)), ] # omit ccs that intersect more than once
route_ccs <- ddply(route_ccs, .(karta), mutate, id = seq_along(OBJECTID))
route_ccs$OBJECTID <- as.numeric(as.character(route_ccs$OBJECTID))

cc_onroute@data$OBJECTID <- as.numeric(cc_onroute@data$OBJECTID)

cc_onroute@data <- data.frame(cc_onroute@data, route_ccs[match(cc_onroute@data[,1], route_ccs[,2]),])
cc_onrouteWGS84 <- spTransform(cc_onroute, CRS("+proj=longlat +datum=WGS84")) # reproject

coord_sel <- coord[coord$karta %in% kartor,]

cc_onrouteWGS84@data$OBJECTID <- as.character(cc_onrouteWGS84@data$OBJECTID)
cc_Centroids <- gCentroid(cc_onrouteWGS84, byid=TRUE, id=cc_onrouteWGS84@data$OBJECTID)
cc_onrouteWGS84@data$OBJECTID <- as.numeric(cc_onrouteWGS84@data$OBJECTID)
df_centroids <- data.frame(cc_Centroids@coords, as.numeric(rownames(cc_Centroids@coords)))
colnames(df_centroids)<- c("cx", "cy", "OBJECTID")
cc_onrouteWGS84@data <- data.frame(cc_onrouteWGS84@data, df_centroids[match(cc_onrouteWGS84@data[,1], df_centroids[,3]),])

coordss <- coord_sel[1:5,]

b<-list()
for(i in 1:nrow(coordss)){
  lon <- coordss$mitt_wgs84_lon[i]
  lat <- coordss$mitt_wgs84_lat[i]
b[[i]] <-  qmap(c(lon = lon, lat = lat), zoom = 14, maptype = 'hybrid')+
  geom_polygon(aes(x = long, y = lat, group=group), data = cc_onrouteWGS84, fill="blue", alpha=.4, color="blue")+
  geom_line(aes(x = long, y = lat, group=group),data = bbs_linesWGS84, color="red")+
  geom_text(label=coordss$karta[i],vjust=1.3, hjust=-0.1, color="white", size=12)+
  geom_text(label=coordss$namn[i],vjust=1.3, hjust=1.2, color="white", size=12)+
  geom_text(data = map_df, aes(label = id,x = cx, y = cy), color = "white", size=6)
}


map_df <- cc_onrouteWGS84@data[,c(1,3,4,5,7,9,10)]


