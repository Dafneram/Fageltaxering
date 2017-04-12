setwd('C:/Users/Dafne/Desktop/Svensk Fågeltaxering')
punktrutter<-read.csv('PUNKTRUTTER.csv', sep=',', dec=',')
kartor_master<-read.csv('KARTOR_MASTER.csv', sep=',', dec=',')
total<-merge(punktrutter,kartor_master, by=c('persnr','rnr'), all='TRUE')
write.csv(total, "total.csv", na="")

personer<-read.csv('PERSONER.csv', sep=',', dec=',')
maps<-read.csv('missing.maps.csv', sep=',', dec=',')
maps.pers<-merge(maps, personer, by='persnr')
write.csv(maps.pers, "maps.pers.csv", na="")