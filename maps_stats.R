### mapping merged files
library(geosphere)


setwd("C:/Monitoring_App")
lines <- read.table('monitoring.txt', header=TRUE, sep='\t')

df <- data.frame(line = unique(lines$line), dist = 0, cat = 0)



for (i in 1:length(df$line)){
  idd <- df$line[i]
  dat <- subset(lines, line==idd)
  df$cat[i] <- dat[1,7]
  d <- c()
  for (j in 1:(ncol(dat)-1)){
    lon1 = dat[j,1] ; lon2 = dat[j+1,1]
    lat1 = dat[j,2] ; lat2 = dat[j+1,2]
    d[j] = distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
  }
  df$dist[i] <- sum(d, na.rm=TRUE)
}

# summary per category
cats_sum <- data.frame(cat<-c(1,2,3), dist=0)
for (i in 1:3){
  dat <- subset(df, cat==cats_sum[i,1])
  cats_sum[i,2] <- sum(dat$dist)
}

# mapping
LAT1 =  min(lines$lat)*0.9999 ; LAT2 = max(lines$lat)*1.0001
LON1 = min(lines$lon)*0.9999 ; LON2 = max(lines$lon)*1.0001

map <- openmap(c(LAT2,LON1), c(LAT1,LON2), zoom = 14,
               type = c("osm", "stamen-toner", "stamen-terrain","stamen-watercolor", "esri","esri-topo")[1],
               mergeTiles = TRUE)
plot(map)

map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

library(raster)
r <- raster(map.latlon)

map <- as.data.frame(r, xy=TRUE)
map$RGB <- rgb(map$layer.1, map$layer.2, map$layer.3, maxColorValue=255)



mussel <- ggplot()+
  geom_raster(data=map, aes(x=x, y=y, fill=RGB))+
  geom_path(data=lines, aes(x=lon, y=lat, group=line, colour=as.factor(log)), size=1, alpha=0.8)+
  #geom_line(data=mp, aes(x=x, y=y), colour='white', size=2)+
  scale_fill_identity(guide=FALSE)+
  scale_colour_manual(name = NULL, breaks=c(1,2,3), values=c('darkgreen','yellow','red'), guide=FALSE)+
  scale_x_continuous(name='longitude',expand=c(0,0)) + 
  scale_y_continuous(name='latitude',expand=c(0,0))

mussel

pdf('mussels_Flodevigen_2020.pdf', width=16, height=16)
mussel
dev.off()





