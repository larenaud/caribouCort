
library(lubridate)
library(dplyr)
library(sp)
library(adehabitatHR)
library(gdata)
library(raster)
library(rgdal)
library(rgeos)
library(sf)

load("~/UdeS/Consultation/LARenaud/20200602spatialData.RData")

proj4string(char.mcp)
proj4string(chCabins)
proj4string(chRoads)

### reassign projections cause something is weird in initial objects
char.mcp<-spTransform(char.mcp,CRS("+init=epsg:32187"))
chCabins<-spTransform(chCabins,CRS("+init=epsg:26919"))
chRoads<-spTransform(chRoads,CRS("+init=epsg:26919"))
mcps<-spTransform(char.mcp,CRS(proj4string(chCabins)))

#windows() # run this (or x11() cause the RStudio plot for spatial object sucks!!!
quartz()
plot(chRoads)
plot(chCabins,pch=16,col="red",cex=1,add=TRUE)
plot(mcps,border="forestgreen",add=TRUE)



# Charlevoix roads --------------------------------------------------------


### transform to sf
roads<-st_as_sf(chRoads)
roadsbuffer<-st_buffer(roads,dist=500)
roadsbuffer<-st_union(roadsbuffer) # dissolve all buffers
hr<-st_as_sf(mcps)

quartz()
plot(hr)
### loop over all homeranges to keep track of empty intersections
# comment # plot to increase speed
l1<-lapply(1:nrow(hr),function(i){
	x<-hr[i,]
	g<-st_intersection(roadsbuffer,x)
	#plot(st_geometry(x),lwd=4,main=i) 
	#plot(st_geometry(roadsbuffer),add=TRUE,col="blue")
	if(length(g)>0){ # if intersection, compute proportion
	#	plot(st_geometry(g),add=TRUE,col="red")
		res<-st_area(g)/st_area(x)
	}else{ # else overlap is 0
		res<-0
	}
	#plot(st_geometry(roads),add=TRUE)
	res
})

### add info to hr
hr$roadsp<-unlist(l1)

### if wanted, return to a sp object
dv<-as(hr,"Spatial")


# Charlevoix cabins -------------------------------------------------------

### transform to sf
cabins<-st_as_sf(chCabins)
cabinsbuffer<-st_buffer(cabins,dist=500)
cabinsbuffer<-st_union(cabinsbuffer) # dissolve all buffers
hr<-st_as_sf(mcps)

quartz()
plot(hr)

plot(cabinsbuffer,pch=16,col="red",cex=1)
plot(mcps,border="forestgreen",add=TRUE)


### loop over all homeranges to keep track of empty intersections
# comment # plot to increase speed
l2<-lapply(1:nrow(hr),function(i){
  x<-hr[i,]
  g<-st_intersection(cabinsbuffer,x)
  plot(st_geometry(x),lwd=4,main=i) 
  plot(st_geometry(cabinsbuffer),add=TRUE,col="blue")
  if(length(g)>0){ # if intersection, compute proportion
    plot(st_geometry(g),add=TRUE,col="red")
    res<-st_area(g)/st_area(x)
  }else{ # else overlap is 0
    res<-0
  }
  plot(st_geometry(cabins),add=TRUE)
  res
})

### add info to hr
hr$cabinsp<-unlist(l2)

### if wanted, return to a sp object
#dv<-as(hr,"Spatial")






# Saguenay Roads ---------------------------------------------------------------


proj4string(sag.mcp)
proj4string(sagCabins)
proj4string(sagRoads)

sag.mcp<-spTransform(sag.mcp,CRS("+init=epsg:26919")) # here changed compared to charlevoix
sagCabins<-spTransform(sagCabins,CRS("+init=epsg:26919"))
sagRoads<-spTransform(sagRoads,CRS("+init=epsg:26919"))
sgmcps<-spTransform(sag.mcp,CRS(proj4string(sagCabins)))


quartz()
plot(sagRoads)
plot(sagCabins,pch=16,col="red",cex=1,add=TRUE)
plot(sgmcps,border="forestgreen",add=TRUE)



### transform to sf
sgroads<-st_as_sf(sagRoads)
sgroadsbuffer<-st_buffer(sgroads,dist=500)
sgroadsbuffer<-st_union(sgroadsbuffer) # dissolve all buffers
sghr<-st_as_sf(sgmcps)

quartz()
plot(sghr)
### loop over all homeranges to keep track of empty intersections
# comment # plot to increase speed
l3<-lapply(1:nrow(sghr),function(i){
  x<-sghr[i,]
  g<-st_intersection(sgroadsbuffer,x)
 # plot(st_geometry(x),lwd=4,main=i) 
  #plot(st_geometry(sgroadsbuffer),add=TRUE,col="blue")
  if(length(g)>0){ # if intersection, compute proportion
  #  plot(st_geometry(g),add=TRUE,col="red")
    res<-st_area(g)/st_area(x)
  }else{ # else overlap is 0
    res<-0
  }
 # plot(st_geometry(sgroads),add=TRUE)
  res
})

### add info to hr
sghr$roadsp<-unlist(l3)

### if wanted, return to a sp object
#sgdv<-as(sghr,"Spatial")


# Saguenay cabins -------------------------------------------------------

### transform to sf
sgcabins<-st_as_sf(sagCabins)
sgcabinsbuffer<-st_buffer(sgcabins,dist=500)
sgcabinsbuffer<-st_union(sgcabinsbuffer) # dissolve all buffers

quartz()
plot(sgcabinsbuffer,pch=16,col="red",cex=1)
plot(sgmcps,border="forestgreen",add=TRUE)


### loop over all homeranges to keep track of empty intersections
# comment # plot to increase speed
l4<-lapply(1:nrow(sghr),function(i){
  x<-sghr[i,]
  g<-st_intersection(sgcabinsbuffer,x)
  #plot(st_geometry(x),lwd=4,main=i) 
 # plot(st_geometry(sgcabinsbuffer),add=TRUE,col="blue")
  if(length(g)>0){ # if intersection, compute proportion
  #  plot(st_geometry(g),add=TRUE,col="red")
    res<-st_area(g)/st_area(x)
  }else{ # else overlap is 0
    res<-0
  }
#  plot(st_geometry(sgcabins),add=TRUE)
  res
})

### add info to hr
sghr$cabinsp<-unlist(l4)

### if wanted, return to a sp object
#sgdv<-as(sghr,"Spatial")

charlevoixData$cabinsp <- hr$cabinsp
charlevoixData$roadsp <- hr$roadsp
sagData$roadsp <- sghr$roadsp
sagData$cabinsp <- sghr$cabinsp

t(t(colSums(sagData[, -c(1:3)])))
t(t(colSums(charlevoixData[, -c(1:3)])))
save(list = ls(), file ="Data/intersection.RData")
