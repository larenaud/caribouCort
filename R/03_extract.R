# suite tidying # extraire les habitats des localisations 


#install.packages("adehabitatHR")
#install.packages("spatialEco")
library(lubridate)
library(dplyr)
library(sp)
library(adehabitatHR)
library(gdata)
library(raster)
library(rgdal)
library(spatialEco) # this one does not load
library(kableExtra)
#import text file contaning all GPS data 2003-2018
#try read.csv() for csv files; read.delim() is for text file
#Depending on system language you may need to add "2" after .csv or .delim (see below) 

setwd("/Users/LimoilouARenaud/Documents/Caribou")


rm(list = ls())
load("Data/spatialDataIntersect.RData")

# localisations filtrées%?? pdop<10??? 


head(charlevoixLocs)

# Import raster files  -----------------------------------------------------
  
chHabitat<-raster("Data/tmp/rasterCharlevoix.tif")
proj4string(chHabitat)


## 
sagHabitat<- raster("Data/tmp/rasterSaguenay.tif")
proj4string(sagHabitat)


# Reassign projections ----------------------------------------------------

# Reproject raster layer
prj<-CRS("+init=EPSG:32187")
prjSag<-CRS("+init=EPSG:26919")

chHabitat<-projectRaster(chHabitat, crs = prj) # initally EPSG:32187 
proj4string(chHabitat)


chHabitat[]=as.integer(chHabitat[])
dataType(chHabitat)<-"INT1U"
dataType(chHabitat)


# sag 

sagHabitat<-projectRaster(sagHabitat, crs = prjSag)
proj4string(sagHabitat) 

sagHabitat[]=as.integer(sagHabitat[])
dataType(sagHabitat)<-"INT1U"



# clean up useless 
rm(pirLocs, coeLocs_out,pirLocs_out, coeLocs,poLocs, poLocs_out, coords.char, coords.sag, bowlocs)
rm(bowLocs, charlevoixLocs_out, sagLocs_out)
rm(chRoads, chArea,chCabins)


# Convert dataset into SpatialPointsDataFrame by indicating which columns contain the Lon/Lat coordinates
coordinates(charlevoixLocs)<-as.data.frame(charlevoixLocs[,4:5])
sagLocs<- na.omit(sagLocs)
coordinates(sagLocs)<-as.data.frame(sagLocs[,3:4])


# WARNING: the spatial object is not projected yet!
str(charlevoixLocs)

crs(sagLocs)
crs(charlevoixLocs)


#Define the Coordinate Reference System (CRS)

# This is really important! Make sure you get this right
# otherwise you points or spatial objects won't appear where they are supposed to...

# You can search for the EPSG: code in google
# depending on the scale of analyses you will have to change this code

# Define the CRS for the spatial object
# To change the CRS you only need to change the numbers in "+init=epsg:3021"
proj4string(charlevoixLocs)<-prj
proj4string(sagLocs)<-prjSag

# OR

charlevoixLocs<-spTransform(charlevoixLocs,CRS("+init=epsg:32187"))
sagLocs<-spTransform(sagLocs,CRS("+init=epsg:26919"))

# verification 
quartz()
plot(sagHabitat)
points(sagLocs, cex = 1)
quartz()
plot(chHabitat)
points(charlevoixLocs, size = 1) # yes ok ! 

# end verification 

#check crs
summary(charlevoixLocs)
proj4string(charlevoixLocs)
proj4string(sagLocs)

head(charlevoixLocs)


# Extract from raster : extract Gps points directly from habitats ------------------
locs<-charlevoixLocs[, 5:6]
buffer_extract_habitatNobuf<-raster::extract(chHabitat, locs, fun = sum, df=TRUE)
head(buffer_extract_habitatNobuf)
#r help - If y represents polygons, the extract method returns the values of the cells of a Raster* object that are covered by a polygon. 
# If y represents points, extract returns the values of a Raster* object for the cells #
# in which a set of points fall.

# tidy up 

# charlevoix set better names for columns
buffer_extract_habitatNobuf<-as.data.frame(buffer_extract_habitatNobuf)
names(buffer_extract_habitatNobuf)<-c("ID","Type")

#buffer_extract_habitatNobuf = buffer_extract_habitatNobuf %>% 
#  filter(Type >0)

# reorganise the data in appropriate format 
chExtractNoBuf<-reshape2::dcast(buffer_extract_habitatNobuf, ID~Type, fun.aggregate=length, value.var="Type")
#habitate<-table(buffer_extract_habitat$ID,buffer_extract_habitat$Type)
View(chExtractNoBuf)


# charlevoix # voir ceci dans la table d'attributs dans qgis
c("other","clearcut05",
  "clearcut620",
  "ds",
  'human',
  "mfmature",
  "natPert05",
  "natPert620",
  "oldConifer",
  "openNoReg",
  "partCut",
  "powerLine",
  "regeneration",
  "water",
  "wetlands",
  "ygConifer")

names(chExtractNoBuf)<-c("ID","other",
                  "clearcut05",
                  "clearcut620",
                  "ds",
                  'human',
                  "mfmature",
                  "natPert05",
                  "natPert620",
                  "oldConifer",
                  "openNoReg",
                  "partCut",
                  "powerLine",
                  "regeneration",
                  "water",
                  "wetlands",
                  "ygConifer")

chData<-cbind(charlevoixLocs[,1:9], chExtractNoBuf[,2:17])

head(chData)

# # utiliser la row sum 
# charlevoixData$sumPix = rowSums(charlevoixData[, 4:18])

# summarize % locs by id by habitat 
chData<-as.data.frame(chData)
tmp<-chData %>% group_by(idyear) %>% 
  summarise_at(vars(other:ygConifer),.fun=list(
            prop = ~sum(.)/length(.)
            )) 
tmp$sumPix = round(rowSums(tmp[, 2:17]), 3) # near 100 % for each ID year 


# Charlevoix roads --------------------------------------------------------
library(lubridate)
library(dplyr)
library(sp)
library(adehabitatHR)
library(gdata)
library(raster)
library(rgdal)
library(rgeos)
library(sf)

### reassign projections cause something is weird in initial objects
char.mcp<-spTransform(char.mcp,CRS("+init=epsg:32187"))
chCabins<-spTransform(chCabins,CRS("+init=epsg:32187"))
chRoads<-spTransform(chRoads,CRS("+init=epsg:32187"))
mcps<-spTransform(char.mcp,CRS(proj4string(chCabins)))

chRoads$CLASSE_ROU

roads12<-chRoads[chRoads$CLASSE_ROU==1|chRoads$CLASSE_ROU==2,]
roads34<-chRoads[chRoads$CLASSE_ROU==3|chRoads$CLASSE_ROU==4,]

### transform to sf
roads12<-st_as_sf(roads12)
roads12buffer<-st_buffer(roads12,dist=500)
roads12buffer<-st_union(roads12buffer) # dissolve all buffers
hr<-st_as_sf(mcps)
loc<-st_as_sf(charlevoixLocs) #will do per loc, not HR


plot(roads12buffer, reset = FALSE, col = "grey")
plot(loc, add = TRUE)


### transform to sf
roads34<-st_as_sf(roads34)
roads34buffer<-st_buffer(roads34,dist=500)
roads34buffer<-st_union(roads34buffer) # dissolve all buffers


quartz()
plot(hr)


### loop over all locs (not hr) to keep track of empty intersections
st_crs(loc)
st_crs(roads12buffer)

nrow(loc)
head(roads12buffer)

res <- st_intersects(loc, roads12buffer, sparse=FALSE)
length (unlist (res)) / nrow (loc) # fraction of intersecting points
dim(res)
dim(loc)
### add info to hr
loc$rd12<-unlist(res)
loc %>% group_by(idyear) %>% summarise(rd= mean(rd12))


### if wanted, return to a sp object
dv<-as(hr,"Spatial")

                   