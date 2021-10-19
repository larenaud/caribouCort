# Script for converting text to numerical values in QGIS  
# # need this to be able to rename column in R. 
# CASE 
# WHEN  "TYPE_COUV" ='F' THEN 1
# WHEN  "TYPE_COUV" ='M' THEN 2
# WHEN  "TYPE_COUV" ='R' THEN 3   
# END
# 
# in QGIS : converted vector files to raster - make it easier to work in R 
# toolbox - vector - vtorast - cell size  = 25 m - type attribute = va (NOT attr)


# cd ~
#   touch .Renviron
# open .Renviron
# Step 3: Save the following as the first line of .Renviron:
#   
#   R_MAX_VSIZE=100Gb 
# 



# script updated jan 2021 



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


# Get and tidy gps points  ------------------------------------------------


charlevoixLocs<-read.csv2("Data/locsCharlevoix.csv")
bowLocs<-read.csv2("Data/locsBowater.csv")
pirLocs<-read.csv2("Data/locsPiraube.csv")
coeLocs<-read.csv2("Data/locsCoeur.csv", sep = ",")
poLocs<-read.csv2("Data/locsPortneuf.csv", sep = ",")
#romLocs<-read.csv2("Data/locsCoeur.csv", sep = ",")

# select date 1JN to 31OCT
charlevoixLocs <- charlevoixLocs %>%
  filter(MOIS>=6 & MOIS <=10)


##convert data column to good format


charlevoixLocs$LATM<-as.numeric(as.character(charlevoixLocs$LATM))
charlevoixLocs$LONGM<-as.numeric(as.character(charlevoixLocs$LONGM))

pirLocs$LocX<-as.numeric(as.character(pirLocs$LocX))
pirLocs$LocY<-as.numeric(as.character(pirLocs$LocY))

coeLocs$POINT_X<-as.numeric(as.character(coeLocs$POINT_X))
coeLocs$POINT_Y<-as.numeric(as.character(coeLocs$POINT_Y))

poLocs$POINT_X<-as.numeric(as.character(poLocs$POINT_X))
poLocs$POINT_Y<-as.numeric(as.character(poLocs$POINT_Y))


charlevoixLocs$DATE_AJ<- as.POSIXct(charlevoixLocs$DATE_AJ, tz="CET", format="%Y-%m-%d")
bowLocs$Date_<- as.POSIXct(bowLocs$Date_, tz="CET", format="%Y-%m-%d")
pirLocs$Date<- as.POSIXct(pirLocs$Date, tz="CET", format="%Y-%m-%d")
coeLocs$Date_<- as.POSIXct(coeLocs$Date_, tz="CET", format="%Y-%m-%d")
poLocs$Date_<- as.POSIXct(poLocs$Date_, tz="CET", format="%Y-%m-%d")

#romLocs$Date_<- as.POSIXct(coeLocs$Date_, tz="CET", format="%Y-%m-%d")

#make sure dates are in good format
is.POSIXt(charlevoixLocs$DATE_AJ)
is.POSIXct(poLocs$DATE_) # dit false... 


# keep only necessary stuff 
names(charlevoixLocs)
charlevoixLocs<-charlevoixLocs[, c( "IDENT","SEXE", "DATE_AJ","LONGM","LATM","YEAR", "AGE","JJULIEN")]

names(bowLocs)
#bowLocs<-read.csv2("Data/locsBowater.csv")
names(pirLocs)
pirLocs <- pirLocs[, c("ID", "Date", "LocX",   "LocY", "JulDay" )]
names(coeLocs)
coeLocs<-coeLocs[,c("Ind", "Date_", "POINT_X","POINT_Y", "Jul_day" )]

names(poLocs)
poLocs<-poLocs[,c("ID", "Date_", "POINT_X","POINT_Y", "Jul_day" )]
#romLocs<-read.csv2("Data/locsCoeur.csv", sep = ",")

names(charlevoixLocs) <- c("ID","sex", "date", "locX","locY", "year", "age", "julDay" )
names(pirLocs) <-c("ID","date", "locX", "locY", "julDay" )
names(coeLocs) <-c("ID", "date", "locX","locY", "julDay" ) # jul day not good
names(poLocs) <-c("ID",  "date"  , "locX" ,"locY", "julDay")


# bind all locs # emply column sex
pirLocs <-pirLocs[, c("ID","date", "locX", "locY", "julDay" )]
coeLocs <-coeLocs[c("ID","date", "locX","locY", "julDay" )] # jul day not good
poLocs <-poLocs[,c("ID", "date"  , "locX" ,"locY", "julDay")]
names(pirLocs) <-c("ID","date", "locX", "locY", "julDay" )

sagLocs <- rbind(pirLocs, coeLocs, pirLocs)
summary(sagLocs)

# add ID year column

sagLocs$year <-substring(sagLocs$date,1,4)
charlevoixLocs$year <-substring(charlevoixLocs$date,1,4)

sagLocs$ID <- as.character(sagLocs$ID)
sagLocs$ID <- stringr :: str_remove(sagLocs$ID, "_")

sagLocs$idyear = paste(sagLocs$ID,sagLocs$year, sep = "-")
charlevoixLocs$idyear = paste(charlevoixLocs$ID,charlevoixLocs$year, sep = "-")
tail(sagLocs$idyear)

rm(poLocs, coeLocs, pirLocs)

# select date 1JN to 31OCT for sag
sagLocs$month <-substring(sagLocs$date,6,7)
sagLocs$month <- as.numeric(sagLocs$month)
sagLocs <- sagLocs %>%
  filter(month>=6 & month <=10)

#save(list = ls(), file = "locClear.RData")


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
plot(sagHabitat)
points(sagLocs)
plot(chHabitat)
points(charlevoixLocs) # yes ok ! 

# end verification 

#check crs
summary(charlevoixLocs)
proj4string(charlevoixLocs)
proj4string(sagLocs)



# Minimum convex polygons  -----------------------------------------------------------------


#Determine the 100% MCP of caribou 
# data is filtered for hair growth 
char.mcp <- mcp(charlevoixLocs[, 9], percent=100, unout = "km2") # select idyear column 
head(char.mcp)
summary(char.mcp)
head(charlevoixLocs)


# SAG 
ta <- table(sagLocs$idyear)<5
which(ta[ta<5] == TRUE)
sagLocs <- sagLocs[!(sagLocs$idyear == 'PO404-2006'),] # this one didn't have 5 relocations 
sag.mcp <- mcp(sagLocs[,7], percent=100, unout = "km2") # selection idyear column 

head(sag.mcp)


# verification 
plot(sagHabitat)
points(sagLocs)
plot(sag.mcp, add= T)


plot(chHabitat)
points(charlevoixLocs) # yes ok ! 
plot(char.mcp, add= T)
# end verification 



# Extract from raster  -------------------------------------------------------------


# charlevoix

# Reported avoidance distances vary between 0 and 5,000 m (Environment Canada 2011), 
# although the most commonly cited distances are <250 m for roads, 100-250 m for seismic lines and 250- 1000 m 
# for well sites (Dyer 1999). 
# Env.Canada 2011 update :
# However, supporting analyses of a range of buffer widths demonstrated that a 500 m buffer on 
#anthropogenic disturbance provided an appropriate, minimum approximation of the zone of influence 
#of these features on caribou demography.

plot(chHabitat)
plot(char.mcp, add = T)
head(char.mcp)
tail(char.mcp)


# extract habitat cover 
buffer_extract_habitat<-raster::extract(chHabitat, char.mcp, df=TRUE)
head(buffer_extract_habitat)
#r help - If y represents polygons, the extract method returns the values of the cells of a Raster* object that are covered by a polygon. 


# tidy up 

# charlevoix set better names for columns
buffer_extract_habitat<-as.data.frame(buffer_extract_habitat)
names(buffer_extract_habitat)<-c("ID","Type")

buffer_extract_habitat = buffer_extract_habitat %>% 
  filter(Type >0)
# reorganise the data in appropriate format 
habitat<-reshape2::dcast(buffer_extract_habitat, ID~Type, fun.aggregate=length, value.var="Type")
#habitate<-table(buffer_extract_habitat$ID,buffer_extract_habitat$Type)
View(habitat)


# saguenay 
# extract habitat cover 
plot(sagHabitat)
points(sagLocs)

sag_extract_habitat<-raster::extract(sagHabitat, sag.mcp, df=TRUE)


# saguenay set better names for columns
sag_extract_habitat<-as.data.frame(sag_extract_habitat)
names(sag_extract_habitat)<-c("ID","Type")
head(sag_extract_habitat)

sag_extract_habitat = sag_extract_habitat %>% 
  filter(Type >0)

# reorganise the data in appropriate format 
sagHabitat<-reshape2::dcast(sag_extract_habitat, ID~Type, fun.aggregate=length, value.var="Type")
#habitate<-table(buffer_extract_habitat$ID,buffer_extract_habitat$Type)
View(sagHabitat)


# Bind and label dataframes  --------------------------------------------------------

# charlevoix # voir ceci dans la table d'attributs dans qgis
c("clearcut05",
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

names(habitat)<-c("ID",
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


names(sagHabitat)<-c("ID",
                     "clearcut05", #1
                  "clearcut620", #2
                  "ds", # 3
                  'human', # 4
                  "mfmature", #5
                  "natPert05", #6
                  "natPert620", #7
                  "oldConifer", # 8
                  "openNoReg", # 9
                  "partCut", # 10
                  "powerLine", #11
                  "regeneration", #12
                  "water", #13
                  "wetlands", #14
                  "ygConifer") #15
# 16 other 





# tidy up mcp 
charlevoixData= as.data.frame(char.mcp)
charlevoixData = cbind(charlevoixData, habitat)

dim(sag.mcp)

# utiliser la row sum 
charlevoixData$sumPix = rowSums(charlevoixData[, 4:18])

# charlevoixData = charlevoixData %>% 
#   group_by(id) %>% 
#   mutate(sumPix = sum(charlevoixData[, 4:18]))

colnames(charlevoixData)         
tmp = charlevoixData %>% mutate(clearcut05 = clearcut05/sumPix,
                          clearcut620 = clearcut620/sumPix,
                          ds = ds/sumPix, 
                          human = human/sumPix, 
                          mfmature = mfmature/sumPix,
                          natPert05 = natPert05/sumPix,
                          natPert620 = natPert620/sumPix,
                          oldConifer= oldConifer/sumPix,
                          openNoReg = openNoReg/sumPix,
                          partCut = partCut/sumPix,
                          powerLine = powerLine/sumPix,
                          regeneration = regeneration/sumPix,
                          water = water/sumPix,
                          wetlands = wetlands/sumPix,
                          ygConifer = ygConifer/sumPix
)

prop = tmp[3,4:18]*100
quartz()
pie(t(prop))

sum(prop)

charlevoixData = tmp


sagData= as.data.frame(sag.mcp)
sagData = cbind(sagData, sagHabitat)
# 
# sagData = sagData %>% 
#   group_by(id) %>% 
#   mutate(sumPix = sum(sagData[, 4:18]))

sagData$sumPix = rowSums(sagData[, 4:18])


colnames(sagData)         
tmp1= sagData %>% mutate(clearcut05 = clearcut05/sumPix,
                          clearcut620 = clearcut620/sumPix,
                          ds = ds/sumPix, 
                          human = human/sumPix, 
                          mfmature = mfmature/sumPix,
                          natPert05 = natPert05/sumPix,
                          natPert620 = natPert620/sumPix,
                          oldConifer= oldConifer/sumPix,
                          openNoReg = openNoReg/sumPix,
                          partCut = partCut/sumPix,
                          powerLine = powerLine/sumPix,
                          regeneration = regeneration/sumPix,
                          water = water/sumPix,
                          wetlands = wetlands/sumPix,
                          ygConifer = ygConifer/sumPix
)

sagData = tmp1

prop = tmp1[3,4:18]*100
quartz()
pie(t(prop))

sum(prop)
summary(prop)


# attention yr + 1 will be year of hair sampling 
dim(charlevoixData)
dim(habitat)



# Intersections roads and cabins  -----------------------------------------

library(lubridate)
library(dplyr)
library(sp)
library(adehabitatHR)
library(gdata)
library(raster)
library(rgdal)
library(rgeos)
library(sf)

# utm  - en m

# fonction gBuffer autour des routes puis l'agglomérer 
# plot 
# gIntersection - entre les mcp et mon gros buffer
# gArea pour calculer les surfaces 


# get roads and cabins

chCabins<-readOGR(dsn="Data/shapefiles/charlevoix/chalets",layer="Chalets_Charlevoix")
chRoads<-readOGR(dsn="Data/shapefiles/charlevoix/routes",layer="Routes_Charlevoix")

sagCabins<-readOGR(dsn="Data/shapefiles/saguenay/chalets",layer="Chalets_PF_clip")
sagRoads<-readOGR(dsn="Data/shapefiles/saguenay/routes",layer="Routes_PF_clip")

proj4string(char.mcp)
proj4string(chCabins)
proj4string(chRoads)

### reassign projections cause something is weird in initial objects
char.mcp<-spTransform(char.mcp,CRS("+init=epsg:32187"))
chCabins<-spTransform(chCabins,CRS("+init=epsg:32187"))
chRoads<-spTransform(chRoads,CRS("+init=epsg:32187"))
mcps<-spTransform(char.mcp,CRS(proj4string(chCabins)))

#windows() # run this (or x11() cause the RStudio plot for spatial object sucks!!!
x11()
quartz()
plot(chRoads)
plot(chCabins,pch=16,col="red",cex=1,add=TRUE)
plot(mcps,border="forestgreen",add=TRUE)



# Charlevoix roads --------------------------------------------------------
chRoads$CLASSE_ROU

roads12<-chRoads[chRoads$CLASSE_ROU==1|chRoads$CLASSE_ROU==2,]
roads34<-chRoads[chRoads$CLASSE_ROU==3|chRoads$CLASSE_ROU==4,]


### transform to sf
roads12<-st_as_sf(roads12)
roads12buffer<-st_buffer(roads12,dist=500)
roads12buffer<-st_union(roads12buffer) # dissolve all buffers
hr<-st_as_sf(mcps)

### transform to sf
roads34<-st_as_sf(roads34)
roads34buffer<-st_buffer(roads34,dist=500)
roads34buffer<-st_union(roads34buffer) # dissolve all buffers


quartz()
plot(hr)


### loop over all homeranges to keep track of empty intersections
# comment # plot to increase speed
roadsList12<-lapply(1:nrow(hr),function(i){
  x<-hr[i,]
  g<-st_intersection(roads12buffer,x)
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

roadsList34<-lapply(1:nrow(hr),function(i){
  x<-hr[i,]
  g<-st_intersection(roads34buffer,x)
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
hr$roadsp12<-unlist(roadsList12)
hr$roadsp34<-unlist(roadsList34)

charlevoixData$roadsp12 <- hr$roadsp12
charlevoixData$roadsp34 <- hr$roadsp34


### if wanted, return to a sp object
dv<-as(hr,"Spatial")


# Charlevoix cabins -------------------------------------------------------

### transform to sf
cabins<-st_as_sf(chCabins)
cabinsbuffer<-st_buffer(cabins,dist=500)
cabinsbuffer<-st_union(cabinsbuffer) # dissolve all buffers
hr<-st_as_sf(mcps)

# quartz()
# plot(hr)
# 
plot(cabinsbuffer,pch=16,col="red",cex=1)
plot(mcps,border="forestgreen",add=TRUE)


### loop over all homeranges to keep track of empty intersections
cabins<-lapply(1:nrow(hr),function(i){
  x<-hr[i,]
  g<-st_intersection(cabinsbuffer,x)
  #plot(st_geometry(x),lwd=4,main=i) 
 # plot(st_geometry(cabinsbuffer),add=TRUE,col="blue")
  if(length(g)>0){ # if intersection, compute proportion
  #  plot(st_geometry(g),add=TRUE,col="red")
    res<-st_area(g)/st_area(x)
  }else{ # else overlap is 0
    res<-0
  }
 # plot(st_geometry(cabins),add=TRUE)
  res
})

### add info to hr
hr$cabinsp<-unlist(cabins)

### if wanted, return to a sp object
dv<-as(hr,"Spatial")


# tidy up 
charlevoixData$cabinsp <- hr$cabinsp

charlHabitat = t(t(colSums(charlevoixData[, -c(1:3)])))

kable(charlHabitat) %>%
  kable_styling(font_size = 10) %>%
  kable_styling("bordered") %>%
  save_kable(file = "TableS1CharlevoixDV.html", self_contained = T)





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

# selection which roads
roadsSg12<-sagRoads[sagRoads$Classe==1|sagRoads$Classe==2,]
roadsSg34<-sagRoads[sagRoads$Classe==3|sagRoads$Classe==4,]


### transform to sf
roadsSg12<-st_as_sf(roadsSg12)
sgroads12buffer<-st_buffer(roadsSg12,dist=500)
sgroads12buffer<-st_union(sgroads12buffer) # dissolve all buffers

roadsSg34<-st_as_sf(roadsSg34)
sgroads34buffer<-st_buffer(roadsSg34,dist=500)
sgroads34buffer<-st_union(sgroads34buffer) # dissolve all buffers

sghr<-st_as_sf(sgmcps)

quartz()
plot(sghr)


### loop over all homeranges to keep track of empty intersections

roadsList12Sg<-lapply(1:nrow(sghr),function(i){
  x<-sghr[i,]
  g<-st_intersection(sgroads12buffer,x)
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


roadsList34Sg<-lapply(1:nrow(sghr),function(i){
  x<-sghr[i,]
  g<-st_intersection(sgroads34buffer,x)
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
sghr$roadsp12<-unlist(roadsList12Sg)
sghr$roadsp34<-unlist(roadsList34Sg)

### if wanted, return to a sp object
#sgdv<-as(sghr,"Spatial")

sagData$roadsp12 <- sghr$roadsp12
sagData$roadsp34 <- sghr$roadsp34


# Saguenay cabins -------------------------------------------------------

### transform to sf
sgcabins<-st_as_sf(sagCabins)
sgcabinsbuffer<-st_buffer(sgcabins,dist=500)
sgcabinsbuffer<-st_union(sgcabinsbuffer) # dissolve all buffers

# quartz()
# plot(sgcabinsbuffer,pch=16,col="red",cex=1)
# plot(sgmcps,border="forestgreen",add=TRUE)


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
sgdv<-as(sghr,"Spatial")

# add roads and cabin in the data frame 
sagData$cabinsp <- sghr$cabinsp

sagHabitat= t(t(colSums(sagData[, -c(1:3)])))

kable(sagHabitat) %>%
  kable_styling(font_size = 10) %>%
kable_styling("bordered") %>%
save_kable(file = "TableS2SagHabitatsDV.html", self_contained = T)

# will have to add roads and cabins 

#save(sagData,charlevoixData, charlevoixLocs, sagLocs, file ="Data/spatialDataIntersect.RData")

















# old ---------------------------------------------------------------------
# 
# chCabin<-raster("Data/tmp/rasterCabCharlevoix.tif")
# proj4string(chCabin)
# plot(chCabin)
# 
# chRoad<-raster("Data/tmp/rasterRdCharlevoix.tif")
# proj4string(chRoad)
# plot(chRoad)


# 
# # extract roads lengths
# chRoad<-raster("Data/tmp/testRoadChGdal.tif") # fait avec CLA_ROU DNAS QGIS AVEC GDAL
# proj4string(chRoad)
# plot(chRoad)
# 
# buffer_extract_road<-raster::extract(chRoad, char.mcp, buffer = 500,  df=TRUE) 
# plot(chRoad)
# 
# plot(char.mcp, add = T)
# 
# # extract cabin density
# chCabin<-raster("Data/tmp/testCabinChGdal.tif") #
# proj4string(chCabin)
# plot(chCabin)
# 
# buffer_extract_cabin<-raster::extract(chCabin, char.mcp, buffer = 500, df=TRUE) # ceci fait un buffer autour du mcp

# 
# # reshape roads
# buffer_extract_road<-as.data.frame(buffer_extract_road)
# names(buffer_extract_road)<-c("ID","Type")
# 
# buffer_extract_road = buffer_extract_road %>% 
#   filter(Type >0)
# # reorganise the data in appropriate format 
# road<-reshape2::dcast(buffer_extract_road, ID~Type, fun.aggregate=length, value.var="Type")
# #habitate<-table(buffer_extract_habitat$ID,buffer_extract_habitat$Type)
# View(road)
# 
# # reshape only cabin 
# buffer_extract_cabin<-as.data.frame(buffer_extract_cabin)
# names(buffer_extract_cabin)<-c("ID","Type")
# 
# buffer_extract_cabin = buffer_extract_cabin %>% 
#   filter(Type >0)
# # reorganise the data in appropriate format 
# cabin<-reshape2::dcast(buffer_extract_cabin, ID~Type, fun.aggregate=length, value.var="Type")
# View(cabin)
# 
# 
# # roads


# extract roads lengths
saq_extract_road<-raster::extract(sagRoad, sag.mcp, df=TRUE) 

# extract cabin density
sag_extract_cab<-raster::extract(sagCabin, sag.mcp, df=TRUE) #C'est bien les mcp ici ?? 


# saq_extract_road<-as.data.frame(saq_extract_road)
# names(saq_extract_road)<-c("ID","Type")
# head(saq_extract_road)
# 
# saq_extract_road = saq_extract_road %>% 
#   filter(Type >0)
# 
# # reorganise the data in appropriate format 
# sagRd<-reshape2::dcast(saq_extract_road, ID~Type, fun.aggregate=length, value.var="Type")
# #habitate<-table(buffer_extract_habitat$ID,buffer_extract_habitat$Type)
# View(sagHabitat)
# 
# # cabins
# sag_extract_cab<-as.data.frame(sag_extract_cab)
# names(sag_extract_cab)<-c("ID","Type")
# head(sag_extract_cab)
# 
# sag_extract_cab = sag_extract_cab %>% 
#   filter(Type >0)
# 
# # reorganise the data in appropriate format 
# sagCab<-reshape2::dcast(sag_extract_cab, ID~Type, fun.aggregate=length, value.var="Type")
# #habitate<-table(buffer_extract_habitat$ID,buffer_extract_habitat$Type)
# View(sagHabitat)
# 

