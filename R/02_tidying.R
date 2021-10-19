# data tidying caribou - from old data files 
#Script created 25-02-2015 from script Modele.R. 
# modified July 21 2017 by L. Renaud (helped by G. Pigeon apparently...)
# modified Nov. 2019 for the last time ! 
library(car)
library (lme4)
library(AICcmodavg)
library(tidyverse)
library(plyr)
library(dplyr)
library(readxl)

getwd()
rm(list=ls())

setwd("~/Documents/LARenaud_Pro/Caribou")

cortisol_age <- read.delim2("~/Documents/LARenaud_Pro/Caribou/Data/all_cortisol_age.txt", comment.char="#")

cortisol_age <- cortisol_age[!cortisol_age$CV >= 21,]

colnames(cortisol_age)
colnames(cortisol_age) <-c("site","sector","year","ID","sex","age","cortisol","plate","cv")
# string = cortisol_age$ID
# cortisol_age$ID= string %>% str_replace("P0", "PO") #attn do not change all 0 for O
# string = cortisol_age$ID
# cortisol_age$ID= string %>% str_replace("C0", "CO")
# string = cortisol_age$ID
# cortisol_age$ID= string %>% str_replace("B0", "BW") # marche pas bien... changer dans saguenay
# cortisol_age$ID= string %>% str_replace("PF", "P") # marche pas bien... changer dans saguenay
# #cortisol_age$ID[34] <- "PO409"


cortisol_age$ID <- as.factor(cortisol_age$ID)
table(cortisol_age$site, cortisol_age$sector)

# tidy id
levels(cortisol_age$ID)
cortisol_age$ID <- stringr :: str_remove(cortisol_age$ID , "_")
cortisol_age$idyear = paste(cortisol_age$ID,cortisol_age$year, sep = "-")



# merging habitats  -------------------------------------------------------
load("Data/spatialDataIntersect.RData")
levels(charlevoixData$id)


colnames(charlevoixData)
colnames(sagData)

load("Data/locClear.RData")

sagData <-merge(sagData, 
             unique(sagLocs[, c("ID", "year", "idyear")]), 
             by.x = "id", 
             by.y = "idyear", 
             all.x =T)

# match with cortisol age
cortisol_age$ID

sagData$ID.y <- stringr :: str_remove(sagData$ID.y , "_")
sagData$ID.y <- stringr :: str_remove(sagData$ID.y , "-")
sagData$ID.y= stringr :: str_replace(sagData$ID.y,c("CO","PO"), c("C0", "P0"))
sagData$ID.y= stringr :: str_replace(sagData$ID.y,"PF", "P") # marche pas bien... changer dans saguenay
sagData$ID.y[1] = "C0712"
string = sagData$ID.y

string = c("C0712","C1018", "C0401","C0402","C0403","C0404","C0405","C0406","C0401","C0401", 
 "C0402","C0402","C0402","C0402","C0404","C0404","C0507","C0507","C0507","C0507" ,
 "C0509","C0509","C0610","C0711","C0711","C0712","C0712","C0712","C0815","C0815" ,
 "P0401","P0402","P0403","P0404","P0405","P0406","P0407","P0408","P0409","P0411" ,
 "P0412","P0414","P0415","P0416","P0417","P0418","P0401","P0401","P0404","P0404" ,
 "P0404","P0407","P0407","P0408","P0409","P0409","P0409","P0412","P0412","P0412" ,
 "P0412","P0414","P0414","P0414","P0414","P0415","P0415","P0415","P0416","P0416" ,
 "P0416","P0416","P0416","P0417","P0418","P0520","P0519","P0519","P0519","P0519" ,
 "P0520","P0520","P0520","P0520","P0522","P0621","P0724","P0724","P0724","P0823" ,
 "P0823","P0826","P0826","P0826","P0827","P0828","P0828","P0829","P0829","P0930") 
tmp<-cbind(sagData$ID.y, string)
tmp<-merge(tmp, sagData, by.x="V1", by.y="ID.y", all = F)

tmp=tmp %>% distinct(.keep_all = TRUE) 
tmp <- tmp[, -c(3,5)]
#tmp1<-merge(tmp, cortisol_age, by.x=c("string","year"), by.y=c("ID", "year")) #n=20

tmp$idyear = paste(tmp$string,tmp$year, sep = "-")
sagData <- tmp

sagData <- sagData[c("year","idyear","string","area","clearcut05","clearcut620" , "ds","human","mfmature",
                     "natPert05","natPert620","oldConifer","openNoReg","partCut","powerLine",
                     "regeneration", "water","wetlands","ygConifer","roadsp12","roadsp34","cabinsp" )]

colnames(sagData) <- c("year","idyear","ID","area","clearcut05","clearcut620" , "ds","human","mfmature",
                     "natPert05","natPert620","oldConifer","openNoReg","partCut","powerLine",
                     "regeneration", "water","wetlands","ygConifer","roadsp12","roadsp34","cabinsp")

colnames(charlevoixData)
colnames(charlevoixLocs)

charlevoixData <-merge(charlevoixData, 
                unique(charlevoixLocs[, c("ID", "year", "idyear")]), 
                by.x = "id", 
                by.y = "idyear", 
                all.x =T)

charlevoixData$ID <- stringr :: str_remove(charlevoixData$ID.y , "_")
charlevoixData$ID <- stringr :: str_remove(charlevoixData$ID.y , "-")

charlevoixData$idyear = paste(charlevoixData$ID,charlevoixData$year, sep = "-")
charlevoixData <- charlevoixData[, -c(1,3,21,24)]


charlevoixData <- charlevoixData[c("year","idyear","ID","area","clearcut05","clearcut620" , "ds","human","mfmature",
                     "natPert05","natPert620","oldConifer","openNoReg","partCut","powerLine",
                     "regeneration", "water","wetlands","ygConifer","roadsp12","roadsp34","cabinsp" )]

colnames(charlevoixData)
colnames(sagData)

spatialData = rbind(charlevoixData, sagData)

colnames(spatialData) <- c("year","idyear","ID","area","clearcut05","clearcut620" , "woodlands","human","mfmature",
                                   "natPert05","natPert620","oldConifer","openNoReg","partCut","powerLine",
                                   "regeneration", "water","wetlands","ygConifer","roadsp12","roadsp34","cabinsp" )


# merge with cortisol 
spatialData$hair_yr <- as.numeric(as.character(spatialData$year)) + 1
spatialData$idyear = paste(spatialData$ID,spatialData$hair_yr, sep = "-")

data <- merge(cortisol_age, 
             spatialData,
             by.x = c("ID", "idyear", "year"),
             by.y = c("ID", "idyear", "hair_yr")) # attention there's a time lag here _ hair collected the following winter. 
data$sp_yr <- data$year.y

#clean 

colnames(data)

data <- data[c("year","sp_yr","idyear","ID","site", "area",  "sex", "age", "cortisol", "plate", "cv",
               "clearcut05","clearcut620" , "woodlands","human","mfmature",
                     "natPert05","natPert620","oldConifer","openNoReg","partCut","powerLine",
                     "regeneration", "water","wetlands","ygConifer","roadsp12","roadsp34","cabinsp" )]

#save(spatialData, cortisol_age, data, file = "Data/spatialCort.RData")


# tidying  ----------------------------------------------------------------


# maybe add age in years ? 
suivi_charlevoix <- read_delim("Data/suivi_charlevoix.csv", ";", escape_double = FALSE, col_types = cols(`Etiq-D` = col_skip(), 
                                                                                                         `Etiq-G` = col_skip(), Lat = col_skip(), 
                                                                                                         Long = col_skip(), `Pan-D` = col_skip(), 
                                                                                                         `Pan-G` = col_skip(), Remarques = col_skip(), 
                                                                                                         Secteur = col_skip(), collier_ID = col_skip(), 
                                                                                                         collier_ptt = col_skip(), collier_type = col_skip()),
                               trim_ws = TRUE)

suivi_charlevoix<- suivi_charlevoix[, 1:7]
suivi_charlevoix$year <- year(ymd(suivi_charlevoix$Date))
suivi_charlevoix$Date <- ymd(suivi_charlevoix$Date)
colnames(suivi_charlevoix)<-c("ID","sex","est_age","a_age","date", "freq","death_cause", "year"   )

library(stringr)
string = suivi_charlevoix$ID
suivi_charlevoix$ID=string %>% str_replace("-", "") # ..

sort(suivi_charlevoix$ID)
string =c("07516",  "07516" , "07516" , "07516" , "07516" , "07619",  "07709" , "07709"  ,"11503" , "11503" , "11503" , "21502" , "21502",
          "21502" , "21502" , "21604" , "21702" , "21702" , "24325" , "24706" , "502710", "502710" ,"50708" , "50708" , "51327B" ,"52601" ,
          "52601" , "52601" , "52601" , "52704" , "52704" , "53501" , "53501" , "53501"  ,"53501" , "55330" , "55330" , "57513" , "57513" ,
          "57513" , "59506" , "59506" , "59506" , "59506" , "65324" , "68519" , "68519" , "68519" , "69520"  ,"69520", "69520" , "69520" ,
          "70605" , "70605" , "70605" , "70605" , "71518" , "71701" , "71701" , "71701" , "73521" , "73521" , "73521" , "73521"  ,"73618" ,
          "73618" , "73618" , "73618" , "73713" , "73713",  "74620" , "79714" , "79714" , "81714" , "81714")
string=paste0("GJ",string)

tmp<-cbind(sort(suivi_charlevoix$ID)[1:75], string)
tmp<-merge(tmp, suivi_charlevoix, by.x="V1", by.y="ID", all = F)
tmp=tmp %>% distinct(.keep_all = TRUE) # n=75

tmp1<-merge(tmp, suivi_charlevoix, by.x=c("V1","year"), by.y=c("ID", "year"), all = T) #n=20

tmp1$V1 <- ifelse(is.na(as.character(tmp1$string)), as.character(tmp1$V1), as.character(tmp1$string))
tmp1$string <- NULL
tmp1

tmp1<- tmp1[,c(1:2,9:13 )]
colnames(tmp1)<-c("ID","year","sex","est_age","a_age","date",
                  "freq")
tmp1$ID <-as.factor(as.character(tmp1$ID))
# tmp1<-tmp1%>%
#   group_by(ID) %>%
#   mutate(YD = max(year))

# make age class

tmp1$ageClass = NA
tmp1$a_age = as.numeric(tmp1$a_age)
tmp1$ageClass <- as.factor(ifelse(tmp1$a_age > 9,"old", ifelse(tmp1$a_age <= 3.5,"young", ifelse(tmp1$a_age > 4 & tmp1$a_age < 9,"prime",NA))))


# merge with data avec l'année de poil ?? à corriger sinon pour sp_yr

data <- merge(data, 
              tmp1[, -c(3:5,7)], 
              by.x = c("ID", "year"), # l'age au moment de la récolte de poil
              by.y = c("ID", "year"),
              all.x = T)

#Vérification des corrélations entre les variables.
library(dplyr)
correl <- round(cor(data[,12:29]),2)
cor_df <- as.data.frame(as.table(correl))
cor_df %>%  arrange(desc(Freq)) %>% filter(abs(Freq)>0.6&abs(Freq)<1)

# Var1        Var2  Freq
# 1     wetlands       water  0.91
# 2        water    wetlands  0.91
# 3    ygConifer    wetlands  0.89
# 4     wetlands   ygConifer  0.89
# 5      partCut   openNoReg  0.85 *
# 6    openNoReg     partCut  0.85
# 7    powerLine     partCut  0.83
# 8      partCut   powerLine  0.83
# 9    ygConifer       water  0.75
# 10       water   ygConifer  0.75
# 11  natPert620   natPert05  0.74 *
# 12   natPert05  natPert620  0.74 
# 13   powerLine   openNoReg  0.73 *
# 14   openNoReg   powerLine  0.73
# 15    mfmature       human  0.69
# 16   openNoReg       human  0.69
# 17       human    mfmature  0.69
# 18       human   openNoReg  0.69
# 19    roadsp34 clearcut620  0.67 *
# 20 clearcut620    roadsp34  0.67
# 21   natPert05 clearcut620  0.62 *
# 22 clearcut620   natPert05  0.62 
# 23    roadsp12   ygConifer -0.61 *
# 24   ygConifer    roadsp12 -0.61 
# 25    roadsp12       water -0.66
# 26       water    roadsp12 -0.66
# 27    roadsp12    wetlands -0.67 *
# 28    wetlands    roadsp12 -0.67  
# 29   ygConifer clearcut620 -0.69* 
# 30 clearcut620   ygConifer -0.69
# 31       water       human -0.70
# 32       human       water -0.70
# 33       water  natPert620 -0.71
# 34  natPert620       water -0.71
# 35   ygConifer  natPert620 -0.73 *
# 36  natPert620   ygConifer -0.73
# 37    wetlands  natPert620 -0.77
# 38  natPert620    wetlands -0.77
# 
# 
# 
# resave data -------------------------------------------------------------

#save(list = ls(), file = "Data/spatialCort.RData")


# survie Charlevoix  --------------------------------------
rm(bowLocs, charlevoixData, charlevoixLocs, sagData, sagLocs, tmp)

surv <- read_excel("Data/raw/originaux/CharlevoixORIGINAL.xls")

surv <- surv[1:85,]
string = surv$Ident

surv$Ident=paste0("GJ",string)
surv$Ident <- stringr :: str_remove(surv$Ident , "-")


dt1 <-surv%>%dplyr::select(Ident, contains("Status")) %>%
  mutate(mysurv=case_when(
    Status %in% c("m") ~ "FALSE",
    Status %in% c("d") ~ "NA",
    is.na(Status)~"NA",
    Status %in% c("v") ~ "TRUE",
    TRUE ~ "1" 
  ))


cortisol_age <- na.omit(cortisol_age)
cortisol_age$alive_t1 <- NA
# loop

for(i in 1:nrow(cortisol_age)){
  
  tmp=surv[surv$Ident == cortisol_age$ID[i],]
  if(nrow(tmp)>0 ){
  if(!is.na(tmp$Status) &!is.na(tmp$Ident)){
  if(tmp$Status=="m"){
    cortisol_age$alive_t1[i]=ifelse((cortisol_age$year[i]+1)<tmp$Status_An,1,0)
  }
  if(tmp$Status =="v"){
    cortisol_age$alive_t1[i]=ifelse((cortisol_age$year[i]+1)<=tmp$Status_An,1,NA)
  }
  }
  }
  }






dt2<-merge(dt1, data, by.x=c("Ident", "Status_An"), by.y=c("ID", "year"), all.y = T) #n=20

dt2$alive_t1 <- dt2$Annee_D < dt2$Annee_F

dt1$alive_t1 <- dt1$year < dt1$YD
dt1[is.na(dt1$YD) ,"alive_t1"] <- T
dt1[dt1$year==max(dt1$year),"alive_t1"] <- NA
 







# Survie saguenay ------------------------------------------------------------------

#suivi saguenay
library(readxl)
suivi_sag <- read_excel("Data/suivi_saguenay.xls")
colnames(suivi_sag)
suivi_sag <- suivi_sag[, c(1:4)]
colnames(suivi_sag) <- c("site_sag", "ID", "sex_sag", "age_sag")
# trim -
library(stringr)
string = suivi_sag$ID
suivi_sag$ID= string %>% str_replace("-", "") # marche pas bien... 
suivi_sag$site_sag[31:46]= "Portneuf"

suivi_sag$site_sag[47:78]= "Piraube"
suivi_sag$age[1:16]= NA

tmp4<- merge(suivi_sag[1:4], cortisol_age, by.x = c("ID", "site_sag"), by.y = c("ID", "site"), all=F) # il en manque pas mal
which(!tmp4$sex==tmp4$sex_sag)# le fait que ce soit toutes des femelles est-il supposé ou vérifié? 
colnames(tmp4)
tmp4 = tmp4[, c("ID","site_sag", "age_sag" , "sector","year","sex", "cortisol" ,"plate", "cv")] #  n= 61
colnames(tmp4)= c("ID","site", "age" , "sector","year","sex", "cortisol" ,"plate", "cv")

boxplot(tmp4$cortisol~tmp4$sex) # just 1 male? 
# 
# # la rorm(tmp, tmp1, tmp2, tmp3,prop, a,b)

suivi_sag <- read_excel("Data/suivi_saguenay.xls")

dt1=suivi_sag%>%dplyr::select(Individu, contains("Fréq")) %>%
  gather("year", "status", -Individu) %>%
  mutate(year = as.numeric(paste0("20",substr(year, 9,10)))) %>% 
  mutate(status2=case_when(
    status %in% c("Mort", "mort") ~ "0",
    status %in% c("Enlevé", "enlevé", "Non-repéré", "Attente") ~ "NA",
    is.na(status)~"NA",
    status %in% c("ok") ~ "1",
    TRUE ~ "1" 
  )) %>% 
  mutate(first_dead = ifelse(status2=="0",year,NA)) %>% 
  mutate(max_year = (year*(status2=="1"))) %>% 
  group_by(Individu) %>%
  summarise(first_dead = min(first_dead,na.rm = T),
            last_live=max(max_year)) %>%
  mutate(first_dead=ifelse(is.infinite(first_dead),NA,first_dead),
         last_live=ifelse(last_live==0,NA,last_live))

string = dt1$Individu
dt1$Individu= string %>% str_replace("-", "") # marche pas bien... 

dt2=merge(tmp4, dt1, by.x="ID", by.y="Individu") %>%
  mutate(surv_t1 = ifelse(last_live >= (year+1),1,NA)) %>%
  mutate(surv_t1b= ifelse(first_dead == (year+1),0,NA )) %>%
  mutate(weird = year>last_live)


# si l'année de sample est 2010 mettre NA pour la surv. 

# check les années d'échantillons de cortisol?? pour les individus à Portneuf? ...2009-2020
table(dt1$status2)

# t'es mieux de travailler avec la carte de l'année qui fitte tes points (ludo)
# tu peux pas mettre du texte dans un fichier raster - transformer en valeurs
# convertir fi. texte en valeur numérique que tu peux rasterirser ton poly gone et gosser dans R. Donner valeur numérique à ta classe d'habitat pour être capable de rasteriser. 







# suivi_romaine <- read_excel("Data/suivi_romaine.xlsx")
# suivi_romaine$date <- ymd(suivi_romaine$date) # marche pas. IMPORTANT!!! 
# 
# string = suivi_romaine$capture
# suivi_romaine$capture=string %>% str_replace("-", "") # .. 
# string = suivi_romaine$poil2009
# suivi_romaine$poil2009=string %>% str_replace("-", "") # .. 
# suivi_romaine$poil2009[5] = NA
# string = suivi_romaine$poil2010
# suivi_romaine$poil2010=string %>% str_replace("-", "") # .. 
# suivi_romaine$site = "Romaine"
# colnames(suivi_romaine) = c("ID","date","year_suivi", "sex","age","age.est", "hair.2009",  "hair.2010" , "site")
# 
# string = suivi_romaine$ID
# suivi_romaine$ID=string %>% str_replace("C0", "CO") # 
# 
# tmp5 = merge(suivi_romaine, cortisol_age, by.x = c("ID","site", "year_suivi"), by.y= c("ID","site", 'year'), all = F)
# colnames(tmp5)
# tmp5 = tmp5[,c("ID","site","date","age.x","age.est","year_suivi","sex.y","cortisol", "plate","cv")]
# colnames(tmp5) = c("ID","site","date","age","age.est", "year","sex","cortisol", "plate","cv")
# tmp5=tmp5 %>% distinct(cortisol,.keep_all = TRUE)
# # ça chie il faut vraiment que j'ai la date - les age et le cortisol ne correspondent pas. 
# # ok avec au moins l'année



# adding sex - age - predation  -------------------------------------------

# rebuild database ; adding age and sex and predation 
# 
# suivi_charlevoix <- read_excel("Data/suivi_charlevoixS_2003-10_modified.xls")
# colnames(suivi_charlevoix)[7]<- "collier_ID"
# colnames(suivi_charlevoix)[8]<- "collier_type"
# colnames(suivi_charlevoix)[9]<- "collier_ptt"
# colnames(suivi_charlevoix)[24]<- "date_misebas"
# colnames(suivi_charlevoix)[25]<- "secteur_misebas"
# colnames(suivi_charlevoix)[28]<- "age_heure"
# 
# suivi_charlevoix <- suivi_charlevoix[-1, ]
# 
# # complete all ID
# suivi_charlevoix[6:11, 1]<- "GJ06"
# suivi_charlevoix[12:18, 1]<- "GJ07"
# suivi_charlevoix[26, 1]<- "07-709"
# suivi_charlevoix[20:23, 1]<- "07-516"
# # suivi_charlevoix[31:35, 1]<- "GJ11"
# suivi_charlevoix[37:38, 1]<- "11-503"
# suivi_charlevoix[49:56, 1]<- "GJ21"
# suivi_charlevoix[57:60, 1]<- "21-502"
# suivi_charlevoix[62, 1]<- "502-710"
# suivi_charlevoix[65, 1]<- "21-702"
# suivi_charlevoix[69:72, 1]<- "GJ24"
# suivi_charlevoix[77:79, 1]<- "GJ26"
# suivi_charlevoix[84:85, 1]<- "GJ30"
# suivi_charlevoix[88:93, 1]<- "GJ50"
# suivi_charlevoix[95, 1]<- "50-708"
# suivi_charlevoix[99:104, 1]<- "GJ52"
# suivi_charlevoix[106:108, 1]<- "52-601"
# suivi_charlevoix[110, 1]<- "52-704"
# suivi_charlevoix[112:118, 1]<- "GJ53"
# suivi_charlevoix[120:122, 1]<- "53-501"
# suivi_charlevoix[124, 1]<- "GJ54"
# suivi_charlevoix[126:130, 1]<- "GJ55"
# suivi_charlevoix[132, 1]<- "55-330"
# # finish the job in excel. 

# #write.csv(suivi_charlevoix, file  = "suivi_charlevoix_modified.csv")
# suivi_charlevoix <- read.csv("Data/suivi_charlevoix.csv", sep=";", comment.char="#")
# summary(suivi_charlevoix)
# 
# colnames(suivi_charlevoix)
# suivi_charlevoix<- suivi_charlevoix[,c(1:4, 6, 17)] # pour le moment pas de RS
# suivi_charlevoix$alive <- NA
# suivi_charlevoix$predation <- NA
# 
# 
# library(lubridate)
# suivi_charlevoix$year <- year(ymd(suivi_charlevoix$Date))
# suivi_charlevoix$Date <- ymd(suivi_charlevoix$Date)
# 
# # à continuer avec  les strings 
# suivi_charlevoix$alive[1:17]<- 1
# suivi_charlevoix$alive[19:23]<- 1
# colnames(suivi_charlevoix)
# 
# colnames(suivi_charlevoix)<-c("ID","sex","est_age","a_age","date","notes", "alive","predation", "year" )
# library(stringr)
# string = suivi_charlevoix$ID
# suivi_charlevoix$ID=string %>% str_replace("-", "") # .. 
# 
# sort(suivi_charlevoix$ID)
# string =c("07516",  "07516" , "07516" , "07516" , "07516" , "07619",  "07709" , "07709"  ,"11503" , "11503" , "11503" , "21502" , "21502", 
#            "21502" , "21502" , "21604" , "21702" , "21702" , "24325" , "24706" , "502710", "502710" ,"50708" , "50708" , "51327B" ,"52601" ,
#            "52601" , "52601" , "52601" , "52704" , "52704" , "53501" , "53501" , "53501"  ,"53501" , "55330" , "55330" , "57513" , "57513" ,
#            "57513" , "59506" , "59506" , "59506" , "59506" , "65324" , "68519" , "68519" , "68519" , "69520"  ,"69520", "69520" , "69520" ,
#            "70605" , "70605" , "70605" , "70605" , "71518" , "71701" , "71701" , "71701" , "73521" , "73521" , "73521" , "73521"  ,"73618" ,
#            "73618" , "73618" , "73618" , "73713" , "73713",  "74620" , "79714" , "79714" , "81714" , "81714")
# string=paste0("GJ",string)
# tmp<-cbind(sort(suivi_charlevoix$ID)[1:75], string)
# tmp<-merge(tmp, suivi_charlevoix, by.x="V1", by.y="ID", all = F)
# tmp= tmp[, c(2:6,10)]
# tmp=tmp %>% distinct(.keep_all = TRUE) # n=75
# tmp1<-merge(tmp, cortisol_age, by.x=c("string","year"), by.y=c("ID", "year")) #n=20
# colnames(tmp1) = c("ID","year","sex.x","est_age","a_age","date","site","sector","sex.y", "age","cortisol", "plate","cv" )
# 
# # duplicated(tmp1)
# # ID = tmp1$string[!duplicated(tmp1$string)]
# # tmp1=tmp1 %>% distinct(string, .keep_all = TRUE)
# tmp1=tmp1 %>% distinct(.keep_all = TRUE) # n=20
# tmp1= tmp1[, c("ID","sex.x","year","est_age","a_age","date", "site","sector","age","cortisol","plate","cv")]
# colnames(tmp1) = c("ID","sex","year","est_age","a_age","date", "site","sector","age","cortisol","plate","cv")
# 
# which(!tmp1$sex.x==tmp1$sex.y) # marche pas ; pas les mêmes niveaux
# boxplot(tmp1$cortisol~tmp1$age)
# boxplot(tmp1$cortisol~tmp1$sex)
# 
# tmp2<-merge(suivi_charlevoix, cortisol_age, all=F) #all remaining GJ ID n=45
# tmp2=tmp2 %>% distinct(.keep_all = TRUE)
# 
# # uniformiser avec tmp1
# colnames(tmp2) 
# tmp2= tmp2[, c("ID","sex","year","est_age","a_age","date", "site","sector","age","cortisol","plate","cv")]
# tmp3 = rbind(tmp1, tmp2)
# tmp3$est_age[51] = "5.5"
# tmp3$est_age[53] = "6.5"
# tmp3$est_age[36] = "8.5"
# droplevels(tmp3$est_age)
# droplevels(tmp3$a_age)
# 
# boxplot(tmp3$cortisol~tmp3$a_age) # vérifier ces quoi ces na ?? 
# boxplot(tmp3$cortisol~tmp3$est_age) # vérifier ces quoi ces na ?? 
# 
# boxplot(tmp2$cortisol~tmp2$age) 
# boxplot(tmp2$cortisol~tmp2$sex) 
# 
# 
# boxplot(tmp5$cortisol~tmp5$age)
# boxplot(tmp5$cortisol~tmp5$age.est)
# boxplot(tmp5$cortisol~tmp5$sex)
# boxplot(tmp5$cortisol~tmp5$year)
