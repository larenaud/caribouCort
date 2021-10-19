#modeling caribou HCC ~ DV

library(plyr)
library(dplyr)
library(lme4)
library(AICcmodavg)
# var part
library(MASS)
library(vegan)
library(lubridate)
library(readr)
library(ggplot2)
library(ggthemes)
library(pander)
library(cowplot)
library(car)

getwd()
rm(list=ls())

setwd("~/Documents/LARenaud_Pro/Caribou")

load("Data/spatialCort.RData")
rm(tmp, tmp1, suivi_charlevoix)


#Vérification des corrélations entre les variables.

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


# Preliminary analyses ----------------------------------------------------

data$year <- as.factor(data$year)
data$site <- as.factor(data$site)
data$cortisol = log(data$cortisol)
data[, 12:29] <- scale(data[, 12:29])
data$site<- as.factor(data$site)
data$ID<- as.factor(data$ID)
data$year<- as.factor(data$year)
data$sex<- as.factor(data$sex)

colnames(data)
summary(cortisol_age$cortisol)

# global for our 2 study areas 

levels(cortisol_age$site  )
cortisol_age%>%
  filter(site == "Charlevoix") %>%
  summarise(mean(cortisol), 
            min(cortisol), 
            max(cortisol)) # 9.433198      1.978795      25.97983
cortisol_age%>%
  filter(site == "Portneuf") %>%
  summarise(mean(cortisol), 
            min(cortisol), 
            max(cortisol)) #  5.177703      1.351579      16.81303


# only for those with DV 
data%>%
  filter(site == "Portneuf") %>%
  summarise(mean(cortisol), 
            min(cortisol), 
            max(cortisol))

data%>%
  filter(site == "Charlevoix") %>%
  summarise(mean(cortisol), 
            min(cortisol), 
            max(cortisol))


#this is with a very reduced sample set
data$site <- droplevels(data$site)
data$sex <- droplevels(data$sex)
# data %>% 
#   group_by(site) %>% 
#   summarise(nat = sum(ds, oldConifer, ygConifer, mfmature, wetlands), 
#             cuts = sum(clearcut05, clearcut620, openNoReg), 
#             cabin = sum(cabinsp), 
#             area = mean(area),
#             roads = sum(roadsp12, roadsp34))

table(data$year, data$site) # we have an unbalanced design

#Charlevoix Portneuf
# 2005          0        3
# 2009         11        9
# 2010          3        6
table(data$year, data$ID) # we have an unbalanced design


library("ggpubr")
ggboxplot(data, x = "year", y = "cortisol", color = "site",
          palette = c("#00AFBB", "#E7B800"))

res.aov2 <- aov(cortisol ~ year*site, data = data)
print(summary(res.aov2)) 
# Df Sum Sq Mean Sq F value Pr(>F)  
# year         2  0.738  0.3688   1.064 0.3591  
# site         1  1.349  1.3488   3.892 0.0588 .
# year:site    1  0.962  0.9620   2.775 0.1073  
# Residuals   27  9.358  0.3466 


# exploratory analysis similar to Ewatcha ? 
summary(mod_area <- lm(cortisol~area, data = data))
            #   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.7943522  0.2179567   8.233 3.44e-09 ***
# area        -0.0001977  0.0008643  -0.229    0.821
# Residual standard error: 0.6425 on 30 degrees of freedom
# Multiple R-squared:  0.001742,	Adjusted R-squared:  -0.03153 
# F-statistic: 0.05235 on 1 and 30 DF,  p-value: 0.8206
# 


# Model selection --------------------------------------------
hist(data$cortisol)
data$ageClass # too many na's for now 

#removed clearcut620 
full <-lm(cortisol~site +area + woodlands +  mfmature +oldConifer  + cabinsp + roadsp12 +roadsp34 + clearcut05 + ygConifer,
          data=data) # 10 var + complete model
car::vif(full)
summary(full)
#removed ygConifer 
full2 <-lm(cortisol~site +area + woodlands +  mfmature +oldConifer  + cabinsp + roadsp12 +roadsp34 + clearcut05,
          data=data) # 10 var + complete model
car::vif(full2)
summary(full2)

# only cuts, not age of cuts = same results 
# data$cuts = data$clearcut05 + data$clearcut620

# Cand.mod<-list()
# Cand.mod[[1]]<-lm(cortisol~ 1,data= data)
# Cand.mod[[2]]<-lm(cortisol~area +site + roadsp12 + roadsp34, data=data) # all raods
# Cand.mod[[3]]<-lm(cortisol~ area + site + cabinsp, data=data) # cabinsp 
# Cand.mod[[4]]<-lm(cortisol~  area +site + clearcut05, data=data) # cuts
# Cand.mod[[5]]<-lm(cortisol ~ area +site + woodlands +  mfmature +oldConifer + ygConifer, data=data) # good habitat 
# Cand.mod[[6]]<-lm(cortisol~ area + site + cabinsp + roadsp12 + roadsp34, data=data) # cabin_road
# Cand.mod[[7]]<-lm(cortisol~area +site + cabinsp + roadsp12 + roadsp34 + clearcut05, data=data) # cabin_cuts_roads
# Cand.mod[[8]]<-lm(cortisol~ area +site+woodlands + mfmature +oldConifer + ygConifer + cabinsp + 
#                     roadsp12 + roadsp34,data=data) # good_cabin_roads # vif roads roadsp34 4.645317 
# Cand.mod[[9]]<-lm(cortisol~area + site+woodlands +  mfmature +oldConifer +ygConifer+ cabinsp + clearcut05, 
#                    data=data) # good_cabin_cuts
# Cand.mod[[10]]<-lm(cortisol~area,data=data) # 
# Cand.mod[[11]]<-lm(cortisol~area + sex,data=data) # 
# Cand.mod[[12]]<-lm(cortisol~area + site+ woodlands +  mfmature +oldConifer + roadsp34 +roadsp12 + clearcut05, 
#                    data=data) # good_roads_cuts
# Cand.mod[[13]]<-lm(cortisol~site +area + woodlands +  mfmature +oldConifer  + cabinsp + roadsp12 +roadsp34 + clearcut05,data=data) #full
# Cand.mod[[14]]<-lm(cortisol~area +site +roadsp34, data=data) # raods
# Cand.mod[[15]]<-lm(cortisol~area +site +roadsp12, data=data) # raods

Cand.mod<-list()
Cand.mod[[1]]<-lm(cortisol~ 1,data= data)
Cand.mod[[2]]<-lm(cortisol~year + area +site + roadsp12 + roadsp34, data=data) # all raods
Cand.mod[[3]]<-lm(cortisol~ year +area + site + cabinsp, data=data) # cabinsp 
Cand.mod[[4]]<-lm(cortisol~ year + area +site + clearcut05, data=data) # cuts
Cand.mod[[5]]<-lm(cortisol ~ year +area +site + woodlands +  mfmature +oldConifer + ygConifer, data=data) # good habitat 
Cand.mod[[6]]<-lm(cortisol~ year +area + site + cabinsp + roadsp12 + roadsp34, data=data) # cabin_road
Cand.mod[[7]]<-lm(cortisol~year +area +site + cabinsp + roadsp12 + roadsp34 + clearcut05, data=data) # cabin_cuts_roads
Cand.mod[[8]]<-lm(cortisol~ year +area +site+woodlands + mfmature +oldConifer + ygConifer + cabinsp + 
                    roadsp12 + roadsp34,data=data) # good_cabin_roads # vif roads roadsp34 4.645317 
Cand.mod[[9]]<-lm(cortisol~year +area + site+woodlands +  mfmature +oldConifer +ygConifer+ cabinsp + clearcut05, 
                  data=data) # good_cabin_cuts
Cand.mod[[10]]<-lm(cortisol~year +area,data=data) # 
Cand.mod[[11]]<-lm(cortisol~year +area + sex,data=data) # 
Cand.mod[[12]]<-lm(cortisol~year +area + site+ woodlands +  mfmature +oldConifer + roadsp34 +roadsp12 + clearcut05, 
                   data=data) # good_roads_cuts
Cand.mod[[13]]<-lm(cortisol~year +site +area + woodlands +  mfmature +oldConifer  + cabinsp + roadsp12 +roadsp34 + clearcut05,data=data) #full
Cand.mod[[14]]<-lm(cortisol~year +area +site +roadsp34, data=data) # raods
Cand.mod[[15]]<-lm(cortisol~year +area +site +roadsp12, data=data) # raods


levels(data$year)
levels(data$ID)
data$ID <- droplevels(data$ID)


Cand.mod<-list()
Cand.mod[[1]]<-lmer(cortisol~ 1+ (1|year) + (1|ID),data= data, REML = F)
Cand.mod[[2]]<-lmer(cortisol~ area +site + roadsp12 + roadsp34 + (1|year) + (1|ID), data=data, REML = F) # all raods
Cand.mod[[3]]<-lmer(cortisol~ area + site + cabinsp+ (1|year)+ (1|ID),data=data, REML = F) # cabinsp 
Cand.mod[[4]]<-lmer(cortisol~  area +site + clearcut05+ (1|year)+ (1|ID), data=data, REML = F) # cuts
Cand.mod[[5]]<-lmer(cortisol ~ area +site + woodlands +  mfmature +oldConifer + ygConifer+ (1|year)+ (1|ID), data=data, REML = F) # good habitat 
Cand.mod[[6]]<-lmer(cortisol~ area + site + cabinsp + roadsp12 + roadsp34+ (1|year)+ (1|ID), data=data, REML = F) # cabin_road
Cand.mod[[7]]<-lmer(cortisol~area +site + cabinsp + roadsp12 + roadsp34 + clearcut05+ (1|year)+ (1|ID), data=data, REML = F) # cabin_cuts_roads
Cand.mod[[8]]<-lmer(cortisol~ area +site+woodlands + mfmature +oldConifer + ygConifer + cabinsp + 
                    roadsp12 + roadsp34+ (1|year)+ (1|ID),data=data, REML = F) # good_cabin_roads # vif roads roadsp34 4.645317 
Cand.mod[[9]]<-lmer(cortisol~area + site+woodlands +  mfmature +oldConifer +ygConifer+ cabinsp + clearcut05+ (1|year)+ (1|ID), 
                  data=data, REML = F) # good_cabin_cuts
Cand.mod[[10]]<-lmer(cortisol~area+ (1|year)+ (1|ID),data=data, REML = F) # 
Cand.mod[[11]]<-lmer(cortisol~area + sex+ (1|year)+ (1|ID),data=data, REML = F) # 
Cand.mod[[12]]<-lmer(cortisol~area + site+ woodlands +  mfmature +oldConifer + roadsp34 +roadsp12 + clearcut05+ (1|year)+ (1|ID), 
                   data=data, REML = F) # good_roads_cuts
Cand.mod[[13]]<-lmer(cortisol~site +area + woodlands +  mfmature +oldConifer  + cabinsp + roadsp12 +roadsp34 + clearcut05+ (1|year)+ (1|ID),data=data, REML = F) #full
Cand.mod[[14]]<-lmer(cortisol~area +site +roadsp34+ (1|year)+ (1|ID), data=data, REML = F) # raods
Cand.mod[[15]]<-lmer(cortisol~area +site +roadsp12+ (1|year)+ (1|ID), data=data, REML = F) # raod
aictab(cand.set=Cand.mod,sort=TRUE)

# Model selection based on AICc:
#   
#   K  AICc Delta_AICc AICcWt Cum.Wt     LL
# Mod1   4 69.97       0.00   0.46   0.46 -30.24
# Mod14  7 72.17       2.20   0.15   0.61 -26.75
# Mod10  5 72.74       2.77   0.11   0.72 -30.22
# Mod11  6 73.02       3.05   0.10   0.82 -28.83
# Mod4   7 74.26       4.29   0.05   0.88 -27.80
# Mod15  7 74.60       4.63   0.05   0.92 -27.97
# Mod3   7 75.11       5.14   0.04   0.96 -28.22
# Mod2   8 75.18       5.21   0.03   0.99 -26.46
# Mod6   9 78.13       8.16   0.01   1.00 -25.97
# Mod7  10 80.25      10.28   0.00   1.00 -24.89
# Mod5  10 86.23      16.26   0.00   1.00 -27.87
# Mod12 12 88.06      18.09   0.00   1.00 -23.82
# Mod13 13 92.14      22.17   0.00   1.00 -22.96
# Mod9  12 94.95      24.98   0.00   1.00 -27.26
# Mod8  13 95.43      25.46   0.00   1.00 -24.60
# 


Cand.mod<-list()
Cand.mod[[1]]<-lmer(cortisol~ 1+ (1|ID),data= data, REML = F)
Cand.mod[[2]]<-lmer(cortisol~ year + area +site + roadsp12 + roadsp34 + (1|year) + (1|ID), data=data, REML = F) # all raods
Cand.mod[[3]]<-lmer(cortisol~ year +area + site + cabinsp+ (1|year)+ (1|ID),data=data, REML = F) # cabinsp 
Cand.mod[[4]]<-lmer(cortisol~  year +area +site + clearcut05+ (1|year)+ (1|ID), data=data, REML = F) # cuts
Cand.mod[[5]]<-lmer(cortisol ~ year +area +site + woodlands +  mfmature +oldConifer + ygConifer+ (1|year)+ (1|ID), data=data, REML = F) # good habitat 
Cand.mod[[6]]<-lmer(cortisol~ year +area + site + cabinsp + roadsp12 + roadsp34+ (1|year)+ (1|ID), data=data, REML = F) # cabin_road
Cand.mod[[7]]<-lmer(cortisol~year +area +site + cabinsp + roadsp12 + roadsp34 + clearcut05+ (1|year)+ (1|ID), data=data, REML = F) # cabin_cuts_roads
Cand.mod[[8]]<-lmer(cortisol~ year +area +site+woodlands + mfmature +oldConifer + ygConifer + cabinsp + 
                      roadsp12 + roadsp34+ (1|year)+ (1|ID),data=data, REML = F) # good_cabin_roads # vif roads roadsp34 4.645317 
Cand.mod[[9]]<-lmer(cortisol~year +area + site+woodlands +  mfmature +oldConifer +ygConifer+ cabinsp + clearcut05+ (1|year)+ (1|ID), 
                    data=data, REML = F) # good_cabin_cuts
Cand.mod[[10]]<-lmer(cortisol~year +area+ (1|year)+ (1|ID),data=data, REML = F) # 
Cand.mod[[11]]<-lmer(cortisol~year +area + sex+ (1|year)+ (1|ID),data=data, REML = F) # 
Cand.mod[[12]]<-lmer(cortisol~year +area + site+ woodlands +  mfmature +oldConifer + roadsp34 +roadsp12 + clearcut05+ (1|year)+ (1|ID), 
                     data=data, REML = F) # good_roads_cuts
Cand.mod[[13]]<-lmer(cortisol~year +site +area + woodlands +  mfmature +oldConifer  + cabinsp + roadsp12 +roadsp34 + clearcut05+ (1|year)+ (1|ID),data=data, REML = F) #full
Cand.mod[[14]]<-lmer(cortisol~year +area +site +roadsp34+ (1|year)+ (1|ID), data=data, REML = F) # raods
Cand.mod[[15]]<-lmer(cortisol~year +area +site +roadsp12+ (1|year)+ (1|ID), data=data, REML = F) # raods

table(data$year, data$ID)

aictab(cand.set=Cand.mod,sort=TRUE)

# K   AICc Delta_AICc AICcWt Cum.Wt     LL
# Mod1   3  67.35       0.00   0.98   0.98 -30.24
# Mod10  7  77.03       9.68   0.01   0.98 -29.18
# Mod11  8  78.22      10.87   0.00   0.99 -27.98
# Mod14  9  78.29      10.95   0.00   0.99 -26.06
# Mod4   9  78.70      11.35   0.00   1.00 -26.26
# Mod15  9  79.97      12.62   0.00   1.00 -26.89
# Mod3   9  80.11      12.76   0.00   1.00 -26.96
# Mod2  10  82.22      14.88   0.00   1.00 -25.87
# Mod6  11  86.36      19.01   0.00   1.00 -25.58
# Mod7  12  89.18      21.83   0.00   1.00 -24.38
# Mod5  12  92.58      25.23   0.00   1.00 -26.08
# Mod12 14  98.61      31.26   0.00   1.00 -22.95
# Mod9  14 101.51      34.17   0.00   1.00 -24.40
# Mod13 15 105.30      37.95   0.00   1.00 -22.65
# Mod8  15 107.65      40.30   0.00   1.00 -23.82
a# essayer route 1-2-3-4 ensemble ML 2020-06-19


car::vif(Cand.mod[[13]])
car::vif(Cand.mod[[8]]) # roads34 : 4.65


cort = aictab(cand.set=Cand.mod,sort=TRUE) # mod 1 then mod 14 

plot(Cand.mod[[14]]) # ok overall 

#exporting AIC table
library(xtable)
aictable<-xtable(cort,caption=NULL,label=NULL,align=NULL,
                 digits=NULL,display=NULL,nice.names=TRUE,
                 include.AICc=TRUE,include.LL=TRUE,include.Cum.Wt=FALSE)
#print.xtable(aictable,type="html",
 #             file="Graphs/T1Aictab.html")


# export results  ---------------------------------------------------------
coefficients(Cand.mod[[14]]) 
round(confint(Cand.mod[[14]]),2)

summary(Cand.mod[[14]]) 
summary(Cand.mod[[13]]) 
ml=c("(Intercept)","sitePortneuf" ,"sexM", "area","woodlands", "mfmature" ,"oldConifer", "cabinsp" , "roadsp12" ,"roadsp34" , "clearcut05", "ygConifer")

mod.avg.coef<-plyr::ldply(ml,function(i){
  avg<-modavg(Cand.mod,parm=i,gamdisp=NULL,conf.level=0.95,second.ord=TRUE,nobs=NULL,
           warn=TRUE,uncond.se="revised")
  cbind(i,as.data.frame(avg[c("Mod.avg.beta","Lower.CL","Upper.CL")]))
})
# get all mod.averaged component into a single table
#mod.avg.coef<-rbind(tmp,mod.avg.coef)

avg<-xtable(mod.avg.coef,caption=NULL,label=NULL,align=NULL,
            digits=NULL,display=NULL,nice.names=TRUE,
            include.AICc=TRUE,include.LL=TRUE,include.Cum.Wt=FALSE)

getwd()

#print.xtable(avg,type="html",file="Graphs/T2Modavg_coef_ci.html")#

#validation
plot(Cand.mod[[9]]) # 2 points outlier but... not removed

# ....... et malheureusement le modèle nul est le meilleur. 

#Model validation
par(mfrow=c(2,2))
plot(Cand.mod[[14]])
#Extract residuals
E1<-resid(Cand.mod[[14]])
E2<-rstandard(Cand.mod[[14]])
F<-fitted(Cand.mod[[14]])
# The residuals E2 are the standardised residuals, and are better. This command plots
# the (stabndardised) residuals versus fitted values (useful for checking homogeneity).
plot(F, E2, xlab="Fitted values", ylab="Residuals")
abline(0,0)

# variance partitioning ---------------------------------------------------
colnames(data)

natural <-subset(data, select =c(woodlands, oldConifer, mfmature, ygConifer))
cuts <-subset(data, select =c(clearcut05, clearcut620, partCut)) # not in model
cabins_roads <- subset(data, select =c(cabinsp, roadsp12, roadsp34))

print(partitionY <- varpart(data$cortisol, natural, cuts, cabins_roads))
opar <- par()
par(mfcol = c(1, 1))
showvarparts(3)
plot(partitionY)
#par(opar)
pdf("Graphs/RenaudFig4VarPart.pdf", width = 8.8, height = 8.8, pointsize = 12)
varpart = plot(partitionY,Xnames = c('natural \n\ habitat', 'forest cuts   ', "cabins and roads   "),
     bg = c('navy', 'tomato', 'orange'), 
     bty = "n")
dev.off()


  # prediction figures ------------------------------------------------------
MuMIn::model.sel(Cand.mod) # all coefficients showed for reference
# cortisol = cortisol[!cortisol$old_mixed >=5,]
colnames(data)
newdDat.base<-as.data.frame(t(colMeans(data[,c("clearcut05","cabinsp","roadsp12","area","woodlands","mfmature","oldConifer", "ygConifer")])))
newdDat.base2<-newdDat.base
for(i in 2:100) 
  newdDat.base2<-rbind(newdDat.base2,newdDat.base)
newdDat.base2$site<-as.factor(rep("Charlevoix",100))
newdDat.base2$sex<-as.factor(rep("F",100))

#road
x=seq(min(data$roadsp34),max(data$roadsp34),length.out=100)# no more 50 values per sex
nd<-newdDat.base2
nd$roadsp34=c(x)#replace fem with our sequence, twice, we will predict these values

pred=modavgPred(Cand.mod,newdata=nd)#Gab : note that I only used the models containing the parameter of interest,similarly to what mod avg does.
nd<-cbind(nd,pred)#
nd$y<-nd$mod.avg.pred 
nd$ci.h<-nd$mod.avg.pred+1.96*nd$uncond.se
nd$ci.l<-nd$mod.avg.pred-1.96*nd$uncond.se

fig_roads<-ggplot(nd,aes(x=roadsp34,y=y))+
  #scale_linetype_manual(values=c("solid","dotted"))+
  geom_smooth(se=F, method = "lm")+
  geom_ribbon(aes(ymin = ci.l, ymax=ci.h),se = F, alpha = 0.2, fill = "blue")+
  geom_point(data=data,aes(x = roadsp12, y=cortisol), size = 2)+
  labs(y=expression("log-cortisol concentration "("pg·mg"^1)),
                    x="Proportion of minor roads (scaled)") + 
  theme(axis.text=element_text(size=10), 
        axis.title = element_text(size =10),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) + 
  theme_cowplot(12) 
print(fig_roads)
#ggsave("Graphs/RenaudFig3Roads.pdf", width= 8.8, height = 8.8, units =  "cm")


# factors 
newdDat.base<-as.data.frame(t(colMeans(data[,c("clearcut05","cabinsp","roadsp12","roadsp34", "area","woodlands","mfmature","oldConifer", "ygConifer")])))
#select 200rows;starting from the data makes sure no column are missing
newdDat.base2<-newdDat.base
for(i in 2:100) 
  newdDat.base2<-rbind(newdDat.base2,newdDat.base)
#newdDat.base2$site<-as.factor(rep(c("Charlevoix", "Portneuf"),50))
newdDat.base2$site<-as.factor(c(rep("Charlevoix",50),rep("Portneuf",50)))
newdDat.base2$sex<-as.factor(c(rep("M",50),rep("F",50)))

pred=modavgPred(Cand.mod, newdata=newdDat.base2)

# newdDat.base2$predi <- predict(mod.7,newdata=newdDat.base2,type="link",re.form=NA,allow.new.levels=T)
# myfun <- function(x)predict (x,newdata=newdDat.base2,type="link",re.form=NA,allow.new.levels=T)
# boo <- bootMer(mod.7, myfun, 1000)
# newdDat.base2$predi <- apply(boo$t, 2, mean)
# newdDat.base2$max <- apply(boo$t, 2, function(x) quantile(x, 0.975))
# newdDat.base2$min <- apply(boo$t, 2, function(x) quantile(x, 0.025))

str(pred)
nd<-cbind(newdDat.base2,pred)#binddataandprediction
nd$y<-nd$mod.avg.pred#expisreverseoflog
nd$ci.h<-nd$mod.avg.pred+1.96*nd$uncond.se#getCI
nd$ci.l<-nd$mod.avg.pred-1.96*nd$uncond.se

fig_site=ggplot(nd,aes(x=site,y=y, ymin = ci.l, ymax = ci.h))+
  geom_pointrange(size = 1.5)+
  labs(x="Site",y="")+ 
  theme(axis.text=element_text(size=10), 
        axis.title = element_text(size =10),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) + 
  theme_cowplot(12)
fig_site
#ggsave("Graphs/RenaudFig3bSite.pdf", width= 8.8, height = 8.8, units =  "cm")


plot <- plot_grid(fig_roads, fig_site, 
                  ncol = 2, 
                  labels = c("A", "B"))
plot

save_plot("Graphs/FIG2RoadsSite.pdf", plot, 
          base_aspect_ratio = 1,
          ncol = 2, 
          nrow = 1)


table(id=data$ID, year = data$year, site = data$site, surv = NA)
t = tibble(ID=data$ID, year = data$year, site = data$site, surv_t1 = NA)

t%>%arrange(ID)

write.csv2(t, 'survTable.csv', row.names = F)
