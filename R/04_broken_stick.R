# broken stick 

library(tidyverse)
library(boot)
rm(list = ls())

load("Data/spatialCort.RData")

data$cortisol = log(data$cortisol)
#data[, 12:29] <- scale(data[, 12:29])
data$site<- as.factor(data$site)
data$ID<- as.factor(data$ID)
data$year<- as.factor(data$year)
data$sex<- as.factor(data$sex)

data <- data[data$sex== "F",] # 26

# faire la somme des perturbations 
data$cumul = rowSums(data[, c(27:29)])

#0.04195609 + 0.60790065 + 0.064372413
par(mfrow=c(1,1))
hist(data$cumul)

m1=lm(cortisol~cumul,data = data)
summary(m1)

brok.stick<-function(x,x.break){
  (x-x.break)*(x>=x.break)
}

m2=lm(cortisol~cumul+brok.stick(cumul,0.367),data = data)
summary(m2)

bp_tab=map_df(seq(0,1,0.1), function(i){ # essayer des valeur de bpoint plus petit 
  m2=lm(cortisol~cumul+brok.stick(cumul,i),data = data)
  tibble(break.point=i,
         aic=AIC(m2))
})

plot(bp_tab)
bp_tab[which.min(bp_tab$aic),]

m2=lm(cortisol~cumul+brok.stick(cumul,0.5),data = data)
summary(m2)
round(confint(m2),2)

newdat<-expand.grid(cumul=seq(min(data$cumul), max(data$cumul), length = 50))
newdat$y=predict(m2,newdata = newdat)

df = data %>%
  filter(cumul >0.5)


ggplot(data,aes(x=cumul,y=cortisol))+
  geom_point(size = 2) + 
  geom_path(data=newdat,aes(y=y),color="blue", size = 1.5) + 
  scale_x_continuous(limits = c(0, 1.1),breaks=seq(0, 1,0.1)) +
  scale_y_continuous(limits = c(0, 3.3),breaks=seq(0, 3.3,0.5)) +
      labs(y=expression("log-cortisol concentration "("pg·mg"^1)), 
       x="Proportion of home range disturbed (%)") + 
    theme_cowplot(10)
ggsave("Graphs/RenaudFig3Broken.pdf", width = 10 , height = 8.8 , unit = "cm")


#20/26

#m2=glm(cbind(nb_alive,nb_dead)~snow_log_up_jul+brok.stick(snow_log_up_jul,128),family = "binomial",data = dt3)

newdat<-data.frame(snow_log_up_jul=90:150)
newdat$y=inv.logit(predict(m2,newdata = newdat))

ggplot(dt3,aes(x=snow_log_up_jul,y=(nb_alive/(nb_alive+nb_dead))))+geom_point()+
geom_path(data=newdat,aes(y=y),color="red")

