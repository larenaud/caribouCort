# scripts for making all figures in ms 

library(kableExtra)
library(pander)
library(ggplot2)
library(hrbrthemes)
library(cowplot)


rm(list = ls())


load("Data/spatialCort.RData")



colnames(data)
data$site <- droplevels(data$site)


tmp <- data %>% 
  filter(site == "Portneuf")

# make prop 
prop <- tmp[,c(12:29)]*100 # exclude roads
tmp = t(t(colMeans(prop)))


data1 <- data.frame(tmp)

rownames(data1) <- c("Young clearcut","Old clearcut","Lichen woodland","Human","Mixed forest","Young disturbance","Old disturbance","Old conifer","No regeneration","Partial cutting",
                    "Power Line",   
                    "Regeneration", "Water","Wetlands","Young conifer", "Cabins","Major roads","Minor roads" )
data1$group <- rownames(data1)

colnames(data1) <-c("Value","Habitat")# Basic piechart


# take good df for real
a = data1 %>%
  filter(!is.na(Value)) %>% 
  arrange(Value) %>%
  tail(20) %>%
  mutate(Country=factor(Habitat, Habitat)) %>%
  ggplot(aes(x=Country, y=Value) ) +
  geom_segment(aes(x=Country ,xend=Country, y=0, yend=Value), color="grey") +
  geom_point(size=3, color="navyblue") +
  coord_flip() +
  theme_cowplot(12) +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("") +
  ylab("Mean proportion of habitat within \n\ home range (%) in Portneuf")






# Create Data 

tmp <- data %>% 
  filter(site == "Charlevoix")

# make prop 
prop <- tmp[,c(12:29)]*100 # exclude roads
tmp = t(t(colMeans(prop)))

data2 <- data.frame(tmp)

rownames(data2) <- c("Young clearcut","Old clearcut","Lichen woodland","Human","Mixed forest","Young disturbance","Old disturbance","Old conifer","No regeneration","Partial cutting",
                    "Power Line",   
                    "Regeneration", "Water","Wetlands","Young conifer", "Cabins","Major roads","Minor roads" )
data2$group <- rownames(data2)

colnames(data2) <-c("Value","Habitat")# Basic piechart


b= data2 %>%
  filter(!is.na(Value)) %>%
  arrange(Value) %>%
  tail(20) %>%
  mutate(Country=factor(Habitat, Habitat)) %>%
  ggplot( aes(x=Country, y=Value) ) +
  geom_segment( aes(x=Country ,xend=Country, y=0, yend=Value), color="grey") +
  geom_point(size=3, color="navyblue") +
  coord_flip() +
  theme_cowplot(12) +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("") +
  ylab("Mean proportion of habitat within \n\ home range (%) in Charlevoix")

cowplot::plot_grid(a,b, labels = c("A", "B"),
                   ncol=2)


p <- plot_grid (a,
                b, 
                labels = c("A", "B"), 
                align = "vh")
getwd()

cairo_pdf(filename = "Graphs/RenaudFig1HabitatProp.pdf", # the name you wanna give to your file
         width = 9.4, height = 7.2, pointsize = 10, 
         fallback_resolution = 300)

print(p)
dev.off()



# this is the OLD db
# cortisol <- read.delim2("~/Documents/LARenaud_Pro/Caribou/Data/Old/datacortisol_dv.txt", dec=",")
# head(cortisol)
# str(cortisol)
# 
# colnames(cortisol) <- c("site", "secteur","year","id","sex","plate", "concentration", "cv", "lnconc", "logconc", 
#                         "area","cabins", "cuts_0_5", "cuts_6_20", "part_cuts", "cuts", "open_woodlands", "old_mixed" ,
#                         "pertnat", "old_conifer", 
#                         "open_noregen", "regen_20_50", "wetlands", "young_con","roads_1_2","roads_3_4","bordures")
# str(cortisol) 
# summary(cortisol)
# 
# old <- na.omit(cortisol)
# new<-na.omit(data)
# colnames(new)
# 
# oldH = t(t(colSums(old[, c(12:26)])))
# newH = t(t(colSums(new[, c(11:28)])))
# 
# oldH
# newH