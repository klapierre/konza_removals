library(tidyverse)
theme_set(theme_bw(12))
setwd("C:\\Users\\megha\\Dropbox\\Konza Research\\Removal Plots_2011\\Masters 2011 data")

biomass<-read.csv("MSBiomassData_2011.csv")
trts<-read.csv("trt_codes.csv")

biomass2<-biomass%>%
  select(-X)%>%
  mutate(biomass=Bag.Weight...Biomass-Bag.Weight)%>%
  rename(plot=Plot) %>% 
  select(plot, Species, biomass)

total<-biomass2%>%
  group_by(plot)%>%
  summarize(total=sum(biomass))%>%
  left_join(trts)
  
ave<-biomass2%>%
  spread(Species, biomass)%>%
  left_join(total)%>%
  gather(type, biomass,Andropogon_gerardii:total)%>%
  group_by(remove, type) %>% 
  summarise(mean_bio=mean(biomass), sd_bio=sd(biomass))%>%
  mutate(se_bio=sd_bio/sqrt(10))%>%
  filter(type!="Wood")


ggplot(data=ave, aes(x=type, y=mean_bio, fill=as.factor(remove)))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=mean_bio-se_bio, ymax=mean_bio+se_bio), position = position_dodge(0.9), width=0.2)+
  scale_fill_manual(name="Remove", values=c("green4", "darkred"))+
  xlab("")+
  ylab("Biomass (g)")


summary(aov(total~remove, data=total))#no sig diff

biomass3<-biomass2%>%
  left_join(trts)%>%
  filter(Species!="Wood")

summary(aov(biomass~Species*remove + Error(ws), data=biomass3))#interaction between species and removal

t.test(biomass~remove, data=subset(biomass3, Species=="Andropogon_gerardii"))##sig diff
t.test(biomass~remove, data=subset(biomass3, Species=="Forb"))##not diff
t.test(biomass~remove, data=subset(biomass3, Species=="Other_grass"))##not diff
t.test(biomass~remove, data=subset(biomass3, Species=="Sorgastrum_nutans"))##not diff
