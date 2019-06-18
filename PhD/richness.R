library(tidyverse)
library(vegan)
library(codyn)
theme_set(theme_bw(12))
setwd("C:\\Users\\megha\\Dropbox\\Konza Research\\Removal Plots_2011\\PhD_Removals")

dat01<-read.csv("removal study_stems_biomass_01_by growth.csv")

trt<-dat01%>%
  select(plot, rlevel, density)%>%
  unique()

dat11<-read.csv("PhD_Biomass_Data_2011_v2.csv") %>%
  select(plot, spnum, bag, both)%>%
  filter(!is.na(both), both!=0)

rich01<-community_structure(dat01, abundance.var="aug", replicate.var = "plot")%>%
  left_join(trt)%>%
  filter(plot<78)%>%
  group_by(rlevel)%>%
  summarise(rich=mean(richness),
            sd=sd(richness))%>%
  mutate(se=sd/sqrt(18), year=2001)

rich11<-community_structure(dat11, abundance.var="both", replicate.var = "plot") %>% 
  left_join(trt)%>%
  filter(plot<78)%>%
  group_by(rlevel)%>%
  summarise(rich=mean(richness),
         sd=sd(richness))%>%
  mutate(se=sd/sqrt(18), year=2011)%>%
  bind_rows(rich01)

ggplot(data=rich11, aes(x=as.factor(rlevel), y=rich, fill=as.factor(year)))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_errorbar(aes(ymin=rich-se, ymax=rich+se), position = position_dodge(0.9), width=0.2)




