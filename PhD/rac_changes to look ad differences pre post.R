library(tidyverse)
library(vegan)
library(devtools)
install_github("NCEAS/codyn", ref = "sp_diff_test")
library(codyn)
theme_set(theme_bw(12))
setwd("C:\\Users\\megha\\Dropbox\\Konza Research\\Removal Plots_2011\\PhD_Removals")

##redo all based on presence/absence

###pre data
dat00<-read.csv("removal study_stems_biomass_00_by growth.csv")%>%
  select(yr, plot, spnum, may)%>%
  mutate(sp="sp")%>%
  mutate(spnum2=paste(sp, spnum, sep=""))%>%
  select(-spnum, -sp)%>%
  filter(may!=0)%>%
  mutate(present=1,
         year=2000)%>%
  select(-may, -yr)

trt<- read.csv("removal study_stems_biomass_00_by growth.csv")%>%
  select(plot, rlevel, density)%>%
  unique()

dat01<-read.csv("removal study_stems_biomass_01_by growth.csv")%>%
  select(yr, plot, spnum, aug)%>%
  mutate(sp="sp")%>%
  mutate(spnum2=paste(sp, spnum, sep=""))%>%
  select(-spnum, -sp)%>%
  filter(aug!=0)%>%
  mutate(present=1,
         year=2001)%>%
  select(-aug, -yr)

dat11<-read.csv("PhD_Biomass_Data_2011_v2.csv") %>%
  select(plot, spnum, bag, both)%>%
  mutate(sp="sp")%>%
  mutate(spnum2=paste(sp, spnum, sep=""))%>%
  select(-spnum, -sp)%>%
  filter(!is.na(both), both!=0)%>%
  mutate(present=1,
         year=2011)%>%
  group_by(year, spnum2, plot)%>%
  summarize(present=mean(present))

dat00_01<-dat00%>%
  bind_rows(dat01)

dat00_11<-dat00%>%
  bind_rows(dat11)

spdiff00_01<-RAC_change_beta(dat00_01, time.var="year", species.var = "spnum2", abundance.var = "present", replicate.var = "plot")

spdiff00_11<-RAC_change_beta(dat00_11, time.var="year", species.var = "spnum2", abundance.var = "present", replicate.var = "plot")

spdiff_trt<-spdiff00_01%>%
  bind_rows(spdiff00_11)%>%
  left_join(trt)%>%
  select(-evenness_change, -rank_change)%>%
  group_by(year, year2, rlevel)%>%
  summarize(drich=mean(richness_change), beta=mean(beta3),
            gain=mean(gains), losses=mean(losses))

ggplot(data=spdiff_trt, aes(x=as.factor(rlevel), y=beta, fill=as.factor(year2)))+
  geom_bar(stat="identity", position = position_dodge())

ggplot(data=spdiff_trt, aes(x=as.factor(rlevel), y=drich, fill=as.factor(year2)))+
  geom_bar(stat="identity", position = position_dodge())

ggplot(data=spdiff_trt, aes(x=as.factor(rlevel), y=gain, fill=as.factor(year2)))+
  geom_bar(stat="identity", position = position_dodge())

ggplot(data=spdiff_trt, aes(x=as.factor(rlevel), y=losses, fill=as.factor(year2)))+
  geom_bar(stat="identity", position = position_dodge())

###by density
spdiff_trt<-spdiff00_01%>%
  bind_rows(spdiff00_11)%>%
  left_join(trt)%>%
  select(-evenness_change, -rank_change)%>%
  group_by(year, year2, density)%>%
  summarize(drich=mean(richness_change), beta=mean(beta3),
            gain=mean(gains), losses=mean(losses))

ggplot(data=spdiff_trt, aes(x=as.factor(density), y=beta, fill=as.factor(year2)))+
  geom_bar(stat="identity", position = position_dodge())

ggplot(data=spdiff_trt, aes(x=as.factor(density), y=drich, fill=as.factor(year2)))+
  geom_bar(stat="identity", position = position_dodge())

ggplot(data=spdiff_trt, aes(x=as.factor(density), y=gain, fill=as.factor(year2)))+
  geom_bar(stat="identity", position = position_dodge())

ggplot(data=spdiff_trt, aes(x=as.factor(density), y=losses, fill=as.factor(year2)))+
  geom_bar(stat="identity", position = position_dodge())
