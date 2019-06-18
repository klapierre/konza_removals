library(tidyverse)
library(vegan)
theme_set(theme_bw(12))
setwd("C:\\Users\\megha\\Dropbox\\Konza Research\\Removal Plots_2011\\PhD_Removals")

##redo all based on presence/absence

###pre data
stems00<-read.csv("removal study_stems_biomass_00_by growth.csv")%>%
  select(yr, density, rlevel, plot, spnum, may)%>%
  mutate(sp="sp")%>%
  mutate(spnum2=paste(sp, spnum, sep=""))%>%
  select(-spnum, -sp)%>%
  filter(may!=0)

tot00<-stems00%>%
  group_by(plot)%>%
  summarize(tot=sum(may))

stems00.2<-stems00%>%
  left_join(tot00)%>%
  mutate(relabund=may/tot)%>%
  filter(plot<78)%>%
  select(-may, -tot)%>%
  spread(spnum2, relabund, fill=0)


#NMDS  
plots<-stems00.2[,1:4]
mds<-metaMDS(stems00.2[,5:50], autotransform=FALSE, shrink=FALSE, trymax=500)
mds

scores <- data.frame(scores(mds, display="sites"))
scores2<- cbind(plots, scores)%>%
  group_by(density, rlevel)%>%
  summarize(mNMDS1=mean(NMDS1), mNMDS2=mean(NMDS2), sd1=sd(NMDS1), sd2=mean(NMDS2))%>%
  mutate(se1=sd1/sqrt(6), se2=sd2/sqrt(6))

##plotting this
ggplot(scores2, aes(x=mNMDS1, y=mNMDS2, color=as.factor(rlevel), shape=as.factor(density)))+
  geom_point(size=5)+
  geom_errorbar(aes(ymin=mNMDS2-se2, ymax=mNMDS2+se2))+
  geom_errorbarh(aes(xmin=mNMDS1-se1, xmax=mNMDS1+se1))

###2001 data stems

stems01<-read.csv("removal study_stems_biomass_01_by growth.csv")%>%
  select(yr, density, rlevel, plot, spnum, aug)%>%
  mutate(sp="sp")%>%
  mutate(spnum2=paste(sp, spnum, sep=""))%>%
  select(-spnum, -sp)%>%
  filter(aug!=0)

tot01<-stems01%>%
  group_by(plot)%>%
  summarize(tot=sum(aug))

stems01.2<-stems01%>%
  left_join(tot01)%>%
  mutate(relabund=aug/tot)%>%
  filter(plot<78)%>%
  select(-aug, -tot)%>%
  spread(spnum2, relabund, fill=0)

#NMDS  
plots<-stems00.2[,1:4]
mds<-metaMDS(stems01.2[,5:39], autotransform=FALSE, shrink=FALSE, trymax=500)
mds

scores <- data.frame(scores(mds, display="sites"))
scores2<- cbind(plots, scores)%>%
  group_by(density, rlevel)%>%
  summarize(mNMDS1=mean(NMDS1), mNMDS2=mean(NMDS2), sd1=sd(NMDS1), sd2=mean(NMDS2))%>%
  mutate(se1=sd1/sqrt(6), se2=sd2/sqrt(6))

##plotting this
ggplot(scores2, aes(x=mNMDS1, y=mNMDS2, color=as.factor(rlevel), shape=as.factor(density)))+
  geom_point(size=5)+
  geom_errorbar(aes(ymin=mNMDS2-se2, ymax=mNMDS2+se2))+
  geom_errorbarh(aes(xmin=mNMDS1-se1, xmax=mNMDS1+se1))


###2001 biomass
bio01<-read.csv("removal study_stems_biomass_01_by growth.csv")%>%
  select(yr, density, rlevel, plot, spnum, allbiom)%>%
  mutate(sp="sp")%>%
  mutate(spnum2=paste(sp, spnum, sep=""))%>%
  select(-spnum, -sp)%>%
  filter(allbiom!=0)

biotot01<-bio01%>%
  group_by(plot)%>%
  summarize(tot=sum(allbiom))

bio01.2<-bio01%>%
  left_join(biotot01)%>%
  mutate(relabund=allbiom/tot)%>%
  filter(plot<78)%>%
  select(-allbiom, -tot)%>%
  spread(spnum2, relabund, fill=0)

#NMDS  
plots<-bio01.2[,1:4]
mds<-metaMDS(bio01.2[,5:39], autotransform=FALSE, shrink=FALSE, trymax=500)
mds

scores <- data.frame(scores(mds, display="sites"))
scores2<- cbind(plots, scores)%>%
  group_by(density, rlevel)%>%
  summarize(mNMDS1=mean(NMDS1), mNMDS2=mean(NMDS2), sd1=sd(NMDS1), sd2=mean(NMDS2))%>%
  mutate(se1=sd1/sqrt(6), se2=sd2/sqrt(6))

##plotting this
ggplot(scores2, aes(x=mNMDS1, y=mNMDS2, color=as.factor(rlevel), shape=as.factor(density)))+
  geom_point(size=5)+
  geom_errorbar(aes(ymin=mNMDS2-se2, ymax=mNMDS2+se2))+
  geom_errorbarh(aes(xmin=mNMDS1-se1, xmax=mNMDS1+se1))

#biomass and stems differ views. Richness is more important in defining communities by biomass

###2011 biomass
trt<-bio01%>%
  select(plot, rlevel, density)%>%
  unique()

bio11<-read.csv("PhD_Biomass_Data_2011_v2.csv")%>%
  select(plot, spnum, bag, both)%>%
  filter(!is.na(both))%>%
  mutate(allbiom=both-bag)%>%
  mutate(sp="sp")%>%
  mutate(spnum2=paste(sp, spnum, sep=""))%>%
  select(-spnum, -sp, -bag, -both)%>%
  filter(allbiom!=0)

biotot11<-bio11%>%
  group_by(plot)%>%
  summarize(tot=sum(allbiom))

bio11.2<-bio11%>%
  left_join(biotot11)%>%
  mutate(relabund=allbiom/tot)%>%
  filter(plot<78)%>%
  select(-allbiom, -tot)%>%
  group_by(plot, spnum2)%>%
  summarize(relabund=sum(relabund))%>%
  spread(spnum2, relabund, fill=0)
bio11.3<-trt%>%
  left_join(bio11.2)%>%
  na.omit

#NMDS  
plots<-bio11.3[,1:3]
mds<-metaMDS(bio11.3[,4:48], autotransform=FALSE, shrink=FALSE, trymax=500)
mds

scores <- data.frame(scores(mds, display="sites"))
scores2<- cbind(plots, scores)%>%
  group_by(density, rlevel)%>%
  summarize(mNMDS1=mean(NMDS1), mNMDS2=mean(NMDS2), sd1=sd(NMDS1), sd2=mean(NMDS2))%>%
  mutate(se1=sd1/sqrt(6), se2=sd2/sqrt(6))

##plotting this
ggplot(scores2, aes(x=mNMDS1, y=mNMDS2, color=as.factor(rlevel), shape=as.factor(density)))+
  geom_point(size=5)+
  geom_errorbar(aes(ymin=mNMDS2-se2, ymax=mNMDS2+se2))+
  geom_errorbarh(aes(xmin=mNMDS1-se1, xmax=mNMDS1+se1))
