library(tidyverse)
library(vegan)
library(codyn)
library(PerformanceAnalytics)


## Sally's desktop
setwd("~/Dropbox/Removal Plots_2011/PhD_Removals")

## Kim's laptop
setwd("C:\\Users\\lapie\\Dropbox (Smithsonian)\\konza projects\\Removal Plots_2011\\PhD_Removals")


#species list
knzSpList <- read.csv('konza_spplist.csv')


###reading in cleaning 2000 data
biomass00<-read.csv("removal study_stems_biomass_00_by growth.csv")
biomass01<-read.csv("removal study_stems_biomass_01_by growth.csv")
biomass11<-read.csv("PhD_Biomass_Data_2011_v2.csv")
trts<-read.csv("PhD_removal plot_treatments.csv")

###2001 - pull out andro 2, sorg 18, bout 4, other grass (type=2+3), forb (type=4+5)
andro<-biomass01%>%
  select(density, rlevel, plot, spnum, allbiom)%>%
  filter(spnum==2)%>%
  select(-spnum)%>%
  rename(andro=allbiom)

sorg<-biomass01%>%
  select(density, rlevel, plot, spnum, allbiom)%>%
  filter(spnum==18)%>%
  select(-spnum)%>%
  rename(sorg=allbiom)

bout<-biomass01%>%
  select(density, rlevel, plot, spnum, allbiom)%>%
  filter(spnum==4)%>%
  select(-spnum)%>%
  rename(bout=allbiom)

othergrass<-biomass01%>%
  select(density, rlevel, plot, spnum, allbiom, type)%>%
  filter(type==2|type==3)%>%
  group_by(density, rlevel, plot) %>% 
  summarise(allbiom=sum(allbiom)) %>% 
  rename(othergrass=allbiom)

forbs<-biomass01%>%
  select(density, rlevel, plot, spnum, allbiom, type)%>%
  filter(type==4|type==5)%>%
  group_by(density, rlevel, plot) %>% 
  summarise(allbiom=sum(allbiom)) %>% 
  rename(forbs=allbiom)

AllBio_01<-andro%>%
  left_join(sorg) %>% 
  left_join(bout) %>% 
  left_join(othergrass) %>% 
  left_join(forbs) 

total<-AllBio_01 %>% 
  gather(type, biomass,andro:forbs)%>%
  group_by(density, rlevel, plot) %>% 
  summarise(total=sum(biomass))

AllBio_01_2<-AllBio_01 %>% 
  left_join(total)

All_01_MeanxDensity<- AllBio_01_2 %>% 
  filter(total!=0) %>% 
  gather(type, biomass,andro:total)%>%
  group_by(density, type) %>% 
  summarise(mean_bio=mean(biomass), sd_bio=sd(biomass))%>%
  mutate(se_bio=sd_bio/sqrt(10))%>%
  filter(density!=4)

ggplot(data=All_01_MeanxDensity, aes(x=type, y=mean_bio, fill=as.factor(density)))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=mean_bio-se_bio, ymax=mean_bio+se_bio), position = position_dodge(0.9), width=0.2)+
  scale_fill_manual(name="Density", values=c("green4", "darkred", "blue"))+
  xlab("")+
  ylab("2001 Biomass (g)")



###2011 - pull out andro 2, sorg 18, bout 4, other grass (type=2+3), forb (type=4+5)
biomass11_v2<-biomass11 %>% 
  filter(both!="NA") %>% 
  mutate(allbiom=both-bag) %>% 
  select(plot, spnum, species, genus, allbiom)


splist_11<-biomass11 %>% 
  select(spnum, genus, species)%>%
  unique()
#write.csv(splist_11, "splist_2011.csv")
unique(biomass11_v2$spnum)

splist_11<-read.csv("splist_2011_v2.csv")

biomass11_v3<-biomass11_v2 %>% 
  right_join(splist_11)%>%
  filter(allbiom!="NA")


andro<-biomass11_v3%>%
  filter(spnum==2)%>%
  select(-spnum, -species, -genus, -type)%>%
  rename(andro=allbiom)

sorg<-biomass11_v3%>%
  filter(spnum==18)%>%
  select(-spnum, -species, -genus, -type)%>%
  rename(sorg=allbiom)

bout<-biomass11_v3%>%
  filter(spnum==4)%>%
  select(-spnum, -species, -genus, -type)%>%
  rename(bout=allbiom)
  
  

othergrass<-biomass11_v3%>%
  filter(type==2)%>%
  group_by(plot) %>% 
  summarise(allbiom=sum(allbiom)) %>% 
  rename(othergrass=allbiom)

forbs<-biomass11_v3%>%
   filter(type==3)%>%
  group_by(plot) %>% 
  summarise(allbiom=sum(allbiom))%>% 
  rename(forbs=allbiom)

AllBio_11<-andro%>%
  left_join(sorg) %>% 
  left_join(bout) %>% 
  left_join(othergrass) %>% 
  left_join(forbs) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
#str(AllBio_11)

total<-AllBio_11 %>% 
  gather(type, biomass,andro:forbs)%>%
  group_by(plot) %>% 
  summarise(total=sum(biomass))

AllBio_11_2<-AllBio_11 %>% 
  left_join(total)%>%
  left_join(trts)

All_11_MeanxDensity<- AllBio_11_2 %>% 
  select(-S) %>% 
  group_by(density, rlevel, plot) %>% 
  gather(type, biomass,andro:total)%>%
  group_by(density, type) %>% 
  summarise(mean_bio=mean(biomass), sd_bio=sd(biomass))%>%
  mutate(se_bio=sd_bio/sqrt(10)) %>% 
  filter(density!='NA')

ggplot(data=All_11_MeanxDensity, aes(x=type, y=mean_bio, fill=as.factor(density)))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=mean_bio-se_bio, ymax=mean_bio+se_bio), position = position_dodge(0.9), width=0.2)+
  scale_fill_manual(name="Density", values=c("green4", "darkred", "blue"))+
  xlab("")+
  ylab("2011 Biomass (g)")




###comparing species lists
sppList00 <- biomass00%>%
  select(plot, spnum, allbiom)%>%
  filter(allbiom>0)%>%
  rename(biomass00=allbiom)

sppList01 <- biomass01%>%
  select(plot, spnum, allbiom)%>%
  filter(allbiom>0)%>%
  rename(biomass01=allbiom)

sppList11 <- biomass11%>%
  mutate(biomass11=both-bag)%>%
  filter(!is.na(biomass11))%>%
  mutate(spnum=ifelse(genus=='unknown forb 1', 501, ifelse(genus=='coronilla', 502, spnum)))%>%
  mutate(yr=10)%>%
  select(plot, spnum, biomass11)

sppListAll <- sppList01%>%
  full_join(sppList11)%>%
  left_join(trts)%>%
  filter(plot<78)
         
sppListGained <- sppListAll%>%
  filter(is.na(biomass01))%>%
  full_join(sppList00)%>%
  filter(plot<78)%>%
  filter(!is.na(biomass11))%>% #90.2% of the species are new (not present in 2000)!
  left_join(knzSpList)




