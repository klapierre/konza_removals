library(grid)
library(vegan)
library(codyn)
library(PerformanceAnalytics)
library(tidyverse)


## Sally's desktop
setwd("~/Dropbox/Removal Plots_2011/PhD_Removals")

## Kim's laptop
setwd("C:\\Users\\lapie\\Dropbox (Smithsonian)\\konza projects\\Removal Plots_2011\\PhD_Removals")

#kim's desktop
setwd("C:\\Users\\la pierrek\\Dropbox (Smithsonian)\\konza projects\\Removal Plots_2011\\PhD_Removals")


###bar graph summary statistics function
#barGraphStats(data=, variable="", byFactorNames=c(""))

barGraphStats <- function(data, variable, byFactorNames) {
  count <- length(byFactorNames)
  N <- aggregate(data[[variable]], data[byFactorNames], FUN=length)
  names(N)[1:count] <- byFactorNames
  names(N) <- sub("^x$", "N", names(N))
  mean <- aggregate(data[[variable]], data[byFactorNames], FUN=mean)
  names(mean)[1:count] <- byFactorNames
  names(mean) <- sub("^x$", "mean", names(mean))
  sd <- aggregate(data[[variable]], data[byFactorNames], FUN=sd)
  names(sd)[1:count] <- byFactorNames
  names(sd) <- sub("^x$", "sd", names(sd))
  preSummaryStats <- merge(N, mean, by=byFactorNames)
  finalSummaryStats <- merge(preSummaryStats, sd, by=byFactorNames)
  finalSummaryStats$se <- finalSummaryStats$sd / sqrt(finalSummaryStats$N)
  return(finalSummaryStats)
}  

theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=20, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=16),
             axis.title.y=element_text(size=20, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=16),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=20))


###########################################################
###########################################################


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
#generate species list for 2000 using stem density data
sppList00 <- biomass00%>%
  select(plot, spnum, may, aug)%>%
  filter(may>0)%>%
  rename(stem_pretrt=may, stem_00=aug)

#generate species list for 2001 using biomass data
sppList01 <- biomass01%>%
  select(plot, spnum, allbiom)%>%
  filter(allbiom>0)%>%
  rename(biomass01=allbiom)

#generate species list for 2011 using biomass data
sppList11 <- biomass11%>%
  mutate(biomass11=both-bag)%>%
  filter(!is.na(biomass11))%>%
  mutate(spnum=ifelse(genus=='unknown forb 1', 501, ifelse(genus=='coronilla', 502, spnum)))%>%
  mutate(yr=10)%>%
  select(plot, spnum, biomass11)

#merge species lists from 2001 and 2011 (post-treatment data)
sppListAll <- sppList01%>%
  full_join(sppList11)%>%
  left_join(trts)%>%
  #drop the true unmanipulated controls, because we don't have all the data for them
  filter(plot<78)

#merge species list from 2000 (pre-treatment data)
sppListGained <- sppListAll%>%
  full_join(sppList00)%>%
  #only keep species that were not present in 2000 post-treatment, but are present in 2011 (i.e., were gained since removal)
  filter(is.na(stem_00)|stem_00==0)%>%
  #drop the true unmanipulated controls, because we don't have all the data for them
  filter(plot<78)%>%
  filter(!is.na(biomass11))%>% #70.3% of the species are new (not present in 2000)
  left_join(knzSpList)

#get counts of number of species gained, and of those how many were new vs previously present
sppCountNew <- sppListGained%>%
  #only keep species that were not present in pre-treatnent (i.e., new to the plots)
  filter(is.na(stem_pretrt))%>%
  group_by(plot, density, rlevel, S)%>%
  summarise(num_new=length(biomass11))%>%
  ungroup()

sppCountGained <- sppListGained%>%
  group_by(plot, density, rlevel, S)%>%
  summarise(num_gained=length(biomass11))%>%
  ungroup()%>%
  #join on the counts of new species
  left_join(sppCountNew)%>%
  #calculate number of species that were gained, but not new to the plots (i.e., those that were present pre-trt)
  mutate(num_old=num_gained-num_new)

#histogram of number of new species per plot
hist(sppCountGained$num_new)

###figures of species gained and whether they are new or old
figNewSpp <- ggplot(data=barGraphStats(data=sppCountGained, variable="num_new", byFactorNames=c("density","rlevel")), aes(x=rlevel, y=mean, fill=density)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(0.9), width=0.2) +
  ylab('Number New Species') + ylim(0,11) +
  theme(legend.position='none') +
  scale_fill_manual(name='Density\nReduction',
                    breaks=c('A','B','C'),
                    values=c('#999999', '#E69F00', '#56B4E9'),
                    labels=c('50%', '25%', '0%')) +
  scale_x_discrete(name='Richness Treatment',
                   breaks=c('A','B','C', 'D'),
                   labels=c('4-6 spp', '7-9 spp', '10-12 spp', '13-16 spp'))

figOldSpp <- ggplot(data=barGraphStats(data=sppCountGained, variable="num_old", byFactorNames=c("density","rlevel")), aes(x=rlevel, y=mean, fill=density)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(0.9), width=0.2) +
  ylab('Number Old Species') + ylim(0,11) +
  theme(legend.title=element_text(size=24), legend.position=c(0.2,0.8)) +
  scale_fill_manual(name='Density\nReduction',
                    breaks=c('A','B','C'),
                    values=c('#999999', '#E69F00', '#56B4E9'),
                    labels=c('50%', '25%', '0%')) +
  scale_x_discrete(name='Richness Treatment',
                   breaks=c('A','B','C', 'D'),
                   labels=c('4-6 spp', '7-9 spp', '10-12 spp', '13-16 spp'))

figGainedSpp <- ggplot(data=barGraphStats(data=sppCountGained, variable="num_gained", byFactorNames=c("density","rlevel")), aes(x=rlevel, y=mean, fill=density)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(0.9), width=0.2) +
  ylab('Number Gained Species') + ylim(0,11) +
  theme(legend.position='none') +
  scale_fill_manual(name='Density\nReduction',
                    breaks=c('A','B','C'),
                    values=c('#999999', '#E69F00', '#56B4E9'),
                    labels=c('50%', '25%', '0%')) +
  scale_x_discrete(name='Richness Treatment',
                   breaks=c('A','B','C', 'D'),
                   labels=c('4-6 spp', '7-9 spp', '10-12 spp', '13-16 spp'))

pushViewport(viewport(layout=grid.layout(1,3)))
print(figOldSpp, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(figNewSpp, vp=viewport(layout.pos.row=1, layout.pos.col=2))
print(figGainedSpp, vp=viewport(layout.pos.row=1, layout.pos.col=3))
#export at 2400 x 600































