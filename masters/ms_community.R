################################################################################
##  ms_community.R: Examining community responses to removals in Smith master's plots.
##
##  Author: Kimberly Komatsu
##  Date created: June 17, 2019
################################################################################

library(vegan)
library(tidyverse)

setwd('C:\\Users\\la pierrek\\Dropbox (Smithsonian)\\konza projects\\Removal Plots_2011\\Masters 2011 data')

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

###############################
###############################


###read in data
#species presence/absence data
sppPA <- read.csv('ms_species_PA_2011.csv')%>%
  rename(present=Present)%>%
  #filter out the dissertation plots
  filter(study!='Dis')%>%
  #filter out species that were not present
  filter(present!=0)

#treatment data
trt <- read.csv('trt_codes.csv')


#calculate richness
rich <- sppPA%>%
  group_by(year, study, plot)%>%
  summarise(richness=length(present))%>%
  ungroup()%>%
  #merge with trt
  right_join(trt)%>%
  filter(year!='NA')
###problems:
#we don't have data from plot 187
#lots of plots we do have data for don't have a trt designation (we drop a lot of plots, fungicide?)


#simple bar graph
ggplot(data=barGraphStats(data=rich, variable="richness", byFactorNames=c("remove")), aes(x=as.factor(remove), y=mean)) +
  geom_bar(stat='identity', fill='white', color='black') +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2) +
  xlab('Removal Treatment') + ylab('Richness')
#export 600x800

#model
summary(rich_model <- aov(richness~remove + Error(ws), data=rich))



#nmds
sppMatrix <- sppPA%>%
  right_join(trt)%>%
  mutate(genus_species=paste(genus, species, sep='_'), remove_ws=paste(remove, ws, sep='::'))%>%
  select(year, study, plot, genus_species, remove_ws, remove, ws, present)%>%
  filter(year!='NA')%>%
  spread(key=genus_species, value=present, fill=0)

dissimilarity <- metaMDS(sppMatrix[,7:57], distance='bray')

sites <- 1:nrow(sppMatrix)
#Make a new data table called BC_Meta_Data and use data from Wide_Relative_Cover columns 1-3
metadata <- sppMatrix[,1:6]
#make a plot using the dataframe dissimilarity and the column "points".  Make "remove" a factor
plot(dissimilarity$points,col=as.factor(metadata$remove_ws), cex=3, pch=20)
#make elipses using the dissimilarity.  Group by "remove" and use standard deviation to draw eclipses and display by sites, add labels based on removal type.
ordiellipse(dissimilarity, groups=as.factor(metadata$remove_ws), kind = "sd",display = "sites", label = T)
#export at 800x800


#permanova
sppMatrix2 <- sppMatrix[,7:57]
envMatrix <- sppMatrix[,1:6]
print(permanova <- adonis2(formula = sppMatrix2~remove*ws, data=envMatrix, permutations = 999, method = "bray"))











