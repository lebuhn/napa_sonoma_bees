#source('~/Documents/Molly Hayes/SFSU/Thesis/Pre fire/Napa bee r code/Vineyard_data_creation.R')
#nettingdata=read.csv('~/Documents/Molly Hayes/SFSU/Thesis/Pre fire/2002nettingdata.csv',header=TRUE)

source("~/Dropbox/napabees/data/Access_data/Vineyard_data_creation.R")

#nettingdata=subset(nettingdata.pan.net,technique=="netting") #Select netting data
nettingdata=subset(nettingdata,HostPlant!="Patrolling")
nettingdata=subset(nettingdata,HostPlant!="patrolling")
nettingdata=subset(nettingdata,HostPlant!="on ground")
nettingdata=subset(nettingdata,HostPlant!="random")

library(bipartite) #Load library for network analyses
library(vegan) #load library for calculating distance

SiteID.u=unique(nettingdata$SiteID) #Vector of SiteID names
#SiteID.u=unique("nettingdata$SiteID", "year") #Vector of SiteID names
napanets=list() #Empty list to store networks for each SiteID
napanets02=list() #Empty list to store networks for each SiteID
vegdist02=list() #Empty list to store vegdist output for each SiteID

split.screen(c(3,5)) #Define number of panels for plot

#For loop to create interaction matrices - Question: is this assuming 1 specimen per row? It doesn't seem to take into account the column "number" where some aren't 1. 
for (i in 1:length(SiteID.u)){
  nettingdata.s=nettingdata[which(nettingdata$SiteID==SiteID.u[i]),] #Subset of records for SiteID i
  napanets[[i]]=as.matrix(with(nettingdata.s,table(HostPlant,CombinedName))) #Store network for SiteID i
  screen(i) #Select plot panel
  (plotweb(napanets[[i]],method="normal",empty=FALSE,low.lablength=0,high.lablength=0,
           col.high="blue",col.low="green"))
  title(main=SiteID.u[i])
}

# Subset for only 2002
nettingdata02=subset(nettingdata,Year.x="2002")

# For loop structure
for (i in 1:length(SiteID.u)){
  nettingdata02.s=nettingdata02[which(nettingdata02$SiteID==SiteID.u[i]),] #Subset of records for SiteID i
  napanets02[[i]]=as.matrix(with(nettingdata02.s,table(HostPlant,CombinedName))) #Store network for SiteID i
  vegdist02[[i]]=vegdist(napanets02[[i]],method="bray",binary=FALSE, diag=FALSE, upper=FALSE,na.rm = FALSE)
  
}
