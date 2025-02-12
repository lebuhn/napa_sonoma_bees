 
#source("~/Dropbox/napabees/R/napabees.r")

source("~/Dropbox/napabees/data/Access_data/Vineyard_data_creation.R")

#nettingdata.pan.net=read.table("~/Dropbox/napabees/dataVineyard_bee_data_7Apr16_modif.csv",header=TRUE,sep=",")
#nettingdata=subset(nettingdata.pan.net,technique=="netting") #Select netting data

nettingdata=subset(nettingdata,Host.plant!="Patrolling")
nettingdata=subset(nettingdata,Host.plant!="patrolling")
nettingdata=subset(nettingdata,Host.plant!="on ground")
nettingdata=subset(nettingdata,Host.plant!="random")

library(bipartite) #Load library for network analyses


#SiteID.u=unique(nettingdata$SiteID, nettingdata$year, nettingdata$event) #Vector of SiteID names by year by event

SiteID.u=unique(nettingdata$SiteID) #Vector of SiteID names
year.u=unique(nettingdata$year) #Vector of year
event.u=unique(nettingdata$event) #Vector of events

napanets=list() #Empty list to store networks for each SiteID

#split.screen(c(3,5)) #Define number of panels for plot



#For loop to create interaction matrices
for (i in 1:length(SiteID.u)){
#     for (j in 1:length(year.u)){
#       for (k in 1:length(event.u)){
  nettingdata.s=nettingdata[which(nettingdata$SiteID==SiteID.u[i]),] #Subset of records for SiteID i
#   nettingdata.sy=nettingdata.s[which(nettingdata.s$year==year.u[j],)] #Subset of records for year j within SiteID i
#   nettingdata.sye=nettingdata.sy[which(nettingdata.sy$event==event.u[k],)] #Subset of records for event k within year j
 
napanets[[i]]=as.matrix(with(nettingdata.s,table(Host.plant,CombinedName))) #Store network for SiteID i
}

#napanets[[i,j,k]]=as.matrix(with(nettingdata.sye,table(Host.plant,CombinedName))) #Store network for SiteID i
}}}
