 
#source("~/Dropbox/napabees/R/napabees.r")

source("~/Dropbox/napabees/data/Access_data/Vineyard_data_creation.R")

#nettingdata.pan.net=read.table("~/Dropbox/napabees/dataVineyard_bee_data_7Apr16_modif.csv",header=TRUE,sep=",")
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

split.screen(c(3,5)) #Define number of panels for plot

#For loop to create interaction matrices
for (i in 1:length(SiteID.u)){
  nettingdata.s=nettingdata[which(nettingdata$SiteID==SiteID.u[i]),] #Subset of records for SiteID i
  napanets[[i]]=as.matrix(with(nettingdata.s,table(HostPlant,CombinedName))) #Store network for SiteID i
  screen(i) #Select plot panel
  plotweb(napanets[[i]],method="normal",empty=FALSE,low.lablength=0,high.lablength=0,
     col.high="blue",col.low="green")
  title(main=SiteID.u[i])
}
temp=array(napanets[1])
for (i in 2:length(SiteID.u)){
    matv =rbind(temp, array(napanets[i]))
                temp = matv}
dim(matv)
matv[1,1]
vegdist(matv, method="bray", binary=FALSE, diag=FALSE, upper=FALSE,
        na.rm = FALSE)  # calculates bray curtis distance measure