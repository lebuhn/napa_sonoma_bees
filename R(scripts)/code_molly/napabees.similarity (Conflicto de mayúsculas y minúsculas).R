library("reshape")
library("vegan")

#source("~/Dropbox/napabees/R/napabees.r")

source("~/Dropbox/napabees/data/Access_data/Vineyard_data_creation.R")

#nettingdata.pan.net=read.table("~/Dropbox/napabees/data/Vineyard_bee_data_7Apr16_modif.csv",header=TRUE,sep=",")
#nettingdata=subset(nettingdata.pan.net,technique=="netting") #Select netting data

nettingdata=subset(nettingdata,Host.plant!="Patrolling")
nettingdata=subset(nettingdata,Host.plant!="patrolling")
nettingdata=subset(nettingdata,Host.plant!="on ground")
nettingdata=subset(nettingdata,Host.plant!="random")
nettingdata=subset(nettingdata,Host.plant!="On road")
nettingdata=subset(nettingdata,year!="2001")

#Create stacked matrix of interaction data seperated by VineyardCoverage, year, site and sampling event
nettingdata.m = subset(nettingdata, select = c(VineyardCoverage, year, SiteName.x, event, Host.plant, CombinedName, Number))
yr.site.event.plant <- cast(nettingdata.m, VineyardCoverage + year + SiteName.x + event + Host.plant ~ CombinedName, value='Number', sum)
