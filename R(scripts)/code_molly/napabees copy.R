library(bipartite) #Load library for network analyses
library(gdata)
library("reshape2")
library("vegan")

#source("~/Dropbox/napabees/R/napabees.r")

source("~/Dropbox/napabees/data/Access_data/Vineyard_data_creation.R")

#nettingdata.pan.net=read.table("~/Dropbox/napabees/dataVineyard_bee_data_7Apr16_modif.csv",header=TRUE,sep=",")
#nettingdata=subset(nettingdata.pan.net,technique=="netting") #Select netting data
nettingdata=subset(nettingdata,Host.plant.nosp!="Patrolling")
nettingdata=subset(nettingdata,Host.plant.nosp!="patrolling")
nettingdata=subset(nettingdata,Host.plant.nosp!="on ground")
nettingdata=subset(nettingdata,Host.plant.nosp!="random")
nettingdata=subset(nettingdata,year!=2001)
nettingdata$SiteID=factor(nettingdata$SiteID)
nettingdata$CombinedName=factor(nettingdata$CombinedName)
nettingdata$Host.plant.nosp=factor(nettingdata$Host.plant.nosp)
nettingdata$Host.plant=factor(nettingdata$Host.plant)
nettingdata$eventmonth=as.integer(nettingdata$eventmonth)
nettingdata$event.100=as.factor(as.numeric(nettingdata$event)/100)

#Create vectors of sites, year and events
SiteID.u=unique(nettingdata$SiteID) #Vector of SiteID names
year.u=unique(nettingdata$year) #Vector of year
event.u=unique(nettingdata$event) #Vector of events
m = 1 #creates index for napanets

#SiteID.u=unique("nettingdata$SiteID", "year") #Vector of SiteID names
napanets=list() #Empty list to store networks for each SiteID
napanetsv=list()
napanetsm1=list()

split.screen(c(3,5)) #Define number of panels for plot
# 
 #==========================================
 #For loop to create interaction matrices
 #==========================================
 
 for (i in 1:length(SiteID.u)){
   for( j in 1:length(year.u)){
       for (k in 1:length(event.u)){
   
   nettingdata.s=nettingdata[which(nettingdata$SiteID==SiteID.u[i]),] #Subset of records for SiteID i
   nettingdata.sy=nettingdata.s[which(nettingdata.s$year==year.u[j]),] #Subset of records for year j within SiteID i
   nettingdata.sye=nettingdata.sy[which(nettingdata.sy$event==event.u[k]),] #Subset of records for event k within year j
   
   napanets[[m]]=as.matrix(with(nettingdata.sye,table(Host.plant.nosp,CombinedName)))#Store network for SiteID i
 
   
  m = m+1
     screen(i) #Select plot panel
   plotweb(napanets[[i]],method="normal",empty=FALSE,low.lablength=0,high.lablength=0,
      col.high="blue",col.low="green")
   title(main=SiteID.u[i])
 }}}
 
 n=1
 napanetsv=matrix(0,length(napanets),length(napanets[[1]]))
 for (n in 1:length(napanets)){
   #napanetsv[[n]]=unmatrix(napanets[[n]]) #convert matrix to vector
   napanetsv[n,]=unmatrix(napanets[[n]]) #convert matrix to vector
   n=n+1
 }
 sum(napanetsv[4] != 0) #look at number of zeros
 
 braynapa=vegdist(napanetsv) # Calculate Bray-Curtis values
 braynapam=as.matrix(braynapa)
 #write.csv(braynapam, file = "braynapamatrix.csv")
 dim(braynapam)
 #pull out values below diagonal.  This works even though there is a subscript error. Matrix is currently named x.
 
 bcvalues = list() 
 i=1
 for (i in 1:nrow(braynapam)){
   bcvalues[i] = braynapam[i+1, i]
 }
 head(bcvalues)
 
 write.csv(bcvalues, file ="bcvalues.csv")

#==========================================
#BEE AND PLANT SPECIES ANALYSIS
#==========================================

#Create stacked matrix of bee by plant data seperated by VineyardCoverage, year, site and sampling event
nettingdata.m = subset(nettingdata, select = c(VineyardCoverage, year, SiteName.x, event.100, Host.plant.nosp, CombinedName, Number))

yr.site.event.plant.nosp <- acast(nettingdata.m, VineyardCoverage + year + SiteName.x  + event.100 + Host.plant.nosp  ~ CombinedName, value.var='Number', sum)

# #calculate abundance
# plantabundance=rowSums(yr.site.event.plant.nosp[,c(2:147)]) 
# plantnames = array(0,nrow(yr.site.event.plant.nosp))
# plantnames = rownames(yr.site.event.plant.nosp)
# sitesplant = cbind( abundance=plantabundance[1:604])

#==========================================
# look at summary for each INTERACTION for each sampling event.  Uses "bp" to indicate bee and plant
#==========================================

yr.site.event.bp <- acast(nettingdata.m, VineyardCoverage + year + SiteName.x + event.100  ~ Host.plant.nosp + CombinedName, value.var='Number', sum, drop = TRUE)
#View(yr.site.event.bp)
yr.site.event.bp.m = subset(yr.site.event.bp, select = c(2:110))

#calculate bray curtis for INTERACTION
brayinteraction.bp = vegdist(yr.site.event.bp)
brayinterm.bp=as.matrix(brayinteraction.bp)


write.csv(brayinterm.bp, file = "brayinteraction.csv")
dim(brayinterm.bp)

#pull out values below diagonal.  This works even though there is a subscript error. Matrix is currently named x.
bcintervalues.bp = array(0,nrow(brayinterm.bp)) 
bcspnames.bp = array(0,nrow(brayinterm.bp))
bcspnames.bp = rownames(brayinterm.bp)
i=1
for (i in 1:nrow(brayinterm.bp)){
  bcintervalues.bp[i] = brayinterm.bp[i+1, i]
}

int.braycomparison = cbind(eventA = bcspnames.bp[1:111], eventB = bcspnames.bp[2:112], bc.interaction=bcintervalues.bp[1:111])




#==========================================
# look at summary for each BEE species for each sampling event.  Bee files use the word "bee"
#==========================================
nettingdata.m1 = subset(nettingdata, select = c(VineyardCoverage, year, SiteName.x, event.100, CombinedName, Number))
yr.site.event.b <- acast(nettingdata.m1, VineyardCoverage + year + SiteName.x + event.100  ~ CombinedName, value.var='Number', sum)
#View(yr.site.event.b)
yr.site.event.m = subset(yr.site.event.b, select = c(2:146))


#calculate bray curtis for BEE SPECIES only
braybee = vegdist(yr.site.event.m)
braybee.m=as.matrix(braybee)

#write.csv(braybeem, file = "braybee.csv")
dim(braybee.m)
#pull out values below diagonal.  This works even though there is a subscript error. Matrix is currently named x.

bcbeevalues = list() 
bcspnames.b = array(0,nrow(braybee.m))
bcspnames.b = rownames(braybee.m)
i=1
for (i in 1:nrow(braybee.m)){
  bcbeevalues[i] = braybee.m[i+1, i]
}
head(bcspvalues)

bee.braycomparison = cbind(eventA = bcspnames.b[1:114], eventB = bcspnames.b[2:115], bc.bees=bcbeevalues[1:114])
write.csv(bee.braycomparison, file ="bcvalues.csv")

#==========================================
# look at summary by PLANT for each sampling event
#==========================================
nettingdata.mp = subset(nettingdata, select = c(VineyardCoverage, year, SiteID, event.100, Host.plant.nosp, Number))
yr.site.event.plant.nosp <- acast(nettingdata.mp, VineyardCoverage + year + SiteID + event.100  ~ Host.plant.nosp, value.var='Number', sum)
#View(yr.site.event.plant.nosp)
yr.site.event.plant.nosp.m = subset(yr.site.event.plant.nosp, select = c(3:91))  #this has a weird extra column inserted so, we start with 3 not 2


#calculate bray curtis for PLANT SPECIES only
brayplant = vegdist(yr.site.event.plant.nosp.m)
brayplant.m=as.matrix(brayplant)

#write.csv(brayplantm, file = "brayplant.csv")
dim(brayplant.m)
#pull out values below diagonal.  This works even though there is a subscript error. Matrix is currently named x.

bcplantvalues = list() 
bcspnames.b = array(0,nrow(brayplant.m))
bcspnames.b = rownames(brayplant.m)
i=1
for (i in 1:nrow(brayplant.m)){
  bcplantvalues[i] = brayplant.m[i+1, i]
}
#head(bcspvalues)
#plant.braycomparison = cbind(eventA = bcspnames.b[1:114], eventB = bcspnames.b[2:115], bc.plant=bcplantvalues[1:114])
plant.braycomparison = cbind(eventA = bcspnames.b[1:(nrow(brayplant.m)-1)], eventB = bcspnames.b[2:nrow(brayplant.m)], bc.plant=bcplantvalues[1:nrow(brayplant.m)])
write.csv(plant.braycomparison, file ="bcvalues.csv")

#============
#Create summary table
#============
bc.values = cbind(eventA = bcspnames.bp[1:111], eventB = bcspnames.bp[2:112], bc.interaction=bcintervalues.bp[1:111])

