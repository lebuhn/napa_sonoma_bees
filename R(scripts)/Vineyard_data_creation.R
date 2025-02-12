#===========
#Code for importing and merging LeBuhn lab bee data from exported access files
#==============

datadir="~/GitHub/napa_sonoma_bees/data/raw/Access_data/"

genus = read.csv(paste(datadir,"BeeNames1_Genus.csv",sep=""), header = TRUE)
species = read.csv(paste(datadir,"BeeNames2_Species.csv",sep=""), header = TRUE)
collection = read.csv(paste(datadir,"COLLECTION.csv",sep=""), header = TRUE)
conditions = read.csv(paste(datadir,"CONDITIONS.csv",sep=""), header = TRUE)
flowers = read.csv(paste(datadir,"Flower_Estimation.csv",sep=""), header = TRUE)
samples = read.csv(paste(datadir,"SAMPLE_EVENTS_FOR_VINEYARDS.csv",sep=""), header = TRUE)
specimens = read.csv(paste(datadir,"tblSPECIMENS.csv",sep=""), header = TRUE)
sites = read.csv(paste(datadir,"SITES.csv",sep=""), header = TRUE)
events = read.csv(paste(datadir,"napa_sample_events.csv", sep =""), header = TRUE)


#======Create merged habitat data file, each date/time/site/technique is a unique collection event
collectionevent <- merge(conditions, sites, by = "SiteID")
collectionevent <- merge(collection, collectionevent, by = "Conditionsid")

# create unique event code in collection event and events
events$eventcode <- with(events, interaction(SiteName, ddate, sep = "", drop = TRUE))
collectionevent$eventcode <- with(collectionevent, interaction(SiteName, date, sep = "", drop = TRUE))
#names(collectionevent)
collectionevent <-merge(collectionevent, events, by = "eventcode")
collectionevent <- merge(samples, collectionevent, by = "Conditionsid")
#str(collectionevent)

# exclude variables 
xcldvars <- names(collectionevent) %in% c( "SiteName.y", "date.y", "Field9", "Field10","FLAG", "Directions", "ContactInfo", "GateCombination", "BeforeGoing", "PantrapFlag", "PairCode", "Map") 
collectionevent <-  collectionevent[!xcldvars]


#=====Create merged specimen data file

beename <- merge(genus, species, by ="BeeGenusID")
bees <- merge(specimens, beename, by = "SpeciesID" )

#=======combine collection event data with specimen data

beedata <- merge(bees, collectionevent, by = "collectionevent")

#===reduce data to only Napa and Sonoma project bees

vineyarddata <- beedata[ which(beedata$Project=='NapaSonomaVineyards'), ]

#=====include only netting data
nettingdata <- vineyarddata[ which(vineyarddata$technique=='netting'),]
#nettingdata <- nettingdata[,-which(names(nettingdata)=="Year")]

