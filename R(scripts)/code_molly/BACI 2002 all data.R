# This code does a BACI comparison of all metrics using the 2002 data (date matched). 

# Load,attach, and preview data
baci.02all=read.csv('~/Documents/Molly Hayes/SFSU/Thesis/Molly r code and data/All bee data/2002 BACI file date matched.csv')
attach(baci.02all)
head(baci.02all)
library(reshape2)
library(ggplot2)
library(Rmisc)

# Subset for control only and impact only
baci.02all.control=subset(baci.02all, SiteClass=="Control")
baci.02all.control
# Switch order so "before" is before "after" for the plots
baci.02all.control$Period=factor(baci.02all.control$Period,levels=c("Before","After"))

baci.02all.impact=subset(baci.02all, SiteClass=="Impact")
baci.02all.impact
# Switch order so "before" is before "after" for the plots
baci.02all.impact$Period=factor(baci.02all.impact$Period,levels=c("Before","After"))



# BEE ABUNDANCE
# Compute the difference in sample means between the before and after period within each site.
beeabund.site=dcast(baci.02all,Site+SiteClass~Period,value.var="Bee.abund")
beeabund.site$diff=beeabund.site$After-beeabund.site$Before
beeabund.site
# Remove extra stuff
#beeabund.site=beeabund.site[-1,-3]
#beeabund.site

# Test if the mean difference is the same for the control and impact groups using a 2 sample t-test.
beeabund.result=t.test(diff~SiteClass,data=beeabund.site,var.equal=TRUE)
beeabund.result$diff.in.means=sum(beeabund.result$estimate*c(1,-1))
names(beeabund.result$diff.in.means)="diff.in.means"
beeabund.result$se.diff=beeabund.result$statistic/abs(beeabund.result$diff.in.means)
names(beeabund.result$se.diff)='SE.diff'
beeabund.result
beeabund.result$diff.in.means
beeabund.result$se.diff

# Plot change
beeabund.controlsummarySE=summarySE(baci.02all.control, measurevar="Bee.abund",groupvar=c("SiteClass","Period"))
beeabund.controlsummarySE

beeabund02all.controlplot=ggplot(data=baci.02all.control,aes(x=Period,y=Bee.abund,group=Site,color=Site,shape=Site,ymin=0,ymax=80))+scale_color_manual(values=c("darkgreen","aquamarine4","dodgerblue","turquoise3"))+ylab("Bee abundance")+geom_point(size=4)+geom_line(size=1)+ggtitle("Bee abundance across control sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Bee.abund,group=Period),alpha=0.2)
beeabund02all.controlplot

beeabund.impactsummary=summary(baci.02all.impact$Bee.abund)
beeabund.impactsummary

beeabund02all.impactplot=ggplot(data=baci.02all.impact,aes(x=Period,y=Bee.abund,group=Site,color=Site,shape=Site,ymin=0,ymax=80))+scale_color_manual(values=c("darkred", "firebrick3", "orangered","darkorange1"))+ylab("Bee abundance")+geom_point(size=4)+geom_line(size=1)+ggtitle("Bee abundance across impact sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Bee.abund,group=Period),alpha=0.2)

beeabund02all.impactplot


# TOTAL BEE ABUNDANCE
# Compute the difference in sample means between the before and after period within each site.
#beeabundt.site=dcast(baci.02all,Site+SiteClass~Period,value.var="Bees.total")
#beeabundt.site$diff=beeabundt.site$After-beeabundt.site$Before
#beeabundt.site
# Remove extra stuff
#beeabund.site=beeabund.site[-1,-3]
#beeabund.site

# Test if the mean difference is the same for the control and impact groups using a 2 sample t-test.
#beeabundt.result=t.test(diff~SiteClass,data=beeabundt.site,var.equal=TRUE)
#beeabundt.result$diff.in.means=sum(beeabundt.result$estimate*c(1,-1))
#names(beeabundt.result$diff.in.means)="diff.in.means"
#beeabundt.result$se.diff=beeabundt.result$statistic/abs(beeabundt.result$diff.in.means)
#names(beeabundt.result$se.diff)='SE.diff'
#beeabundt.result
#beeabundt.result$diff.in.means
#beeabundt.result$se.diff

# Plot change
#beeabundt.controlsummarySE=summarySE(baci.02all.control, measurevar="Bees.total",groupvar=c("SiteClass","Period"))
#beeabundt.controlsummarySE

#beeabundt02all.controlplot=ggplot(data=baci.02all.control,aes(x=Period,y=Bee.abundt,group=Site,color=Site,shape=Site,ymin=0,ymax=80))+scale_color_manual(values=c("darkgreen","aquamarine4","dodgerblue","turquoise3"))+ylab("Total bee abundance")+geom_point(size=4)+geom_line(size=1)+ggtitle("Total bee abundance across control sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Bee.abundt,group=Period),alpha=0.2)

#beeabundt02all.controlplot

#beeabundt.impactsummary=summary(baci.02all.impact$Bees.total)
#beeabundt.impactsummary

#beeabundt02all.impactplot=ggplot(data=baci.02all.impact,aes(x=Period,y=Bees.total,group=Site,color=Site,shape=Site))+scale_color_manual(values=c("darkred", "firebrick3", "orangered","darkorange1"))+ylab("Total bee abundance")+geom_point(size=4)+geom_line(size=1)+ggtitle("Total bee abundance across impact sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Bee.abundt,group=Period),alpha=0.2)

#beeabundt02all.impactplot


# BEE SPECIES RICHNESS
# Compute the difference in sample means between the before and after period within each site.
beediv.site=dcast(baci.02all,Site+SiteClass~Period,value.var="Bee.div")
beediv.site$diff=beediv.site$After-beediv.site$Before
beediv.site
# Remove extra stuff
#beediv.site=beediv.site[-1,-3]
#beediv.site

# Test if the mean difference is the same for the control and impact groups using a 2 sample t-test.
beediv.result=t.test(diff~SiteClass,data=beediv.site,var.equal=TRUE)
beediv.result$diff.in.means=sum(beediv.result$estimate*c(1,-1))
names(beediv.result$diff.in.means)="diff.in.means"
beediv.result$se.diff=beediv.result$statistic/abs(beediv.result$diff.in.means)
names(beediv.result$se.diff)='SE.diff'
beediv.result
beediv.result$diff.in.means
beediv.result$se.diff

# Plot change
beediv.controlsummary=summary(baci.02all.control$Bee.div)
beediv.controlsummary

beediv02all.controlplot=ggplot(data=baci.02all.control,aes(x=Period,y=Bee.div,group=Site,color=Site,shape=Site,ymin=0,ymax=35))+scale_color_manual(values=c("darkgreen","aquamarine4","dodgerblue","turquoise3"))+ylab("Bee species richness")+geom_point(size=4)+geom_line(size=1)+ggtitle("Bee species richness across control sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Bee.div,group=Period),alpha=0.2)

beediv02all.controlplot


beediv.impactsummary=summary(baci.02all.impact$Bee.div)
beediv.impactsummary

beediv02all.impactplot=ggplot(data=baci.02all.impact,aes(x=Period,y=Bee.div,group=Site,color=Site,shape=Site,ymin=0,ymax=35))+scale_color_manual(values=c("darkred", "firebrick3", "orangered","darkorange1"))+ylab("Bee species richness")+geom_point(size=4)+geom_line(size=1)+ggtitle("Bee species richness across impact sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Bee.div,group=Period),alpha=0.2)

beediv02all.impactplot


# PLANT SPECIES RICHNESS
# Compute the difference in sample means between the before and after period within each site.
plantdiv.site=dcast(baci.02all,Site+SiteClass~Period,value.var="Plant.div")
plantdiv.site$diff=plantdiv.site$After-plantdiv.site$Before
plantdiv.site
# Remove extra stuff
#plantdiv.site=plantdiv.site[-1,-3]
#plantdiv.site

# Test if the mean difference is the same for the control and impact groups using a 2 sample t-test.
plantdiv.result=t.test(diff~SiteClass,data=plantdiv.site,var.equal=TRUE)
plantdiv.result$diff.in.means=sum(plantdiv.result$estimate*c(1,-1))
names(plantdiv.result$diff.in.means)="diff.in.means"
plantdiv.result$se.diff=plantdiv.result$statistic/abs(plantdiv.result$diff.in.means)
names(plantdiv.result$se.diff)='SE.diff'
plantdiv.result
plantdiv.result$diff.in.means
plantdiv.result$se.diff

# Plot change
plantdiv.controlsummary=summary(baci.02all.control$Plant.div)
plantdiv.controlsummary

plantdiv02all.controlplot=ggplot(data=baci.02all.control,aes(x=Period,y=Plant.div,group=Site,color=Site,shape=Site,ymin=0,ymax=35))+scale_color_manual(values=c("darkgreen","aquamarine4","dodgerblue","turquoise3"))+ylab("Plant species richness")+geom_point(size=4)+geom_line(size=1)+ggtitle("Plant species richness across control sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Bee.div,group=Period),alpha=0.2)

plantdiv02all.controlplot


plantdiv.impactsummary=summary(baci.02all.impact$Plant.div)
plantdiv.impactsummary

plantdiv02all.impactplot=ggplot(data=baci.02all.impact,aes(x=Period,y=Plant.div,group=Site,color=Site,shape=Site,ymin=0,ymax=35))+scale_color_manual(values=c("darkred", "firebrick3", "orangered","darkorange1"))+ylab("Plant species richness")+geom_point(size=4)+geom_line(size=1)+ggtitle("Plant species richness across impact sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Bee.div,group=Period),alpha=0.2)

plantdiv02all.impactplot


# GENERALITY HL
# Compute the difference in sample means between the before and after period within each site.
beegen.site=dcast(baci.02all,Site+SiteClass~Period,value.var="Generality.HL")
beegen.site$diff=beegen.site$After-beegen.site$Before
beegen.site
# Remove extra stuff
#beegen.site=beegen.site[-1,-3]
#beegen.site

# Test if the mean difference is the same for the control and impact groups using a 2 sample t-test.
beegen.result=t.test(diff~SiteClass,data=beegen.site,var.equal=TRUE)
beegen.result$diff.in.means=sum(beegen.result$estimate*c(1,-1))
names(beegen.result$diff.in.means)="diff.in.means"
beegen.result$se.diff=beegen.result$statistic/abs(beegen.result$diff.in.means)
names(beegen.result$se.diff)='SE.diff'
beegen.result
beegen.result$diff.in.means
beegen.result$se.diff

# Plot change
beegen.controlsummary=summary(baci.02all.control$Generality.HL)
beegen.controlsummary

beegen02all.controlplot=ggplot(data=baci.02all.control,aes(x=Period,y=Generality.HL,group=Site,color=Site,shape=Site,ymin=1,ymax=3.5))+scale_color_manual(values=c("darkgreen","aquamarine4","dodgerblue","turquoise3"))+ylab("Bee generality")+geom_point(size=4)+geom_line(size=1)+ggtitle("Bee generality across control sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Generality.HL,group=Period),alpha=0.2)

beegen02all.controlplot


beegen.impactsummary=summary(baci.02all.impact$Generality.HL)
beegen.impactsummary

beegen02all.impactplot=ggplot(data=baci.02all.impact,aes(x=Period,y=Generality.HL,group=Site,color=Site,shape=Site,ymin=1,ymax=3.5))+scale_color_manual(values=c("darkred", "firebrick3", "orangered","darkorange1"))+ylab("Bee generality")+geom_point(size=4)+geom_line(size=1)+ggtitle("Bee generality across impact sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Generality.HL,group=Period),alpha=0.2)

beegen02all.impactplot


# NESTEDNESS
# Compute the difference in sample means between the before and after period within each site.
nest.site=dcast(baci.02all,Site+SiteClass~Period,value.var="Nestedness")
nest.site$diff=nest.site$After-nest.site$Before
nest.site
# Remove extra stuff
#nest.site=nest.site[-1,-3]
#nest.site

# Test if the mean difference is the same for the control and impact groups using a 2 sample t-test.
nest.result=t.test(diff~SiteClass,data=nest.site,var.equal=TRUE)
nest.result$diff.in.means=sum(nest.result$estimate*c(1,-1))
names(nest.result$diff.in.means)="diff.in.means"
nest.result$se.diff=nest.result$statistic/abs(nest.result$diff.in.means)
names(nest.result$se.diff)='SE.diff'
nest.result
nest.result$diff.in.means
nest.result$se.diff

# Plot change
nest.controlsummary=summary(baci.02all.control$Nestedness)
nest.controlsummary

nest02all.controlplot=ggplot(data=baci.02all.control,aes(x=Period,y=Nestedness,group=Site,color=Site,shape=Site,ymin=5,ymax=50))+scale_color_manual(values=c("darkgreen","aquamarine4","dodgerblue","turquoise3"))+ylab("Nestedness")+geom_point(size=4)+geom_line(size=1)+ggtitle("Nestedness across control sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Nestedness,group=Period),alpha=0.2)

nest02all.controlplot


nest.impactsummary=summary(baci.02all.impact$Nestedness)
nest.impactsummary

nest02all.impactplot=ggplot(data=baci.02all.impact,aes(x=Period,y=Nestedness,group=Site,color=Site,shape=Site,ymin=5,ymax=50))+scale_color_manual(values=c("darkred", "firebrick3", "orangered","darkorange1"))+ylab("Nestedness")+geom_point(size=4)+geom_line(size=1)+ggtitle("Nestedness across impact sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Nestedness,group=Period),alpha=0.2)

nest02all.impactplot


# CONNECTANCE
# Compute the difference in sample means between the before and after period within each site.
con.site=dcast(baci.02all,Site+SiteClass~Period,value.var="Connectance")
con.site$diff=con.site$After-con.site$Before
con.site
# Remove extra stuff
#con.site=con.site[-1,-3]
#con.site

# Test if the mean difference is the same for the control and impact groups using a 2 sample t-test.
con.result=t.test(diff~SiteClass,data=con.site,var.equal=TRUE)
con.result$diff.in.means=sum(con.result$estimate*c(1,-1))
names(con.result$diff.in.means)="diff.in.means"
con.result$se.diff=con.result$statistic/abs(con.result$diff.in.means)
names(con.result$se.diff)='SE.diff'
con.result
con.result$diff.in.means
con.result$se.diff

# Plot change
con.controlsummary=summary(baci.02all.control$Connectance)
con.controlsummary

con02all.controlplot=ggplot(data=baci.02all.control,aes(x=Period,y=Connectance,group=Site,color=Site,shape=Site,ymin=0.09,ymax=0.19))+scale_color_manual(values=c("darkgreen","aquamarine4","dodgerblue","turquoise3"))+ylab("Connectance")+geom_point(size=4)+geom_line(size=1)+ggtitle("Connectance across control sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Connectance,group=Period),alpha=0.2)

con02all.controlplot


con.impactsummary=summary(baci.02all.impact$Connectance)
con.impactsummary

con02all.impactplot=ggplot(data=baci.02all.impact,aes(x=Period,y=Connectance,group=Site,color=Site,shape=Site,ymin=0.09,ymax=0.19))+scale_color_manual(values=c("darkred", "firebrick3", "orangered","darkorange1"))+ylab("Connectance")+geom_point(size=4)+geom_line(size=1)+ggtitle("Connectance across impact sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Connectance,group=Period),alpha=0.2)

con02all.impactplot


# WEIGHTED CONNECTANCE
# Compute the difference in sample means between the before and after period within each site.
wcon.site=dcast(baci.02all,Site+SiteClass~Period,value.var="Weighted.connectance")
wcon.site$diff=wcon.site$After-wcon.site$Before
wcon.site
# Remove extra stuff
#con.site=con.site[-1,-3]
#con.site

# Test if the mean difference is the same for the control and impact groups using a 2 sample t-test.
wcon.result=t.test(diff~SiteClass,data=wcon.site,var.equal=TRUE)
wcon.result$diff.in.means=sum(wcon.result$estimate*c(1,-1))
names(wcon.result$diff.in.means)="diff.in.means"
wcon.result$se.diff=wcon.result$statistic/abs(wcon.result$diff.in.means)
names(wcon.result$se.diff)='SE.diff'
wcon.result
wcon.result$diff.in.means
wcon.result$se.diff

# Plot change
wcon.controlsummary=summary(baci.02all.control$Weighted.connectance)
wcon.controlsummary

wcon02all.controlplot=ggplot(data=baci.02all.control,aes(x=Period,y=Weighted.connectance,group=Site,color=Site,shape=Site,ymin=0,ymax=0.5))+scale_color_manual(values=c("darkgreen","aquamarine4","dodgerblue","turquoise3"))+ylab("Weighted connectance")+geom_point(size=4)+geom_line(size=1)+ggtitle("Weighted connectance across control sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Weighted.connectance,group=Period),alpha=0.2)

wcon02all.controlplot


wcon.impactsummary=summary(baci.02all.impact$Weighted.connectance)
wcon.impactsummary

wcon02all.impactplot=ggplot(data=baci.02all.impact,aes(x=Period,y=Weighted.connectance,group=Site,color=Site,shape=Site,ymin=0,ymax=0.5))+scale_color_manual(values=c("darkred", "firebrick3", "orangered","darkorange1"))+ylab("Weighted connectance")+geom_point(size=4)+geom_line(size=1)+ggtitle("Weighted connectance across impact sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Weighted.connectance,group=Period),alpha=0.2)

wcon02all.impactplot


# EVENNESS
# Compute the difference in sample means between the before and after period within each site.
even.site=dcast(baci.02all,Site+SiteClass~Period,value.var="Evenness")
even.site$diff=even.site$After-even.site$Before
even.site
# Remove extra stuff
#con.site=con.site[-1,-3]
#con.site

# Test if the mean difference is the same for the control and impact groups using a 2 sample t-test.
even.result=t.test(diff~SiteClass,data=even.site,var.equal=TRUE)
even.result$diff.in.means=sum(even.result$estimate*c(1,-1))
names(even.result$diff.in.means)="diff.in.means"
even.result$se.diff=even.result$statistic/abs(even.result$diff.in.means)
names(even.result$se.diff)='SE.diff'
even.result
even.result$diff.in.means
even.result$se.diff

# Plot change
even.controlsummary=summary(baci.02all.control$Evenness)
even.controlsummary

even02all.controlplot=ggplot(data=baci.02all.control,aes(x=Period,y=Evenness,group=Site,color=Site,shape=Site,ymin=0.3,ymax=0.6))+ylab("Evenness")+scale_color_manual(values=c("darkgreen","aquamarine4","dodgerblue","turquoise3"))+geom_point(size=4)+geom_line(size=1)+ggtitle("Evenness across control sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Evenness,group=Period),alpha=0.2)

even02all.controlplot


even.impactsummary=summary(baci.02all.impact$Evenness)
even.impactsummary

even02all.impactplot=ggplot(data=baci.02all.impact,aes(x=Period,y=Evenness,group=Site,color=Site,shape=Site,ymin=0.3,ymax=0.6))+scale_color_manual(values=c("darkred", "firebrick3", "orangered","darkorange1"))+ylab("Evenness")+geom_point(size=4)+geom_line(size=1)+ggtitle("Evenness across impact sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Evenness,group=Period),alpha=0.2)

even02all.impactplot


# MODULARITY
# Compute the difference in sample means between the before and after period within each site.
mod.site=dcast(baci.02all,Site+SiteClass~Period,value.var="Modularity")
mod.site$diff=mod.site$After-mod.site$Before
mod.site
# Remove extra stuff
#con.site=con.site[-1,-3]
#con.site

# Test if the mean difference is the same for the control and impact groups using a 2 sample t-test.
mod.result=t.test(diff~SiteClass,data=mod.site,var.equal=TRUE)
mod.result$diff.in.means=sum(mod.result$estimate*c(1,-1))
names(mod.result$diff.in.means)="diff.in.means"
mod.result$se.diff=mod.result$statistic/abs(mod.result$diff.in.means)
names(mod.result$se.diff)='SE.diff'
mod.result
mod.result$diff.in.means
mod.result$se.diff

# Plot change
mod.controlsummary=summary(baci.02all.control$Modularity)
mod.controlsummary

mod02all.controlplot=ggplot(data=baci.02all.control,aes(x=Period,y=Modularity,group=Site,color=Site,shape=Site,ymin=0.25,ymax=0.8))+scale_color_manual(values=c("darkgreen","aquamarine4","dodgerblue","turquoise3"))+ylab("Modularity")+geom_point(size=4)+geom_line(size=1)+ggtitle("Modularity across control sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Modularity,group=Period),alpha=0.2)

mod02all.controlplot


mod.impactsummary=summary(baci.02all.impact$Modularity)
mod.impactsummary

mod02all.impactplot=ggplot(data=baci.02all.impact,aes(x=Period,y=Modularity,group=Site,color=Site,shape=Site,ymin=0.25,ymax=0.8))+scale_color_manual(values=c("darkred", "firebrick3", "orangered","darkorange1"))+ylab("Modularity")+geom_point(size=4)+geom_line(size=1)+ggtitle("Modularity across impact sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Modularity,group=Period),alpha=0.2)

mod02all.impactplot


# SHANNON DIVERSITY
# Compute the difference in sample means between the before and after period within each site.
sdiv.site=dcast(baci.02all,Site+SiteClass~Period,value.var="Shannon.diversity")
sdiv.site$diff=sdiv.site$After-sdiv.site$Before
sdiv.site
# Remove extra stuff
#con.site=con.site[-1,-3]
#con.site

# Test if the mean difference is the same for the control and impact groups using a 2 sample t-test.
sdiv.result=t.test(diff~SiteClass,data=sdiv.site,var.equal=TRUE)
sdiv.result$diff.in.means=sum(sdiv.result$estimate*c(1,-1))
names(sdiv.result$diff.in.means)="diff.in.means"
sdiv.result$se.diff=sdiv.result$statistic/abs(sdiv.result$diff.in.means)
names(sdiv.result$se.diff)='SE.diff'
sdiv.result
sdiv.result$diff.in.means
sdiv.result$se.diff

# Plot change
sdiv.controlsummary=summary(baci.02all.control$Shannon.diversity)
sdiv.controlsummary

sdiv02all.controlplot=ggplot(data=baci.02all.control,aes(x=Period,y=Shannon.diversity,group=Site,color=Site,shape=Site,ymin=1.5,ymax=4))+scale_color_manual(values=c("darkgreen","aquamarine4","dodgerblue","turquoise3"))+ylab("Shannon Diversity")+geom_point(size=4)+geom_line(size=1)+ggtitle("Shannon diversity across control sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Shannon.diversity,group=Period),alpha=0.2)

sdiv02all.controlplot


sdiv.impactsummary=summary(baci.02all.impact$Shannon.diversity)
sdiv.impactsummary

sdiv02all.impactplot=ggplot(data=baci.02all.impact,aes(x=Period,y=Shannon.diversity,group=Site,color=Site,shape=Site,ymin=1.5,ymax=4))+scale_color_manual(values=c("darkred", "firebrick3", "orangered","darkorange1"))+ylab("Shannon diversity")+geom_point(size=4)+geom_line(size=1)+ggtitle("Shannon diversity across impact sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=Shannon.diversity,group=Period),alpha=0.2)

sdiv02all.impactplot

# BEE DIET
# Compute the difference in sample means between the before and after period within each site.
ppol.site=dcast(baci.02all,Site+SiteClass~Period,value.var="P.polylectic")
ppol.site$diff=ppol.site$After-ppol.site$Before
ppol.site
# Remove extra stuff
#con.site=con.site[-1,-3]
#con.site

# Test if the mean difference is the same for the control and impact groups using a 2 sample t-test.
ppol.result=t.test(diff~SiteClass,data=ppol.site,var.equal=TRUE)
ppol.result$diff.in.means=sum(ppol.result$estimate*c(1,-1))
names(ppol.result$diff.in.means)="diff.in.means"
ppol.result$se.diff=ppol.result$statistic/abs(ppol.result$diff.in.means)
names(ppol.result$se.diff)='SE.diff'
ppol.result
ppol.result$diff.in.means
ppol.result$se.diff

# Plot change
ppol.controlsummary=summary(baci.02all.control$P.polylectic)
ppol.controlsummary

ppol02all.controlplot=ggplot(data=baci.02all.control,aes(x=Period,y=P.annuals,group=Site,color=Site,shape=Site))+scale_color_manual(values=c("darkgreen","aquamarine4","dodgerblue","turquoise3"))+ylab("Percent")+geom_point(size=4)+geom_line(size=1)+ggtitle("Percent polylectic across control sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=P.polylectic,group=Period),alpha=0.2)

ppol02all.controlplot


ppol.impactsummary=summary(baci.02all.impact$P.polylectic)
ppol.impactsummary

ppol02all.impactplot=ggplot(data=baci.02all.impact,aes(x=Period,y=P.polylectic,group=Site,color=Site,shape=Site))+scale_color_manual(values=c("darkred", "firebrick3", "orangered","darkorange1"))+ylab("Percent")+geom_point(size=4)+geom_line(size=1)+ggtitle("Percent polylectic across impact sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=P.polylectic,group=Period),alpha=0.2)

ppol02all.impactplot


# BEE NESTING
# Compute the difference in sample means between the before and after period within each site.
pbelow.site=dcast(baci.02all,Site+SiteClass~Period,value.var="P.below")
pbelow.site$diff=pbelow.site$After-pbelow.site$Before
pbelow.site
# Remove extra stuff
#con.site=con.site[-1,-3]
#con.site

# Test if the mean difference is the same for the control and impact groups using a 2 sample t-test.
pbelow.result=t.test(diff~SiteClass,data=pbelow.site,var.equal=TRUE)
pbelow.result$diff.in.means=sum(pbelow.result$estimate*c(1,-1))
names(pbelow.result$diff.in.means)="diff.in.means"
pbelow.result$se.diff=pbelow.result$statistic/abs(pbelow.result$diff.in.means)
names(pbelow.result$se.diff)='SE.diff'
pbelow.result
pbelow.result$diff.in.means
pbelow.result$se.diff

# Plot change
pbelow.controlsummary=summary(baci.02all.control$P.below)
pbelow.controlsummary

pbelow02all.controlplot=ggplot(data=baci.02all.control,aes(x=Period,y=P.annuals,group=Site,color=Site,shape=Site))+scale_color_manual(values=c("darkgreen","aquamarine4","dodgerblue","turquoise3"))+ylab("Percent below ground")+geom_point(size=4)+geom_line(size=1)+ggtitle("Percent below ground across control sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=P.below,group=Period),alpha=0.2)

pbelow02all.controlplot


pbelow.impactsummary=summary(baci.02all.impact$P.below)
pbelow.impactsummary

pbelow02all.impactplot=ggplot(data=baci.02all.impact,aes(x=Period,y=P.below,group=Site,color=Site,shape=Site))+scale_color_manual(values=c("darkred", "firebrick3", "orangered","darkorange1"))+ylab("Percent below ground")+geom_point(size=4)+geom_line(size=1)+ggtitle("Percent below ground across impact sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=P.below,group=Period),alpha=0.2)

pbelow02all.impactplot


# ANNUALS/PERENNIALS
# Compute the difference in sample means between the before and after period within each site.
pann.site=dcast(baci.02all,Site+SiteClass~Period,value.var="P.annuals")
pann.site$diff=pann.site$After-pann.site$Before
pann.site
# Remove extra stuff
#con.site=con.site[-1,-3]
#con.site

# Test if the mean difference is the same for the control and impact groups using a 2 sample t-test.
pann.result=t.test(diff~SiteClass,data=pann.site,var.equal=TRUE)
pann.result$diff.in.means=sum(pann.result$estimate*c(1,-1))
names(pann.result$diff.in.means)="diff.in.means"
pann.result$se.diff=pann.result$statistic/abs(pann.result$diff.in.means)
names(pann.result$se.diff)='SE.diff'
pann.result
pann.result$diff.in.means
pann.result$se.diff

# Plot change
pann.controlsummary=summary(baci.02all.control$P.annuals)
pann.controlsummary

pann02all.controlplot=ggplot(data=baci.02all.control,aes(x=Period,y=P.annuals,group=Site,color=Site,shape=Site,ymin=20,ymax=95))+scale_color_manual(values=c("darkgreen","aquamarine4","dodgerblue","turquoise3"))+ylab("Percent")+geom_point(size=4)+geom_line(size=1)+ggtitle("Percent annuals across control sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=P.annuals,group=Period),alpha=0.2)

pann02all.controlplot


pann.impactsummary=summary(baci.02all.impact$P.annuals)
pann.impactsummary

pann02all.impactplot=ggplot(data=baci.02all.impact,aes(x=Period,y=P.annuals,group=Site,color=Site,shape=Site,ymin=20,ymax=95))+scale_color_manual(values=c("darkred", "firebrick3", "orangered","darkorange1"))+ylab("Percent")+geom_point(size=4)+geom_line(size=1)+ggtitle("Percent annuals across impact sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=P.annuals,group=Period),alpha=0.2)

pann02all.impactplot


# NATIVE/NONNATIVE
# Compute the difference in sample means between the before and after period within each site.
pnative.site=dcast(baci.02all,Site+SiteClass~Period,value.var="P.native")
pnative.site$diff=pnative.site$After-pnative.site$Before
pnative.site
# Remove extra stuff
#con.site=con.site[-1,-3]
#con.site

# Test if the mean difference is the same for the control and impact groups using a 2 sample t-test.
pnative.result=t.test(diff~SiteClass,data=pnative.site,var.equal=TRUE)
pnative.result$diff.in.means=sum(pnative.result$estimate*c(1,-1))
names(pnative.result$diff.in.means)="diff.in.means"
pnative.result$se.diff=pnative.result$statistic/abs(pnative.result$diff.in.means)
names(pnative.result$se.diff)='SE.diff'
pnative.result
pnative.result$diff.in.means
pnative.result$se.diff

# Plot change
pnative.controlsummary=summary(baci.02all.control$P.native)
pnative.controlsummary

pnative02all.controlplot=ggplot(data=baci.02all.control,aes(x=Period,y=P.native,group=Site,color=Site,shape=Site,ymin=0,ymax=90))+scale_color_manual(values=c("darkgreen","aquamarine4","dodgerblue","turquoise3"))+ylab("Percent")+geom_point(size=4)+geom_line(size=1)+ggtitle("Percent native across control sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=P.native,group=Period),alpha=0.2)

pnative02all.controlplot


pnative.impactsummary=summary(baci.02all.impact$P.native)
pnative.impactsummary

pnative02all.impactplot=ggplot(data=baci.02all.impact,aes(x=Period,y=P.native,group=Site,color=Site,shape=Site,ymin=0,ymax=90))+scale_color_manual(values=c("darkred", "firebrick3", "orangered","darkorange1"))+ylab("Percent")+geom_point(size=4)+geom_line(size=1)+ggtitle("Percent native across impact sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=P.native,group=Period),alpha=0.2)

pnative02all.impactplot


# INVASIVE/NONINVASIVE
# Compute the difference in sample means between the before and after period within each site.
pinvasive.site=dcast(baci.02all,Site+SiteClass~Period,value.var="P.invasive")
pinvasive.site$diff=pinvasive.site$After-pinvasive.site$Before
pinvasive.site
# Remove extra stuff
#con.site=con.site[-1,-3]
#con.site

# Test if the mean difference is the same for the control and impact groups using a 2 sample t-test.
pinvasive.result=t.test(diff~SiteClass,data=pinvasive.site,var.equal=TRUE)
pinvasive.result$diff.in.means=sum(pinvasive.result$estimate*c(1,-1))
names(pinvasive.result$diff.in.means)="diff.in.means"
pinvasive.result$se.diff=pinvasive.result$statistic/abs(pinvasive.result$diff.in.means)
names(pinvasive.result$se.diff)='SE.diff'
pinvasive.result
pinvasive.result$diff.in.means
pinvasive.result$se.diff

# Plot change
pinvasive.controlsummary=summary(baci.02all.control$P.invasive)
pinvasive.controlsummary

pinvasive02all.controlplot=ggplot(data=baci.02all.control,aes(x=Period,y=P.invasive,group=Site,color=Site,shape=Site,ymin=0,ymax=85))+scale_color_manual(values=c("darkgreen","aquamarine4","dodgerblue","turquoise3"))+ylab("Percent")+geom_point(size=4)+geom_line(size=1)+ggtitle("Percent invasive across control sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=P.invasive,group=Period),alpha=0.2)

pinvasive02all.controlplot


pinvasive.impactsummary=summary(baci.02all.impact$P.invasive)
pinvasive.impactsummary

pinvasive02all.impactplot=ggplot(data=baci.02all.impact,aes(x=Period,y=P.invasive,group=Site,color=Site,shape=Site,ymin=0,ymax=85))+scale_color_manual(values=c("darkred", "firebrick3", "orangered","darkorange1"))+ylab("Percent")+geom_point(size=4)+geom_line(size=1)+ggtitle("Percent invasive across impact sites")+theme(plot.title = element_text(hjust = 0.5))+geom_boxplot(aes(x=Period,y=P.invasive,group=Period),alpha=0.2)

pinvasive02all.impactplot


# Plot all graphs on one page
library(gridExtra)
grid.arrange(beeabund02all.controlplot, beeabund02all.impactplot, beediv02all.controlplot, beediv02all.impactplot, plantdiv02all.controlplot, plantdiv02all.impactplot, nrow=3)

grid.arrange(beegen02all.controlplot, beegen02all.impactplot, nest02all.controlplot, nest02all.impactplot, mod02all.controlplot, mod02all.impactplot, nrow=3)

grid.arrange(con02all.controlplot, con02all.impactplot, wcon02all.controlplot, wcon02all.impactplot, even02all.controlplot, even02all.impactplot, nrow=3)

grid.arrange(pann02all.controlplot, pann02all.impactplot, pnative02all.controlplot, pnative02all.impactplot, pinvasive02all.controlplot, pinvasive02all.impactplot, nrow=3)
