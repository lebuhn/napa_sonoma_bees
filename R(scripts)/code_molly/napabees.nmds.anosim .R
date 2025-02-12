#This script finds a non-parameteric monotonic relationship between the dissimilarities in the samples matrix, and plots the location of each site in a low-dimensional space (similar to principle component analysis).
# ============================================================
# Based on a Tutorial on drawing an NMDS plot using ggplot2
# by Umer Zeeshan Ijaz (http://userweb.eng.gla.ac.uk/umer.ijaz)
# =============================================================
# 1.	Tonietto (modeled after her paper)
# 	Bee community composition
# i.	NMDS in vegan package
# ii.	calculated mean relative abundance for each site x year
# iii.	data were relativized by species and site maxima to account for high variation in abundance
# iv.	used 3 axes in NMDS
# d.	To test for differences in bee composition by site type, did permanova
# i.	bray Curtis dissimilarity
# ii.	10000 permutations
# iii.	if significant
# 1.	compared which species using similarity percentage (SIMPER) analysis in vegan which evaluates contribution of each species to Bray Curtis dissimilarity
# e.	Traits space occupied by bee communities
# i.	traits: native, nest, lecty, sociality, size, phenology
# ii.	calculated community weighted mean trait values for each site
# iii.	used weights that reflected number of classes
# iv.	did a two axis NMDS ordination
# v.	used Permanova to test for differences and evaluate which traits.

# set the directory - you may have to change this on your machine
setwd('~/Dropbox/dune_occ')
rm(list=ls())

#read in the data
abund_table<-read.table("data/SPE.dune.csv", header = TRUE, sep = ";")
#Make the table a dataframe
abund_table<-as.data.frame(abund_table)
meta_table<-read.csv("data/ENV.dune.csv",header = TRUE, sep = ';')
#Just a check to ensure that the samples in meta_table are in the same order as in abund_table
meta_table<-meta_table[rownames(abund_table),]


lbls <-as.integer(abund_table$Sites)
grps <- levels(abund_table$Sites)
ngrps <- length(grps)
#Get grouping information
grouping_info<-data.frame(meta_table)
head(grouping_info)


#Load vegan and ggplot2 libraries
library(ggplot2)
library(vegan)

#Get MDS stats
sol<-metaMDS(abund_table,distance = "bray", k = 2, trymax = 50)

stressplot(sol)


ordiplot(sol,type="n")
orditorp(sol,display="species",col="red",air=0.01)
orditorp(sol,display="sites",col=c(rep("green",2),rep("blue",2), rep("red",2),rep("orange",2), rep("pink",2),rep("yellow",2), rep("purple",2)),
         air=0.01,cex=1.25)

#NMDS and anosim from http://geoffreyzahn.com/nmds-example/

#Make a new data frame, and put Treatment there, to be useful for coloring, and shape of point
NMDS=data.frame(x=sol$point[,1],y=sol$point[,2],Sites=as.factor(grouping_info[,1]),Treatment=as.factor(grouping_info[,2]))


#look at NMDS
head(NMDS)
ordiplot(sol, display="sites", type="n") 
points(sol, col="black", pch = lbls) 
ordihull(sol, NMDS$Treatment, scaling = "symmetric", lty=lbls) 

#Get spread of points based on treatment
plot.new()
ord<-ordiellipse(sol, as.factor(grouping_info[,2]) ,display = "sites", kind ="sd", conf = 0.95, label = T)
dev.off()

#Reference: http://stackoverflow.com/questions/13794419/plotting-ordiellipse-function-from-vegan-package-onto-nmds-plot-created-in-ggplo
#Data frame df_ell contains values to show ellipses. It is calculated with function veganCovEllipse which is hidden in vegan package. This function is applied to each level of NMDS (group) and it uses also function cov.wt to calculate covariance matrix.
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

#Generate ellipse points
df_ell <- data.frame()
for(g in levels(NMDS$Treatment)){
  if(g!="" && (g %in% names(ord))){
    
    df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$Treatment==g,],
                                                     veganCovEllipse(ord[[g]]$cov,ord[[g]]$center,ord[[g]]$scale)))
                                  ,Treatment=g))
  }
}

head(df_ell)
# NMDS1      NMDS2 Country
# 1 1.497379 -0.7389216       T
# 2 1.493876 -0.6800680       T
# 3 1.483383 -0.6196981       T
# 4 1.465941 -0.5580502       T
# 5 1.441619 -0.4953674       T
# 6 1.410512 -0.4318972       T

#Generate mean values from NMDS plot grouped on Countries
NMDS.mean=aggregate(NMDS[,1:2],list(group=NMDS$Treatment),mean)

# > NMDS.mean
# group          x          y
# 1     T -0.2774564 -0.2958445
# 2     V  0.1547353  0.1649902

#Now do the actual plotting
library(ggplot2)

shape_values<-seq(1,14)

p<-ggplot(data=NMDS,aes(x,y,colour=Treatment))
p<-p+ annotate("text",x=NMDS.mean$x,y=NMDS.mean$y,label=NMDS.mean$group,size=4)
p<-p+ geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2), size=1, linetype=2)
p<-p+geom_point(aes(shape=NMDS$Sites))+scale_shape_manual(values=shape_values)+theme_bw() 
pdf("dune.NMDS.pdf")
print(p)
dev.off()


#Testing for differences Adonis.  Adonis is a nonparametric statistical method 
#that takes a distance matrix and a category to determine sample grouping from. 
#It computes an R2 value (effect size) which shows the percentage of variation 
#explained by the supplied category, as well as a p-value to determine the statistical
#significance. Adonis creates a set by first identifying the relevant centroids of the 
#data and then calculating the squared deviations from these points. After that, significance 
#tests are performed using F-tests based on sequential sums of squares from permutations of 
#the raw data.

min_depth = min(colSums(abund_table))

#t_otus_rarefied <- as.data.frame(round(rrarefy(abund_table, min_depth)))
otus_dist = as.matrix((vegdist(abund_table, "bray")))

anosim_treatment = anosim(otus_dist, grouping_info$Treatement)
anosim_treatment # take a look at results
summary(anosim_treatment)
plot(anosim_treatment)


#ANOSIM is a method that tests whether two or more groups of samples are significantly different 
#(similar to adonis, above). You can specify a category in the metadata to separate samples into 
#groups and then test whether there are significant differences between those groups.  Since ANOSIM 
#is nonparametric, statistical significance is determined through permutations. Note: ANOSIM only 
#works with a categorical variable that is used to do the grouping. Mantel is recommended for continuous variables.

adonis_location = adonis(otus_dist ~ Treatement, meta_table)

# Load multSE function
library(devtools)
multSE = source_url("https://raw.githubusercontent.com/jslefche/multSE/master/R/multSE.R")[[1]]
adonis_location # take a look at results; there are no summary() or plot() methods included