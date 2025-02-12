# Load and preview data
regressiondat=read.csv('~/Documents/Molly Hayes/SFSU/Thesis/Molly r code and data/All bee data/Resilience regression 2003 no abs.csv',header=TRUE)
head(regressiondat)
library(ggplot2)

# Subset for metric = Bee.abund
bee.abund=subset(regressiondat,Metric=="Bee.abund")
bee.abund=subset(bee.abund,Site!="Wappo")
bee.abund=subset(bee.abund,Site!="Veterans")
bee.abund=subset(bee.abund,Site!="Quintessa")
bee.abund=subset(bee.abund,Site!="Goode")
head(bee.abund)
bee.abund=as.data.frame(bee.abund)

bee.abundplot=ggplot(bee.abund,aes(Before,Change))+geom_point()+geom_smooth(method='lm', se=FALSE)+ggtitle("Bee abundance")+theme(plot.title = element_text(hjust = 0.5))
bee.abundplot

bee.abundlm=lm(formula=Before~Change,data=bee.abund)
summary(bee.abundlm)


# Subset for metric = Bee.div
bee.div=subset(regressiondat,Metric=="Bee.div")
bee.div=subset(bee.div,Site!="Wappo")
bee.div=subset(bee.div,Site!="Veterans")
bee.div=subset(bee.div,Site!="Quintessa")
bee.div=subset(bee.div,Site!="Goode")
head(bee.div)
bee.div=as.data.frame(bee.div)

bee.divplot=ggplot(bee.div,aes(Before,Change))+geom_point()+geom_smooth(method='lm', se=FALSE)+ggtitle("Bee species richness")+theme(plot.title = element_text(hjust = 0.5))
bee.divplot

bee.divlm=lm(formula=Before~Change,data=bee.div)
summary(bee.divlm)


# Subset for metric = Plant.div
plant.div=subset(regressiondat,Metric=="Plant.div")
plant.div=subset(plant.div,Site!="Wappo")
plant.div=subset(plant.div,Site!="Veterans")
plant.div=subset(plant.div,Site!="Quintessa")
plant.div=subset(plant.div,Site!="Goode")
head(plant.div)
plant.div=as.data.frame(plant.div)

plant.divplot=ggplot(plant.div,aes(Before,Change))+geom_point()+geom_smooth(method='lm', se=FALSE)+ggtitle("Plant species richness")+theme(plot.title = element_text(hjust = 0.5))
plant.divplot

plant.divlm=lm(formula=Before~Change,data=plant.div)
summary(plant.divlm)


# Subset for metric = Generality
generality=subset(regressiondat,Metric=="Generality.HL")
generality=subset(generality,Site!="Wappo")
generality=subset(generality,Site!="Veterans")
generality=subset(generality,Site!="Quintessa")
generality=subset(generality,Site!="Goode")
head(generality)
generality=as.data.frame(generality)

generalityplot=ggplot(generality,aes(Before,Change))+geom_point()+geom_smooth(method='lm', se=FALSE)+ggtitle("Generality")+theme(plot.title = element_text(hjust = 0.5))
generalityplot

generalitylm=lm(formula=Before~Change,data=generality)
summary(generalitylm)

generalitybeeabundlm=lm(formula=generality$Before~bee.abund$Change)
summary(generalitybeeabundlm)

generalitybeedivlm=lm(formula=generality$Before~bee.div$Change)
summary(generalitybeedivlm)

generalityplantdivlm=lm(formula=generality$Before~plant.div$Change)
summary(generalityplantdivlm)


# Subset for metric = Nestedness
nestedness=subset(regressiondat,Metric=="Nestedness")
nestedness=subset(nestedness,Site!="Wappo")
nestedness=subset(nestedness,Site!="Veterans")
nestedness=subset(nestedness,Site!="Quintessa")
nestedness=subset(nestedness,Site!="Goode")
head(nestedness)
nestedness=as.data.frame(nestedness)

nestednessplot=ggplot(nestedness,aes(Before,Change))+geom_point()+geom_smooth(method='lm', se=FALSE)+ggtitle("Nestedness")+theme(plot.title = element_text(hjust = 0.5))
nestednessplot

nestednesslm=lm(formula=Before~Change,data=nestedness)
summary(nestednesslm)

nestednessbeeabundlm=lm(formula=nestedness$Before~bee.abund$Change)
summary(nestednessbeeabundlm)

nestednessbeedivlm=lm(formula=nestedness$Before~bee.div$Change)
summary(nestednessbeedivlm)

nestednessplantdivlm=lm(formula=nestedness$Before~plant.div$Change)
summary(nestednessplantdivlm)


# Subset for metric = Connectance
connectance=subset(regressiondat,Metric=="Connectance")
connectance=subset(connectance,Site!="Wappo")
connectance=subset(connectance,Site!="Veterans")
connectance=subset(connectance,Site!="Quintessa")
connectance=subset(connectance,Site!="Goode")
head(connectance)
connectance=as.data.frame(connectance)

connectanceplot=ggplot(connectance,aes(Before,Change))+geom_point()+geom_smooth(method='lm', se=FALSE)+ggtitle("Connectance")+theme(plot.title = element_text(hjust = 0.5))
connectanceplot

connectancelm=lm(formula=Before~Change,data=connectance)
summary(connectancelm)

connectancebeeabundlm=lm(formula=connectance$Before~bee.abund$Change)
summary(connectancebeeabundlm)

connectancebeedivlm=lm(formula=connectance$Before~bee.div$Change)
summary(connectancebeedivlm)

connectanceplantdivlm=lm(formula=connectance$Before~plant.div$Change)
summary(connectanceplantdivlm)


# Subset for metric = Wconnectance
Wconnectance=subset(regressiondat,Metric=="W.connectance")
Wconnectance=subset(Wconnectance,Site!="Wappo")
Wconnectance=subset(Wconnectance,Site!="Veterans")
Wconnectance=subset(Wconnectance,Site!="Quintessa")
Wconnectance=subset(Wconnectance,Site!="Goode")
head(Wconnectance)
Wconnectance=as.data.frame(Wconnectance)

Wconnectanceplot=ggplot(Wconnectance,aes(Before,Change))+geom_point()+geom_smooth(method='lm', se=FALSE)+ggtitle("Weighted connectance")+theme(plot.title = element_text(hjust = 0.5))
Wconnectanceplot

Wconnectancelm=lm(formula=Before~Change,data=Wconnectance)
summary(Wconnectancelm)

wconnectancebeeabundlm=lm(formula=Wconnectance$Before~bee.abund$Change)
summary(wconnectancebeeabundlm)

wconnectancebeedivlm=lm(formula=Wconnectance$Before~bee.div$Change)
summary(wconnectancebeedivlm)

wconnectanceplantdivlm=lm(formula=Wconnectance$Before~plant.div$Change)
summary(wconnectanceplantdivlm)


# Subset for metric = Evenness
Evenness=subset(regressiondat,Metric=="Evenness")
Evenness=subset(Evenness,Site!="Wappo")
Evenness=subset(Evenness,Site!="Veterans")
Evenness=subset(Evenness,Site!="Quintessa")
Evenness=subset(Evenness,Site!="Goode")
head(Evenness)
Evenness=as.data.frame(Evenness)

Evennessplot=ggplot(Evenness,aes(Before,Change))+geom_point()+geom_smooth(method='lm', se=FALSE)+ggtitle("Evenness")+theme(plot.title = element_text(hjust = 0.5))
Evennessplot

Evennesslm=lm(formula=Before~Change,data=Evenness)
summary(Evennesslm)

Evennessbeeabundlm=lm(formula=Evenness$Before~bee.abund$Change)
summary(Evennessbeeabundlm)

Evennessbeedivlm=lm(formula=Evenness$Before~bee.div$Change)
summary(Evennessbeedivlm)

Evennessplantdivlm=lm(formula=Evenness$Before~plant.div$Change)
summary(Evennessplantdivlm)


# Subset for metric = Modularity
Modularity=subset(regressiondat,Metric=="Modularity")
Modularity=subset(Modularity,Site!="Wappo")
Modularity=subset(Modularity,Site!="Veterans")
Modularity=subset(Modularity,Site!="Quintessa")
Modularity=subset(Modularity,Site!="Goode")
head(Modularity)
Modularity=as.data.frame(Modularity)

Modularityplot=ggplot(Modularity,aes(Before,Change))+geom_point()+geom_smooth(method='lm', se=FALSE)+ggtitle("Modularity")+theme(plot.title = element_text(hjust = 0.5))
Modularityplot

Modularitylm=lm(formula=Before~Change,data=Modularity)
summary(Modularitylm)

Modularitybeeabundlm=lm(formula=Modularity$Before~bee.abund$Change)
summary(Modularitybeeabundlm)

Modularitybeedivlm=lm(formula=Modularity$Before~bee.div$Change)
summary(Modularitybeedivlm)

Modularityplantdivlm=lm(formula=Modularity$Before~plant.div$Change)
summary(Modularityplantdivlm)


# Subset for metric = Sdiv
Sdiv=subset(regressiondat,Metric=="S.div")
Sdiv=subset(Sdiv,Site!="Wappo")
Sdiv=subset(Sdiv,Site!="Veterans")
Sdiv=subset(Sdiv,Site!="Quintessa")
Sdiv=subset(Sdiv,Site!="Goode")
head(Sdiv)
Sdiv=as.data.frame(Sdiv)

Sdivplot=ggplot(Sdiv,aes(Before,Change))+geom_point()+geom_smooth(method='lm', se=FALSE)+ggtitle("Shannon diversity")+theme(plot.title = element_text(hjust = 0.5))
Sdivplot

Sdivlm=lm(formula=Before~Change,data=Sdiv)
summary(Sdivlm)

Sdivbeeabundlm=lm(formula=Sdiv$Before~bee.abund$Change)
summary(Sdivbeeabundlm)

Sdivbeedivlm=lm(formula=Sdiv$Before~bee.div$Change)
summary(Sdivbeedivlm)

Sdivplantdivlm=lm(formula=Sdiv$Before~plant.div$Change)
summary(Sdivplantdivlm)


# Subset for metric = P.annuals
P.annuals=subset(regressiondat,Metric=="P.annuals")
P.annuals=subset(P.annuals,Site!="Wappo")
P.annuals=subset(P.annuals,Site!="Veterans")
P.annuals=subset(P.annuals,Site!="Quintessa")
P.annuals=subset(P.annuals,Site!="Goode")
head(P.annuals)
P.annuals=as.data.frame(P.annuals)

P.annualsplot=ggplot(P.annuals,aes(Before,Change))+geom_point()+geom_smooth(method='lm', se=FALSE)+ggtitle("Percent annuals")+theme(plot.title = element_text(hjust = 0.5))
P.annualsplot

P.annualslm=lm(formula=Before~Change,data=P.annuals)
summary(P.annualslm)

P.annualsbeeabundlm=lm(formula=P.annuals$Before~bee.abund$Change)
summary(P.annualsbeeabundlm)

P.annualsbeedivlm=lm(formula=P.annuals$Before~bee.div$Change)
summary(P.annualsbeedivlm)

P.annualsplantdivlm=lm(formula=P.annuals$Before~plant.div$Change)
summary(P.annualsplantdivlm)


# Subset for metric = P.native
P.native=subset(regressiondat,Metric=="P.native")
P.native=subset(P.native,Site!="Wappo")
P.native=subset(P.native,Site!="Veterans")
P.native=subset(P.native,Site!="Quintessa")
P.native=subset(P.native,Site!="Goode")
head(P.native)
P.native=as.data.frame(P.native)

P.nativeplot=ggplot(P.native,aes(Before,Change))+geom_point()+geom_smooth(method='lm', se=FALSE)+ggtitle("Percent native")+theme(plot.title = element_text(hjust = 0.5))
P.nativeplot

P.nativelm=lm(formula=Before~Change,data=P.native)
summary(P.nativelm)

P.nativebeeabundlm=lm(formula=P.native$Before~bee.abund$Change)
summary(P.nativebeeabundlm)

P.nativebeedivlm=lm(formula=P.native$Before~bee.div$Change)
summary(P.nativebeedivlm)

P.nativeplantdivlm=lm(formula=P.native$Before~plant.div$Change)
summary(P.nativeplantdivlm)


# Subset for metric = P.invasive
P.invasive=subset(regressiondat,Metric=="P.invasive")
P.invasive=subset(P.invasive,Site!="Wappo")
P.invasive=subset(P.invasive,Site!="Veterans")
P.invasive=subset(P.invasive,Site!="Quintessa")
P.invasive=subset(P.invasive,Site!="Goode")
head(P.invasive)
P.invasive=as.data.frame(P.invasive)

P.invasiveplot=ggplot(P.invasive,aes(Before,Change))+geom_point()+geom_smooth(method='lm', se=FALSE)+ggtitle("Percent invasive")+theme(plot.title = element_text(hjust = 0.5))
P.invasiveplot

P.invasivelm=lm(formula=Before~Change,data=P.invasive)
summary(P.invasivelm)

P.invasivebeeabundlm=lm(formula=P.invasive$Before~bee.abund$Change)
summary(P.nativebeeabundlm)

P.invasivebeedivlm=lm(formula=P.invasive$Before~bee.div$Change)
summary(P.invasivebeedivlm)

P.invasiveplantdivlm=lm(formula=P.invasive$Before~plant.div$Change)
summary(P.invasiveplantdivlm)
