##____________________________________________##
#___SCRIPT TO CLEAN DATA_____________________##
##____________________________________________##


# Load packages -----------------------------------------------------------

packs <- c("lubridate", "stringr", "ggplot2",'dplyr','tidyr','plotly','xlsx')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)

# load functions ---------------------------------------------------------------
source('1-code/Find_errors.R')


# load data ---------------------------------------------------------------


###ECOPALM 
###KNDE


knd=read.table(file = './0-data/Growth_Ecopalm_KNDE_cleaned.csv',header = T,sep=';',dec='.')

knd=knd%>%
    mutate(id=paste(TreeNb,NbLeaf,sep='_'),
           Progeny=as.factor(Progeny),
           NewLeafDate=dmy(NewLeafDate),
           PruningLeafDate=dmy(PruningLeafDate),
           DryLeafDate=dmy(DryLeafDate),
           BrokenLeafDate=dmy(BrokenLeafDate),
           HarvestingBunchDate=dmy(HarvestingBunchDate),
           SpatheOpeningDate=dmy(SpatheOpeningDate))
  
knd=knd%>%
  mutate(RachisWaterContent=(RacSampleFW-RacSampleDW)/RacSampleFW,
         RachisDW=RachisFW*(1-RachisWaterContent),
         PetioleWaterContent=(PetSampleFW-PetSampleDW)/PetSampleFW,
         PetioleDW=PetioleFW*(1-PetioleWaterContent),
         LeafletWaterContent=(LeafSampleFW-LeafSampleDW)/LeafSampleFW,
         LeafletDW=LeafletFW*(1-LeafletWaterContent),
         PeduncleWaterContent=(PedSampleFW-PedSampleDW)/PedSampleFW,
         PeduncleDW=PeduncleFW*(1-PeduncleWaterContent),
         SpikeletWaterContent=(SpiSampleFW-SpiSampleDW)/SpiSampleFW,
         SpikeletDW=SpikeletFW*(1-SpikeletWaterContent),
         ParthenoFruitWaterContent=(ParthenoSampleFW-ParthenoSampleDW)/ParthenoSampleFW,
         ParthenoFruitDW=ParthenoFruitFW*(1-ParthenoFruitWaterContent),
         FruitWaterContent=(FruitSampleFW-FruitSampleDW)/FruitSampleFW,
         FruitDW=FruitFW*(1-FruitWaterContent),
         BunchFW=PeduncleFW+SpikeletFW+ParthenoFruitFW+FruitFW,
         BunchDW=PeduncleDW+SpikeletDW+ParthenoFruitDW+FruitDW)

str(knd)
summary(knd)




####BMLE
bmle=read.table(file = './0-data/Growth_Ecopalm_BMLE_cleaned.csv',header = T,sep=';',dec='.')
bmle=bmle%>%
  mutate(id=paste(TreeNb,NbLeaf,sep='_'),
         Progeny=as.factor(Progeny),
         NewLeafDate=dmy(NewLeafDate),
         PruningLeafDate=dmy(PruningLeafDate),
         DryLeafDate=dmy(DryLeafDate),
         BrokenLeafDate=dmy(BrokenLeafDate),
         HarvestingBunchDate=dmy(HarvestingBunchDate),
         SpatheOpeningDate=dmy(SpatheOpeningDate))

bmle=bmle%>%
  mutate(RachisWaterContent=(RacSampleFW-RacSampleDW)/RacSampleFW,
         RachisDW=RachisFW*(1-RachisWaterContent),
         PetioleWaterContent=(PetSampleFW-PetSampleDW)/PetSampleFW,
         PetioleDW=PetioleFW*(1-PetioleWaterContent),
         LeafletWaterContent=(LeafSampleFW-LeafSampleDW)/LeafSampleFW,
         LeafletDW=LeafletFW*(1-LeafletWaterContent),
         PeduncleWaterContent=(PedSampleFW-PedSampleDW)/PedSampleFW,
         PeduncleDW=PeduncleFW*(1-PeduncleWaterContent),
         SpikeletWaterContent=(SpiSampleFW-SpiSampleDW)/SpiSampleFW,
         SpikeletDW=SpikeletFW*(1-SpikeletWaterContent),
         ParthenoFruitWaterContent=(ParthenoSampleFW-ParthenoSampleDW)/ParthenoSampleFW,
         ParthenoFruitDW=ParthenoFruitFW*(1-ParthenoFruitWaterContent),
         FruitWaterContent=(FruitSampleFW-FruitSampleDW)/FruitSampleFW,
         FruitDW=FruitFW*(1-FruitWaterContent),
         BunchFW=PeduncleFW+SpikeletFW+ParthenoFruitFW+FruitFW,
         BunchDW=PeduncleDW+SpikeletDW+ParthenoFruitDW+FruitDW)

str(bmle)
summary(bmle)



####PHLE
phle=read.table(file = './0-data/Growth_Ecopalm_PHLE_cleaned.csv',header = T,sep=';',dec='.')
phle=phle%>%
  mutate(id=paste(TreeNb,NbLeaf,sep='_'),
         Progeny=as.factor(Progeny),
         NewLeafDate=dmy(NewLeafDate),
         PruningLeafDate=dmy(PruningLeafDate),
         DryLeafDate=dmy(DryLeafDate),
         BrokenLeafDate=dmy(BrokenLeafDate),
         HarvestingBunchDate=dmy(HarvestingBunchDate),
         SpatheOpeningDate=dmy(SpatheOpeningDate))



phle=phle%>%
  mutate(RachisWaterContent=(RacSampleFW-RacSampleDW)/RacSampleFW,
         RachisDW=RachisFW*(1-RachisWaterContent),
         LeafletWaterContent=(LeafSampleFW-LeafSampleDW)/LeafSampleFW,
         LeafletDW=LeafletFW*(1-LeafletWaterContent),
         PeduncleWaterContent=(PedSampleFW-PedSampleDW)/PedSampleFW,
         PeduncleDW=PeduncleFW*(1-PeduncleWaterContent),
         SpikeletWaterContent=(SpiSampleFW-SpiSampleDW)/SpiSampleFW,
         SpikeletDW=SpikeletFW*(1-SpikeletWaterContent),
         ParthenoFruitWaterContent=(ParthenoSampleFW-ParthenoSampleDW)/ParthenoSampleFW,
         ParthenoFruitDW=ParthenoFruitFW*(1-ParthenoFruitWaterContent),
         FruitWaterContent=(FruitSampleFW-FruitSampleDW)/FruitSampleFW,
         FruitDW=FruitFW*(1-FruitWaterContent),
         BunchFW=PeduncleFW+SpikeletFW+ParthenoFruitFW+FruitFW,
         BunchDW=PeduncleDW+SpikeletDW+ParthenoFruitDW+FruitDW)


str(phle)
summary(phle)



###  ATP reserves
atp=read.table(file = './0-data/Growth_ATP_KNDE_cleaned.csv',header = T,sep=';',dec='.')
atp=atp%>%
  mutate(id=paste(TreeNb,NbLeaf,sep='_'),
         Progeny=as.factor(Progeny),
         NewLeafDate=dmy(NewLeafDate),
         PruningLeafDate=dmy(PruningLeafDate),
         DryLeafDate=dmy(DryLeafDate),
         BrokenLeafDate=dmy(BrokenLeafDate),
         HarvestingBunchDate=dmy(HarvestingBunchDate),
         SpatheOpeningDate=dmy(SpatheOpeningDate))

atp=atp%>%
  mutate(RachisWaterContent=(RacSampleFW-RacSampleDW)/RacSampleFW,
         RachisDW=RachisFW*(1-RachisWaterContent),
         LeafletWaterContent=(LeafSampleFW-LeafSampleDW)/LeafSampleFW,
         LeafletDW=LeafletFW*(1-LeafletWaterContent),
         PeduncleWaterContent=(PedSampleFW-PedSampleDW)/PedSampleFW,
         PeduncleDW=PeduncleFW*(1-PeduncleWaterContent),
         SpikeletWaterContent=(SpiSampleFW-SpiSampleDW)/SpiSampleFW,
         SpikeletDW=SpikeletFW*(1-SpikeletWaterContent),
         ParthenoFruitWaterContent=(ParthenoSampleFW-ParthenoSampleDW)/ParthenoSampleFW,
         ParthenoFruitDW=ParthenoFruitFW*(1-ParthenoFruitWaterContent),
         FruitWaterContent=(FruitSampleFW-FruitSampleDW)/FruitSampleFW,
         FruitDW=FruitFW*(1-FruitWaterContent),
         BunchFW=PeduncleFW+SpikeletFW+ParthenoFruitFW+FruitFW,
         BunchDW=PeduncleDW+SpikeletDW+ParthenoFruitDW+FruitDW)


str(atp)
summary(atp)


# var='Petiole.Length'

# var='Petiole.FW'
# var='Petiole.Water.Content'

# var='Rachis.Length'
# var='Rachis.FW'

# var='Leaflet.Nb.Rigth'
# var='Leaflet.Nb.Left'
# var='Leaflet.FW'
# var='Leaflet.Water.Content'

# var='Peduncle.FW'
# var='Peduncle.Water.Content'

# var='Spikelet.FW'
# var='Spikelet.Water.Content'

# var='Partheno.Fruit.Nb'
# var='Partheno.Fruit.FW'
# var='Partheno.Fruit.Water.Content'

# var='Fruit.Nb'
# var='Fruit.FW'
# var='Fruit.Water.Content'


# var='Bunch.DW.cor'



# find outliers -----------------------------------------------------------

outlier=find_outliers(data = knd,y_var =var,x_var='Nb.Leaf')

res=merge(knd,outlier,all.x=T)

gr=ggplot(data=knd,aes_string(x='Nb.Leaf',y=var,group='TreeNb',col='Progeny'))+
  geom_line()+
  geom_point(size=0.5)+
  geom_point(data=res[!is.na(res$type)& res$type=='outlier',],aes_string('Nb.Leaf',y='value',group='TreeNb'),col=2,pch=0)+
  facet_grid(Treatment~.)


ggplotly(gr)









# box=knd%>%
#   ggplot(aes_string(x='Progeny',y=var,col='Progeny'))+
#   geom_boxplot()+
#   facet_grid(Treatment~.)
# 
# 
# ggplotly(box)

###ATP
# atpC=read.xlsx(file = './Data/ATP_reserves_raw/proto_S_growth_def_control_oil_palm_march10.xls',header = T,sheetIndex = 1)
# 
# atpC=atpC%>%
#   mutate(id=paste(TreeNb,NbLeaf,sep='_'))
# 
# test=atpC%>%
#   filter(!(id %in% unique(knd$id)))
# 
# unique(test$id)

# atpF=read.xlsx(file = './Data/ATP_reserves_raw/proto_S_growth_def_FPT_oil_palm_march10_corrected.xls',header = T,sheetIndex = 1)
# 
# atpF=atpF%>%
#   mutate(id=paste(TreeNb,NbLeaf,sep='_'))


