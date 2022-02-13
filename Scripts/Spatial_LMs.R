source(paste(getwd(),"/Scripts/Aspatial_LMs.R",sep=""))
library(spdep)
library(sp)
library(viridisLite)
library(tmaptools)
# model3.2 is final aspatial model
dat_w_resid = KingsCountyHomeSales_1_ %>%
  mutate(resids = model3.2$residuals) %>%
  st_as_sf(.,coords=c('longitude','latitude'))



tm_shape(Washington_Census_tracts,bbox=st_bbox(sf_salesdat))+
  tm_borders()+
  tm_shape(dat_w_resid)+
  tm_dots(col='resids',
          size=.005,
          breaks=c(-3,-2,-1,0,1,2),
          palette=get_brewer_pal('PiYG',n=5),legend.show=F)+
  tm_add_legend(type='symbol',col=get_brewer_pal('PiYG',n=5),
                labels=c("< -2.0",
                         "-2.0 to -1.0",
                         "-1.0 to 0.0",
                         "0.0 to 1.0",
                         "1.0 to 2.0"),
                shape=21)

m1 = KingsCountyHomeSales_1_[,12]
m2 = KingsCountyHomeSales_1_[,13]
m3 = cbind(m1,m2)
latlong = as.data.frame(x=m3,ncol=2)
xy=latlong[,c(1,2)]
latlong = SpatialPointsDataFrame(coords=xy,data=latlong)
knn5list = knearneigh(x=latlong, k=5, longlat=T)
knn5 = knn2nb(knn5list, sym = TRUE)
lm.morantest(model3.2,nb2listw(knn5))
test=localmoran(dat_w_resid$resids,listw = nb2listw(knn5))
dat_w_resid$Ii = test[,1]
dat_w_resid$E.Ii = test[,2]
dat_w_resid$Var.Ii = test[,3]
dat_w_resid$Z.Ii = test[,4]
dat_w_resid$pval = test[,5]

tm_shape(Washington_Census_tracts,bbox=st_bbox(sf_salesdat))+
  tm_borders()+
  tm_shape(dat_w_resid )+
  tm_dots(col='black')+
  tm_shape(dat_w_resid %>%
             filter(Ii > 0 & pval<0.05))+
  tm_dots(col='red')

#lm.LMtests(model,nb2listw(knn5), test=c('LMerr','LMlag','RMLerr','RLMlag','SARMA'))
spatmod1=lm.LMtests(model3.2,nb2listw(knn5), test=c('LMerr'))
summary(spatmod1)
spatmod2=lm.LMtests(model3.2,nb2listw(knn5), test=c('LMlag'))
summary(spatmod2)
