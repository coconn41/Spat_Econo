source(paste(getwd(),"/Scripts/Aspatial_LMs.R",sep=""))
library(spdep)
library(sp)
library(viridisLite)
library(tmaptools)
library(spatialreg)
library(lme4)
# model3.2 is final aspatial model
dat_w_resid = KingsCountyHomeSales_1_ %>%
  mutate(resids = model3.2$residuals) %>%
  st_as_sf(.,coords=c('longitude','latitude'))

dat_w_resid$perc = ifelse(dat_w_resid$resids<=quantile(dat_w_resid$resids,probs=c(.01,.05,.95,.99))[1],"1%",
                          ifelse(dat_w_resid$resids<=quantile(dat_w_resid$resids,probs=c(.01,.05,.95,.99))[2],"5%",
                                ifelse(dat_w_resid$resids>=quantile(dat_w_resid$resids,probs=c(.01,.05,.95,.99))[4],"99%",
                                       ifelse(dat_w_resid$resids>=quantile(dat_w_resid$resids,probs=c(.01,.05,.95,.99))[3],"95%","6%-94%"))))
tmap_mode(mode='view')
tm_shape(Washington_Census_tracts,bbox=st_bbox(sf_salesdat))+
  tm_borders()+
  tm_shape(dat_w_resid %>% filter(perc!="6%-94%"))+
  tm_dots(col='perc',
          size=.05,
          shape=20,
          palette=get_brewer_pal("RdBu",n=4))+
  tm_shape(dat_w_resid)+
  tm_dots(col='price')+
  tm_add_legend(type='symbol',
                shape=21)

  # Year built became ns
model3.3=lm(data = KingsCountyHomeSales_1_,
            formula = log(price) ~ bedrooms + bathrooms + sqft_living +
              sqft_lot + waterfront +
              as.factor(zipcode) + pop_estimate +
              median_age + LHS + HS + SC + BACH +prop_poverty)
summary(model3.3)
par(mfrow=c(2,2))
plot(model3.3)


m1 = KingsCountyHomeSales_1_[,12]
m2 = KingsCountyHomeSales_1_[,13]
m3 = cbind(m1,m2)
m3$latitude = m3$latitude + runif(n = nrow(m3),min = -.00001,max=.00001)
m3$longitude = m3$longitude + runif(n = nrow(m3),min = -.00001,max=.00001)
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

#tm_shape(Washington_Census_tracts,bbox=st_bbox(sf_salesdat))+
#  tm_borders()+
#  tm_shape(dat_w_resid )+
  # tm_dots(col='black')+
  # tm_shape(dat_w_resid %>%
  #            filter(Ii > 0 & pval<0.05))+
  # tm_dots(col='red')

# No ZIP
model3.4=lm(data = KingsCountyHomeSales_1_,
            formula = log(price) ~ bedrooms + bathrooms + sqft_living +
              sqft_lot + waterfront + pop_estimate +
              median_age + LHS + HS + SC + BACH +prop_poverty)
summary(model3.4)

#lm.LMtests(model,nb2listw(knn5), test=c('LMerr','LMlag','RMLerr','RLMlag','SARMA'))
spatmods_wzip=lm.LMtests(model3.3,nb2listw(knn5),
                         test='all')
spatmods_nozip=lm.LMtests(model3.4,nb2listw(knn5),
                          test='all')
summary(spatmods_wzip)
summary(spatmods_nozip)

# spatial error models

lagmod=spatialreg::lagsarlm(data = sf_salesdat,
         formula = log(price) ~ bedrooms + bathrooms + sqft_living +
           sqft_lot + waterfront + pop_estimate +
           median_age + LHS + HS + SC + BACH +prop_poverty,
         listw=nb2listw(knn5,style="W"),method='LU')
summary(lagmod)
# 


errormod=spatialreg::errorsarlm(data = sf_salesdat,
                              formula = log(price) ~ bedrooms + bathrooms + sqft_living +
                                sqft_lot + waterfront + pop_estimate +
                                median_age + LHS + HS + SC + BACH +prop_poverty,
                              listw=nb2listw(knn5,style="W"),method='LU')
summary(errormod)
# Residuals nearby, unexplained variance

KelegianModel=spatialreg::gstsls(data = sf_salesdat,
                                formula = log(price) ~ bedrooms + bathrooms + sqft_living +
                                  sqft_lot + waterfront + pop_estimate +
                                  median_age + LHS + HS + SC + BACH +prop_poverty,
                                listw=nb2listw(knn5,style="W"))
summary(KelegianModel)

# What do lambda and rho mean?
# Lag is capturing interaction between nearby interaction models
# Error is capturing spatial mismatch (mismatch in scale between way process is measured and actually occurring.)
# Captures heterogeneity in the process.
# Think that the error is looking at the entire spatial relationship of the model prediction
# While the error is looking at the relationship between dependent variables

intercepts_mod = lmer(data = sf_salesdat,
                      formula = log(price) ~ bedrooms + bathrooms + sqft_living +
                        sqft_lot + waterfront + pop_estimate +
                        median_age + LHS + HS + SC + BACH +prop_poverty+ (1|census_tract))
summary(intercepts_mod)
randoms=rownames_to_column(ranef(intercepts_mod)$census_tract,"census_tract")

sf_inter=merge(sf_salesdat,randoms,by='census_tract')

tm_shape(Washington_Census_tracts,bbox=st_bbox(sf_salesdat))+
  tm_borders()+
  tm_shape(sf_inter)+
  tm_dots(col='(Intercept)')



intercepts_mod1 = lmer(data = sf_salesdat,
                      formula = log(price) ~ bedrooms + bathrooms + sqft_living +
                        sqft_lot + waterfront + pop_estimate +
                        median_age + LHS + HS + SC + BACH +prop_poverty+ (1|census_tract) + (1|census_tract:sqft_lot))
summary(intercepts_mod1)

randoms=rownames_to_column(ranef(intercepts_mod1)$census_tract,"census_tract")
randoms1=rownames_to_column(ranef(intercepts_mod1)$`census_tract:sqft_lot`,"sqft_lot")
randoms1$census_tract=as.numeric(substring(randoms1$sqft_lot,1,11))
randoms1$sqft_lot=as.numeric(substring(randoms1$sqft_lot,13,16))
sf_inter = left_join(sf_salesdat,randoms1,by=c("census_tract",'sqft_lot'))

tm_shape(Washington_Census_tracts,bbox=st_bbox(sf_salesdat))+
  tm_borders()+
  tm_shape(sf_inter)+
  tm_dots(col='(Intercept)')

