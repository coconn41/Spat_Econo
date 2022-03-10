source(paste(getwd(),"/Scripts/Spatial_LMs.R",sep=""))
library(mgcv)
# Gam using mgcv

# Zuur chapter 3.3 technical details of fitting GAM in mgcv

# gam(y - s(x), data = whatever)

g1=gam(data = sf_salesdat,
       formula = log(price) ~ s(bedrooms) + s(bathrooms) + s(sqft_living) +
         s(sqft_lot) + waterfront + s(pop_estimate) +
         s(median_age) + s(LHS) + s(HS) + s(SC) + s(BACH) +s(prop_poverty))
summary(g1)
plot(g1)
# This gave an error
# It requires a lot of degrees of freedom?

# What variable might you be interested in the spline of?

# Test out the lat/long spline
sf_salesdat$longitude=st_coordinates(sf_salesdat)[,1]
sf_salesdat$latitude=st_coordinates(sf_salesdat)[,2]
g2=gam(data = sf_salesdat,
       formula = log(price) ~ s(longitude,latitude))
summary(g2)
plot(g2)

g3=gam(data=sf_salesdat,
       formula=log(price) ~ s(longitude,latitude) + bedrooms + bathrooms + sqft_living +
         sqft_lot + waterfront + pop_estimate +
         median_age + LHS + HS + SC + BACH +prop_poverty)
summary(g3)
plot(g3)

# This has a pretty low AIC, try making it a mixed model

g4=gam(data=sf_salesdat,
       formula=log(price) ~ s(longitude,latitude) + bedrooms + bathrooms + sqft_living +
         sqft_lot + waterfront + pop_estimate +
         median_age + LHS + HS + SC + BACH +prop_poverty + s(census_tract,bs='re'))
summary(g4)
#How to add mixed effects?
m1_gam <- gam(response ~ treatment:transf_time +
                s(subject, bs = 're') +
                s(subject, transf_time, bs = 're'),
              data = rats, method = 'REML')

m2_gam=gam(log(price) ~  bedrooms + bathrooms + sqft_living + s(census_tract, bs = 're'),data=sf_salesdat)
summary(m2_gam)

coef(m2_gam)
plot(m2_gam)

gam.vcomp(m2_gam) 

gam.check(m2_gam)

#https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/