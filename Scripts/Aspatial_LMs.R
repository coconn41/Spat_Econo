source(paste(getwd(),"/Scripts/King_County_viz.R",sep=""))
library(readr)
KingsCountyHomeSales_1_ <- read_csv("Data/KingsCountyHomeSales (1).csv")
DataDictionary <- read_csv("Data/DataDictionary.csv")
# Model of just house related variables
model1=lm(data = KingsCountyHomeSales_1_,
   formula = price ~ bedrooms + bathrooms + sqft_living +
     sqft_lot + floors + waterfront + yr_built )
summary(model1)
# All are significant: Adjusted R2 = .5856
par(mfrow=c(2,2))
plot(model1,main="Property related variables only")
# Model of just location related variables
model2.1=lm(data = KingsCountyHomeSales_1_,
          formula = price ~ as.factor(zipcode) + pop_estimate +
            median_age + LHS + HS + SC + BACH +
            GRAD + prop_poverty)
summary(model2.1)
# Remove GRAD
model2.2=lm(data = KingsCountyHomeSales_1_,
            formula = price ~ as.factor(zipcode) + pop_estimate +
              median_age + LHS + HS + SC + BACH + prop_poverty)
summary(model2.2)
# All remaining are significant: Adjusted R2 = 0.4476
par(mfrow=c(2,2))
plot(model2.2,main="Location related variables only")

# Full model - step 1

model3=lm(data = KingsCountyHomeSales_1_,
          formula = price ~ bedrooms + bathrooms + sqft_living +
            sqft_lot + floors + waterfront + yr_built +
            as.factor(zipcode) + pop_estimate +
            median_age + LHS + HS + SC + BACH +
            GRAD + prop_poverty)
summary(model3)

# Remove prop_poverty

model3.1=lm(data = KingsCountyHomeSales_1_,
          formula = price ~ bedrooms + bathrooms + sqft_living +
            sqft_lot + floors + waterfront + yr_built +
            as.factor(zipcode) + pop_estimate +
            median_age + LHS + HS + SC + BACH +
            GRAD )
summary(model3.1)

# Remove GRAD

model3.2=lm(data = KingsCountyHomeSales_1_,
            formula = price ~ bedrooms + bathrooms + sqft_living +
              sqft_lot + floors + waterfront + yr_built +
              as.factor(zipcode) + pop_estimate +
              median_age + LHS + HS + SC + BACH)
summary(model3.2)

# Remove floors

model3.3=lm(data = KingsCountyHomeSales_1_,
            formula = price ~ bedrooms + bathrooms + sqft_living +
              sqft_lot + waterfront + yr_built +
              as.factor(zipcode) + pop_estimate +
              median_age + LHS + HS + SC + BACH)
summary(model3.3)

# Adjusted R2 = .7828

par(mfrow=c(2,2))
plot(model3.3,main = "Full model")

#Recurring highly influential residual for all models, investigate:
# These values are all fit to be negatige. Why?
subset(model3.3$fitted.values,model3.3$fitted.values<0)

#Highly influential value is obs 15868
KingsCountyHomeSales_1_[15868,]
# 33 bathrooms? Hmmmm
hist(KingsCountyHomeSales_1_$bedrooms)
#should be 3 bathrooms. Recode:
KingsCountyHomeSales_1_[15868,4]=3

model3.4=lm(data = KingsCountyHomeSales_1_,
            formula = price ~ bedrooms + bathrooms + sqft_living +
              sqft_lot + waterfront + yr_built +
              as.factor(zipcode) + pop_estimate +
              median_age + LHS + HS + SC + BACH)
summary(model3.4)

# Improved Full model R2 to .7833
# Improved property only model to .5871

#Investigate residuals
resid=subset(model3.3$residuals,model3.3$residuals>2e6)
ind=1
for(i in names(resid)){
  if(ind==1){a = KingsCountyHomeSales_1_[i,]}
  if(ind>1){b=KingsCountyHomeSales_1_[i,]
  a=bind_rows(a,b)}
  ind=ind+1
}

tm_shape(Washington_Census_tracts,bbox=st_bbox(sf_salesdat))+
  tm_borders()+
tm_shape(sf_salesdat)+
  tm_dots(alpha=.3)+
tm_shape(sf_salesdat %>% filter(id%in%a$id))+
  tm_dots(col='red',size=2)

# North end?