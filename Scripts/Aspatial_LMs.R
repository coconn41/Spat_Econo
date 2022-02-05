source(paste(getwd(),"/Scripts/King_County_viz.R",sep=""))

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
