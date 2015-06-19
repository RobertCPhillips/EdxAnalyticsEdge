#The file climate_change.csv contains climate data from May 1983 to December 2008. The available variables include:
#  - Year: the observation year.
#  - Month: the observation month.
#  - Temp: the difference in degrees Celsius between the average global temperature in that period and a reference value. 
#  - CO2, N2O, CH4, CFC.11, CFC.12: atmospheric concentrations of carbon dioxide (CO2), nitrous oxide (N2O), methane  (CH4), trichlorofluoromethane (CCl3F; commonly referred to as CFC-11) and dichlorodifluoromethane (CCl2F2; commonly referred to as CFC-12), respectively. CO2, N2O and CH4 are expressed in ppmv (parts per million by volume  -- i.e., 397 ppmv of CO2 means that CO2 constitutes 397 millionths of the total volume of the atmosphere). CFC.11 and CFC.12 are expressed in ppbv (parts per billion by volume).
#  - Aerosols: the mean stratospheric aerosol optical depth at 550 nm. 
#  - TSI: the total solar irradiance (TSI) in W/m2 (the rate at which the sun's energy is deposited per unit area). 
#  - MEI: multivariate El Nino Southern Oscillation index (MEI), a measure of the strength of the El Nino/La Nina-Southern Oscillation (a weather effect in the Pacific Ocean that affects global temperatures). 

climate <- read.csv('climate_change.csv')
str(climate)

training <- subset(climate, Year <= 2006)
test <- subset(climate, Year > 2006)

# model using the dependent variable Temp and using MEI, CO2, CH4, N2O, CFC.11, 
# CFC.12, TSI, and Aerosols as independent variables
model1 <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, 
             data=training)
summary(model1)
cor(training)

#Given that the correlations are so high, build a model with only
# MEI, TSI, Aerosols and N2O as independent variables
model2 <- lm(Temp ~ MEI + N2O + TSI + Aerosols, 
             data=training)
summary(model2)

#find optimal model using AIC
model3 <- step(model1)
summary(model3)

predict1 <- predict(model3,test)
predict1.sse <- sum((test$Temp - predict1)^2)
predict1.sst <- sum((mean(training$Temp) - test$Temp)^2)
predict1.rsq <- 1 - predict1.sse/predict1.sst
