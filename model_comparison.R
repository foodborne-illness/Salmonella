################## Model Diagnostics ##################
library(glarma)
library(tidyverse)

source("data_processing.R")
source("modeling_Poisson.R")
source("modeling_NegBin.R")

# Lrtest and AIC
summary(glarmamod.P)
summary(glarmamod.NB)
likTests(glarmamod.P) # 3.58e-11 ***
likTests(glarmamod.NB) # 0.0001261 ***

# Other Dianostics
extractAIC.glarma(glarmamod.P) # 619.1898
rmse(glarmamod.P) # 2.112186
nrmse(glarmamod.P) # 0.005658644
extractAIC.glarma(glarmamod.NB) # 529.9099
rmse(glarmamod.NB) # 3.820704
nrmse(glarmamod.NB) # 0.07111145

par(mfrow = c(2,2))
# ACF
plot(glarmamod.P, which = 9,
     title = "ACF of Randomized Residuals (Poisson)") 
plot(glarmamod.NB, which = 9,
     title = "ACF of Randomized Residuals (NB)") 

# QQ plot
plot(glarmamod.P, which = 4,
     title = "QQ Plot of Pearson Residuals (Poisson)") 
plot(glarmamod.NB, which = 4,
     title = "QQ Plot of Pearson Residuals (NB)") 

par(mfrow = c(1,2))
# PIT
plot(glarmamod.P, which = 5,
     title = "Uniform PIT (Poisson)") 
plot(glarmamod.NB, which = 5,
     title = "Uniform PIT (NB)") 


# compare Poisson and NB
summary(glarmamod.NB)
coef(glarmamod.NB)$NB # this is the dispersion parameter
#Negative Binomial Parameter:
#  Estimate Std.Error z-ratio Pr(>|z|)    
#alpha    7.866     1.696   4.639 3.51e-06 ***

# calculate pvalue of lrtest
pchisq(2 * (logLik(glarmamod.NB) - logLik(glarmamod.P)),
       df = 1, lower.tail = FALSE) # 1.959117e-19

