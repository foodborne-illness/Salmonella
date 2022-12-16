################## Model Selection and Prediction ##################
library(glarma)
library(tidyverse)

source("data_processing.R")

################## Final GLARMA model assuming Poisson ##################
# Fit glm for number of cases
glm.P <- glm(n ~ step.2013 + I(Jan + Feb + Mar) 
             + I(Apr + May + Jun + Jul + Aug) 
             + I(Sep + Oct + Nov) 
             + p.bAB  
             + p.gyAB 
             + p.ggAB 
             + p.snp1 
             + p.snp4
             + p.snp5 
             + p.snp6 
             + p.snp7  
             + p.snp9 
             + p.stress1 
             + Blood  
             + Feces,
             data = clinical.train, na.action = na.omit,  x = TRUE)

# Create design matrix X
X.P <- glm.P$x
Y.P <- clinical.train$n

# Fit glarma model
glarmamod.P <- glarma(Y.P, X.P, phiLags = c(1),
                      method = "FS", residuals = "Pearson")
summary(glarmamod.P)

# Model diagnostics
extractAIC.glarma(glarmamod.P) # 883.7013
rmse(glarmamod.P) # 2.630112
nrmse(glarmamod.P) # 0.03507392

########################## Fitted VS Obs Plot ############################
#plot(glarmamod.P, which = 1, ylim = c(0, 400), title = "Observed vs Fixed vs GLARMA (Poisson)") 

# Create fitted vs obs plot manually
plot.data <- data.frame(n = clinical.train$n)
plot.data$date <- with(clinical.train, as.Date(paste(year,month,'1',sep='-'), format='%Y-%m-%d'))
plot.data$glarmafit.poi <- fitted.glarma(glarmamod.P)
plot.data$fixed.poi <- exp(X.P %*% coef(glarmamod.P)$beta)
colors <- c("obs" = "black","fixed" = "blue", "glarmafit" = "red")
linetypes <- c("obs" = 2, "fixed" = 1,"glarmafit" = 1)
fitplot.P <- ggplot(plot.data) +
  geom_line(aes(date,n, color = "obs", linetype = "obs")) + 
  geom_line(aes(date,glarmafit.poi, color = "glarmafit", linetype = "glarmafit")) +
  geom_line(aes(date,fixed.poi, color = "fixed", linetype = "fixed")) + 
  labs(x = "Time (Year-Month)", y = "Clinical case", color = "", linetype = "",
       title = "Observed vs Fixed vs GLARMA (Poisson)") +
  scale_color_manual(values = colors) +
  scale_linetype_manual(values = linetypes) +
  theme_minimal() +
  theme(axis.line.y = element_line( color = "black", size = 0.5),
    axis.line.x = element_line( color = "black", size = 0.5),
    legend.position="top",
    text = element_text(size = 20),
    plot.title = element_text(hjust = 0.5)) 
fitplot.P

################################# FORECAST ################################
# Combine train and test dataset
clinical.test$Northeast<-0
clinical.test$Midwest <- 0
clinical.test$West <-0
clinical.test$Southwest<-0
clinical.test$Southeast<-0
clinical.all <- rbind(clinical.train, clinical.test)

# Modeling on the entire dataset for forecasting
glm.P.test <- glm(n ~ step.2013 + I(Jan + Feb + Mar) 
                  + I(Apr + May + Jun + Jul + Aug) 
                  + I(Sep + Oct + Nov) 
                  + p.bAB + p.gyAB + p.ggAB + p.snp1 
                  + p.snp4 + p.snp5 + p.snp6 + p.snp7  
                  + p.snp9 + p.stress1 + Blood + Feces,
             data = clinical.all, na.action = na.omit,  x = TRUE)

# Save some values
X.P.test <- glm.P.test$x
Y.P.test <- clinical.all$n
allX.P <- X.P.test
allFits.P <- fitted(glarmamod.P)

# Using actual values in forecasts
forecasts.P <- numeric(nrow(X.P.test))
for (i in (55:61)){
  y <- clinical.all$n[1:i]
  X <- glm.P.test[1:i]$x
  
  predmod.P <-  glarma(Y.P.test, X.P.test, phiLags = c(1),
                       method = "FS", residuals = "Pearson")
  
  XT1 <- matrix(allX.P[i + 1, ], nrow = 1)
  mu <- glarma::forecast(predmod.P, 1, XT1)$mu
  if (i == 55){
    forecasts.P[1:55] <- fitted(predmod.P)
  }
  forecasts.P[i+1] <- mu
}

# Plot for forecasting
preds.P <- ts(forecasts.P[55:61], start = c(2017, 9), deltat = 1/12)
fitted.P <- ts(allFits.P, start = c(2013, 1), deltat = 1/12)
obs <- ts(clinical.all$n, start = c(2013, 1), deltat = 1/12)
# prediction rMSE
sqrt(sum((as.vector(preds.P) - as.vector(obs)[55:61])^2) / sum(as.vector(obs)[55:61]))
# 3.604526


par(cex=1.4, cex.main=1.4, cex.lab = 1.2, cex.sub = 1.5)
plot(obs, ylab = "Clinical case", xlab = "Time (Year-Month)",
     lty = 2, bty="l", ylim = c(0,650),
     main = "Predictions with GLARMA (Poisson)")
lines(fitted.P)
lines(preds.P, col = "red")
grid(nx =11, ny=11)
par(xpd = NA)
graph.param <- legend("top", legend = c("observations", "fitted", "predicted"),
                      ncol = 3, cex = 0.7, bty = "n", plot = FALSE)
legend(graph.param$rect$left,
       graph.param$rect$top + graph.param$rect$h + graph.param$rect$w,
       legend =  c("observations", "fitted values", "predictions"), col = c("black","black","red"),
       lty = c(2,1,1), ncol = 3, cex = 0.7, bty = "n", text.font = 4)
par(xpd = FALSE)



