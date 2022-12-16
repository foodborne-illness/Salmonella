################## Model Selection and Prediction ##################
library(glarma)
library(tidyverse)

source("data_processing.R")

################## Final GLARMA model assuming NB ##################
# Fit glm for number of cases
glm.NB <- glm(n ~ step.2013 + I(Jan + Feb + Mar) 
              + I(Apr + May + Jun + Jul + Aug) 
              + I(Sep + Oct + Nov) 
               + p.gyAB 
               + p.snp3
               + p.snp5 
               + p.snp6 
               + p.snp7 
               + p.stress2 
               + Northeast 
               + Blood,
              data = clinical.train, na.action = na.omit, x = TRUE)

# Create design matrix X
X.NB <- glm.NB$x
Y.NB <- clinical.train$n

# Fit glarma model
glarmamod.NB <- glarma(Y.NB, X.NB, phiLags = c(2), type = "NegBin", 
                       method = "FS", residuals = "Pearson")
summary(glarmamod.NB)

# Model diagnostics
extractAIC.glarma(glarmamod.NB) # 529.9099
rmse(glarmamod.NB) # 3.820704
nrmse(glarmamod.NB) # 0.07111145

########################## Fitted VS Obs Plot ############################
plot(glarmamod.NB, which = 1, ylim = c(0, 400), title = "Observed vs Fixed vs GLARMA (NB)") 

 # Create fitted vs obs plot manually
plot.data <- data.frame(n = clinical.train$n)
plot.data$date <- with(clinical.train, as.Date(paste(year,month,'1',sep='-'), format='%Y-%m-%d'))
plot.data$glarmafit.nb <- fitted.glarma(glarmamod.NB)
plot.data$fixed.nb <- exp(X.NB %*% coef(glarmamod.NB)$beta)
colors <- c("obs" = "black","fixed" = "blue", "glarmafit" = "red")
linetypes <- c("obs" = 2, "fixed" = 1,"glarmafit" = 1)
fitplot.NB <- ggplot(plot.data) +
  geom_line(aes(date,n, color = "obs", linetype = "obs")) + 
  geom_line(aes(date,glarmafit.nb, color = "glarmafit", linetype = "glarmafit")) +
  geom_line(aes(date,fixed.nb, color = "fixed", linetype = "fixed")) + 
  labs(x = "Time (Year-Month)", y = "Clinical case", color = "", linetype = "",
       title = "Observed vs Fixed vs GLARMA (NB)") +
  scale_color_manual(values = colors) +
  scale_linetype_manual(values = linetypes) +
  theme_minimal() +
  theme(axis.line.y = element_line( color = "black", size = 0.5),
        axis.line.x = element_line( color = "black", size = 0.5),
        legend.position="top",
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5)) 
fitplot.NB

################################# FORECAST ################################
# Combine train and test dataset
clinical.test$Northeast<-0
clinical.test$Midwest <- 0
clinical.test$West <-0
clinical.test$Southwest<-0
clinical.test$Southeast<-0
clinical.all <- rbind(clinical.train, clinical.test)

# Modeling on the entire dataset for forecasting
glm.NB.test <- glm(n ~ step.2013 + I(Jan + Feb + Mar) 
                   + I(Apr + May + Jun + Jul + Aug) 
                   + I(Sep + Oct + Nov) 
                   + p.gyAB 
                   + p.snp3
                   + p.snp5 
                   + p.snp6 
                   + p.snp7 
                   + p.stress2 
                   + Northeast 
                   + Blood,
                   data = clinical.all, na.action = na.omit, x = TRUE)

# Save some values
X.NB.test <- glm.NB.test$x
Y.NB.test <- clinical.all$n
allX.NB <- X.NB.test
allFits.NB <- fitted(glarmamod.NB)

# Using actual values in forecasts
forecasts.NB <- numeric(nrow(X.NB.test))
for (i in (55:61)){
  y <- clinical.all$n[1:i]
  X <- glm.NB.test[1:i]$x
  
  predmod.NB <- glarma(Y.NB.test, X.NB.test, phiLags = c(2), type = "NegBin", 
                       method = "FS", residuals = "Pearson")
  
  XT1 <- matrix(allX.NB[i + 1, ], nrow = 1)
  mu <- glarma::forecast(predmod.NB, 1, XT1)$mu
  if (i == 55){
    forecasts.NB[1:55] <- fitted(predmod.NB)
  }
  forecasts.NB[i+1] <- mu
}

# Plot for forecasting
preds.NB <- ts(forecasts.NB[55:61], start = c(2017, 9), deltat = 1/12)
fitted.NB <- ts(allFits.NB, start = c(2013, 1), deltat = 1/12)
obs <- ts(clinical.all$n, start = c(2013, 1), deltat = 1/12)
# prediction rMSE
sqrt(sum((as.vector(preds.NB) - as.vector(obs)[55:61])^2) / sum(as.vector(obs)[55:61]))
# 8.551664

par(cex=1.4, cex.main=1.4, cex.lab = 1.2, cex.sub = 1.5)
plot(obs, ylab = "Clinical case", xlab = "Time (Year-Month)",
     lty = 2, bty="l", ylim = c(0,650),
     main = "Predictions with GLARMA (NB)")
lines(fitted.NB)
lines(preds.NB, col = "red")
grid(nx =11, ny=11)
par(xpd = NA)
graph.param <- legend("top", legend = c("observations", "fitted", "predicted"),
                      ncol = 3, cex = 0.7, bty = "n", plot = FALSE)
legend(graph.param$rect$left,
       graph.param$rect$top + graph.param$rect$h + graph.param$rect$w,
       legend =  c("observations", "fitted values", "predictions"), col = c("black","black","red"),
       lty = c(2,1,1), ncol = 3, cex = 0.7, bty = "n", text.font = 4)
par(xpd = FALSE)

