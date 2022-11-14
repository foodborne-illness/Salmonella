########################## PHP 2550 Method Implementation ##########################
library(ggplot2)
library(gsarima)
library(tidyverse)
library(astsa)
library(VGAM)

# import data
data <- read.csv("metadata.csv")
# drop four incorrectly duplicate records
project.data <- data %>%
  filter(!(X %in% c("15794","83086","147695","202903")))
# obtain vector of all locations that include USA
str.USA <- unique(project.data$location)[grep("USA", unique(project.data$location), ignore.case = T)]
# subset data to only USA
data.USA <- project.data %>% 
  filter(location %in% str.USA)

# data preprocessing
data.USA.tidy <- data.USA %>% 
  filter(isolation.type == "clinical") %>% # filter with only clinical 
  filter(serovar %in% c("enteritidis", "Enteritidis"))  %>% # serovar of interest
  filter(as.numeric(substr(collection.date,1,4)) >= 2010) %>% # filter cases after 2010 
  mutate(year = substr(collection.date,1,4), month = substr(collection.date,6,7), 
         day = substr(collection.date,9,10)) %>%
  #mutate(time = ifelse(is.na(month), NA, sprintf("%d-%02d", year, month))) %>%
  mutate(time = ifelse(month == "", NA, zoo::as.yearmon(paste(year, month), "%Y %m")))

# summarize enteritidis cases by year and month
enteritidis.case <- data.USA.tidy %>%
  filter(!is.na(time)) %>%
  group_by(time) %>%
  summarise(count = n())

# create time series plot
tsplot <- ggplot(enteritidis.case, aes(x=time, y=count)) +
  geom_line() +
  labs(title = "Exposure of Salmonella Enteritidis Cases from 2010 to 2020", 
       x = "Year-Month", y = "Enteritidis")

# create time series dataset
ts.enteritidis.case <- ts(enteritidis.case$count, freq=1) 

# ARIMA model
(arima.res <- sarima(ts.enteritidis.case, p=1, d=1, q=0))
summary(arima.res$fit)

# Model Diagnostics
sarima(ts.enteritidis.case, p=1, d=1, q=0, gg=TRUE, col=4)

# Generalized ARMA Model
garma.res <- vglm(count ~ 1, trace = TRUE, data = enteritidis.case,
                  garma(link = "loglink", p = 2, coefstart = c(4, 0.3, 0.4)))
summary(garma.res)
coef(garma.res, matrix = TRUE)

# Fitted vs Predicted
with(enteritidis.case, plot(count, las = 1, xlab = "", ylab = "", col = "blue"))
with(enteritidis.case, lines(time[-(1:garma.res@misc$plag)], fitted(garma.res), col = "orange"))
abline(h = mean(with(enteritidis.case, count)), lty = "dashed", col = "gray") 


