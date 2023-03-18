rm(list=ls())
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
library(caret)
library(performance)
library(pscl)  #penggunaan pr2




data <- Data_Contoh_Multinomial
str(data)
data$StatusMerokok <- relevel(data$StatusMerokok, ref = "1")
m <- multinom(StatusMerokok ~ ., data = data)
summary(m)
z <- summary(m)$coefficients/summary(m)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
multicollinearity(m)
model_performance(m,metrics = "all")
pR2(m)
pStatusMerokok = predict(m,data[,-1])
tableses<- table(data$StatusMerokok, pStatusMerokok)
confusionMatrix(tableses)
(OR=exp(coef(m)))


