
#diviser les donn?es en train et test (al?atoire)
install.packages("caret")
install.packages("pillar")
library(caret)
# Cr?er les indices de partitionnement
train_indexes1 <- createDataPartition(dataset1dif1, p = 0.8, list = FALSE)
train_indexes2 <- createDataPartition(dataset2dif1, p = 0.8, list = FALSE)
# Cr?er les ensembles d'entra?nement et de test
train1 <- dataset1dif1[train_indexes1]
test1 <- dataset1dif1[-train_indexes1]
train2 <- dataset2dif1[train_indexes2]
test2 <- dataset2dif1[-train_indexes2]

#mod?le arima
#periode 1
modelARIMA1=arima(train1,order=c(1,1,2))
modelARIMA2=arima(train2,order=c(0,1,1))
install.packages("forecast")
library(forecast)
# validation du mod?le ARIMA1
# analyse des r?sidus
dev.new()
checkresiduals(modelARIMA1) 
residualsARIMA1=residuals(modelARIMA1)
plot(residualsARIMA1)
modelARIMA1 
# Tester la capacit? de pr?diction
fcARIMA1=forecast(modelARIMA1,263)
errorsARIMA1 <- test1 - fcARIMA1$mean
mseARIMA1 <- mean(errorsARIMA1^2)  # Mean Squared Error
rmseARIMA1 <- sqrt(mseARIMA1)      # Root Mean Squared Error

# validation du mod?le ARIMA2
# analyse des r?sidus
dev.new()
checkresiduals(modelARIMA2) 
residualsARIMA2=residuals(modelARIMA2)
plot(residualsARIMA2)
modelARIMA2
# Tester la capacit? de pr?diction
fcARIMA2=forecast(modelARIMA2,80)
errorsARIMA2 <- test2 - fcARIMA2$mean
mseARIMA2 <- mean(errorsARIMA2^2)  # Mean Squared Error
rmseARIMA2 <- sqrt(mseARIMA2)      # Root Mean Squared Error

