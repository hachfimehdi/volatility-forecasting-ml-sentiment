install.packages("TSstudio")
library(TSstudio)
library(dplyr)
library(readxl)
library(FinTS)
library(caret)
library(glmnet)
library(forecast)

DataPeriode1 <- read_excel("C:/Users/hp/Desktop/M2 ERCF/Mémoire/Données/DataPeriode1.xlsx")
DataPeriode2 <- read_excel("C:/Users/hp/Desktop/M2 ERCF/Mémoire/Données/DataPeriode2.xlsx")

DataPeriode1$DividendGrowthRate=DataPeriode1$DividendGrowthRate*100
DataPeriode1$PolicyRate=DataPeriode1$PolicyRate*100
DataPeriode1$InterestRate=DataPeriode1$InterestRate*100
DataPeriode1$Yield=DataPeriode1$Yield*100
DataPeriode1$Volatility=DataPeriode1$Volatility*100
DataPeriode2$DividendGrowthRate=DataPeriode2$DividendGrowthRate*100
DataPeriode2$PolicyRate=DataPeriode2$PolicyRate*100
DataPeriode2$InterestRate=DataPeriode2$InterestRate*100
DataPeriode2$Yield=DataPeriode2$Yield*100
DataPeriode2$Volatility=DataPeriode2$Volatility*100

head(DataPeriode1)
head(DataPeriode2)

time_index1 <- 1:nrow(DataPeriode1) #nombre des mois dans la période 1
time_index2 <- 1:nrow(DataPeriode2) #nombre des mois dans la période 2

#Traçage du rendement
dev.new()
par(mfrow = c(2, 1))  # Diviser la fenêtre en 2 lignes, 1 colonne
min_yield <- min(DataPeriode1$Yield, DataPeriode2$Yield)
max_yield <- max(DataPeriode1$Yield, DataPeriode2$Yield)

# Tracer la première période (1881-1990) en bleu (en haut)
plot(time_index1, DataPeriode1$Yield, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Rendement (%)",
     main = "Évolution du rendement (Janvier 1881 - Décembre 1990)",
     xaxt = "n",ylim = c(min_yield, max_yield))  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (années)
years1 <- seq(1881, 1990, by = 10)  # Générer des années tous les 20 ans
year_indices1 <- seq(1, nrow(DataPeriode1), length.out = length(years1))  # Correspond aux indices
axis(1, at = year_indices1, labels = years1)

# Tracer la deuxième période (1991-2024) en bleu (en bas)
plot(time_index2, DataPeriode2$Yield, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Rendement (%)",
     main = "Évolution du rendement (Janvier 1991 - Septembre 2024)",
     xaxt = "n",ylim = c(min_yield, max_yield))  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (années)
years2 <- seq(1990, 2024 , by = 10)  # Générer des années tous les 20 ans
year_indices2 <- seq(1, nrow(DataPeriode2), length.out = length(years2))  # Correspond aux indices
axis(1, at = year_indices2, labels = years2)

# Réinitialiser les paramètres de la fenêtre graphique
par(mfrow = c(1, 1))



#Traçage du taux d'intérêt
dev.new()
par(mfrow = c(2, 1))  # Diviser la fenêtre en 2 lignes, 1 colonne
min_InterestRate <- min(DataPeriode1$InterestRate, DataPeriode2$InterestRate)
max_InterestRate <- max(DataPeriode1$InterestRate, DataPeriode2$InterestRate)

# Tracer la première période (1881-1990) en bleu (en haut)
plot(time_index1, DataPeriode1$InterestRate, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Taux d'intérêt (%)",
     main = "Évolution du taux d'intérêt (Janvier 1881 - Décembre 1990)",
     xaxt = "n",ylim = c(min_InterestRate, max_InterestRate))  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (années)
years1 <- seq(1881, 1990, by = 10)  # Générer des années tous les 20 ans
year_indices1 <- seq(1, nrow(DataPeriode1), length.out = length(years1))  # Correspond aux indices
axis(1, at = year_indices1, labels = years1)

# Tracer la deuxième période (1991-2024) en bleu (en bas)
plot(time_index2, DataPeriode2$InterestRate, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Taux d'intérêt (%)",
     main = "Évolution du taux d'intérêt (Janvier 1991 - Septembre 2024)",
     xaxt = "n",ylim = c(min_InterestRate, max_InterestRate))  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (années)
years2 <- seq(1990, 2024 , by = 10)  # Générer des années tous les 20 ans
year_indices2 <- seq(1, nrow(DataPeriode2), length.out = length(years2))  # Correspond aux indices
axis(1, at = year_indices2, labels = years2)

# Réinitialiser les paramètres de la fenêtre graphique
par(mfrow = c(1, 1))



#Traçage du taux directeur
dev.new()
par(mfrow = c(2, 1))  # Diviser la fenêtre en 2 lignes, 1 colonne
min_PolicyRate <- min(DataPeriode1$PolicyRate, DataPeriode2$PolicyRate)
max_PolicyRate <- max(DataPeriode1$PolicyRate, DataPeriode2$PolicyRate)

# Tracer la première période (1881-1990) en bleu (en haut)
plot(time_index1, DataPeriode1$PolicyRate, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Taux directeur (%)",
     main = "Évolution du taux directeur (Janvier 1881 - Décembre 1990)",
     xaxt = "n",ylim = c(min_PolicyRate, max_PolicyRate))  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (années)
years1 <- seq(1881, 1990, by = 10)  # Générer des années tous les 20 ans
year_indices1 <- seq(1, nrow(DataPeriode1), length.out = length(years1))  # Correspond aux indices
axis(1, at = year_indices1, labels = years1)

# Tracer la deuxième période (1991-2024) en bleu (en bas)
plot(time_index2, DataPeriode2$PolicyRate, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Taux directeur (%)",
     main = "Évolution du taux directeur (Janvier 1991 - Septembre 2024)",
     xaxt = "n",ylim = c(min_PolicyRate, max_PolicyRate))  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (années)
years2 <- seq(1990, 2024 , by = 10)  # Générer des années tous les 20 ans
year_indices2 <- seq(1, nrow(DataPeriode2), length.out = length(years2))  # Correspond aux indices
axis(1, at = year_indices2, labels = years2)

# Réinitialiser les paramètres de la fenêtre graphique
par(mfrow = c(1, 1))



#Traçage du taux de croissance de dividende
dev.new()
par(mfrow = c(2, 1))  # Diviser la fenêtre en 2 lignes, 1 colonne
min_DividendGrowthRate <- min(DataPeriode1$DividendGrowthRate, DataPeriode2$DividendGrowthRate)
max_DividendGrowthRate <- max(DataPeriode1$DividendGrowthRate, DataPeriode2$DividendGrowthRate)

# Tracer la première période (1881-1990) en bleu (en haut)
plot(time_index1, DataPeriode1$DividendGrowthRate, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Taux de croissance de dividende (%)",
     main = "Évolution du taux de croissance de dividende 
     (Janvier 1881 - Décembre 1990)",
     xaxt = "n",ylim = c(min_DividendGrowthRate, max_DividendGrowthRate))  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (années)
years1 <- seq(1881, 1990, by = 10)  # Générer des années tous les 20 ans
year_indices1 <- seq(1, nrow(DataPeriode1), length.out = length(years1))  # Correspond aux indices
axis(1, at = year_indices1, labels = years1)

# Tracer la deuxième période (1991-2024) en bleu (en bas)
plot(time_index2, DataPeriode2$DividendGrowthRate, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Taux de croissance de dividende (%)",
     main = "Évolution du taux de croissance de dividende 
     (Janvier 1991 - Septembre 2024)",
     xaxt = "n",ylim = c(min_DividendGrowthRate, max_DividendGrowthRate))  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (années)
years2 <- seq(1990, 2024 , by = 10)  # Générer des années tous les 20 ans
year_indices2 <- seq(1, nrow(DataPeriode2), length.out = length(years2))  # Correspond aux indices
axis(1, at = year_indices2, labels = years2)

# Réinitialiser les paramètres de la fenêtre graphique
par(mfrow = c(1, 1))



#Traçage volatilité
dev.new()
par(mfrow = c(2, 1))  # Diviser la fenêtre en 2 lignes, 1 colonne
min_Volatility <- min(DataPeriode1$Volatility, DataPeriode2$Volatility)
max_Volatility <- max(DataPeriode1$Volatility, DataPeriode2$Volatility)

# Tracer la première période (1881-1990) en bleu (en haut)
plot(time_index1, DataPeriode1$Volatility, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Volatilité (%)",
     main = "Évolution de la volatilité 
     (Janvier 1881 - Décembre 1990)",
     xaxt = "n",ylim = c(min_Volatility, max_Volatility))  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (années)
years1 <- seq(1881, 1990, by = 10)  # Générer des années tous les 20 ans
year_indices1 <- seq(1, nrow(DataPeriode1), length.out = length(years1))  # Correspond aux indices
axis(1, at = year_indices1, labels = years1)

# Tracer la deuxième période (1991-2024) en bleu (en bas)
plot(time_index2, DataPeriode2$Volatility, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Volatilité (%)",
     main = "Évolution de la volatilité 
     (Janvier 1991 - Septembre 2024)",
     xaxt = "n",ylim = c(min_Volatility, max_Volatility))  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (années)
years2 <- seq(1990, 2024 , by = 10)  # Générer des années tous les 20 ans
year_indices2 <- seq(1, nrow(DataPeriode2), length.out = length(years2))  # Correspond aux indices
axis(1, at = year_indices2, labels = years2)
par(mfrow = c(1, 1))



dataset1=subset(DataPeriode1,select=-c(1)) #enlever la colonne DATE
dataset2=subset(DataPeriode2,select=-c(1)) #enlever la colonne DATE
#matrice de corrélation
install.packages("corrplot")
library(corrplot) 
# Calculer les matrices de corrélation et les convertir en pourcentage
correlation_matrix1 <- cor(dataset1, use = "complete.obs", method = "pearson") 
correlation_matrix2 <- cor(dataset2, use = "complete.obs", method = "pearson") 

dev.new()
par(mfrow = c(2, 1))

# Premier graphique pour la première période
corrplot(correlation_matrix1, 
         method = "number", type = "upper",addCoef.col = "black", tl.col = "black",
         number.cex = 0.9, cl.pos = "n", col = colorRampPalette(c("black"))(200))
title("Janvier 1881 - Décembre 1990                                                                     ",
      line = 1.2)

# Deuxième graphique pour la deuxième période
corrplot(correlation_matrix2, 
         method = "number", type = "upper", addCoef.col = "black",tl.col = "black",
         number.cex = 0.9, cl.pos = "n", col = colorRampPalette(c("black"))(200))
title("Janvier 1991 - Septembre 2024                                                                     ", 
      line = 1.2)
par(mfrow = c(1, 1))


ts.plot(dataset1)
volatilite1 <- dataset1[, 5] #prendre seulement la colonne Volatilité
ts.plot(dataset2)
volatilite2 <- dataset2[, 5] #prendre seulement la colonne Volatilité
# Tracer le graphe de l'ACF
dev.new()
par(mfrow = c(2, 1))
acf(volatilite1, main = "ACF de la Volatilité 1881-1990", lag.max = 100)
acf(volatilite2, main = "ACF de la Volatilité 1991-2024", lag.max = 100)
par(mfrow = c(1, 1))

# Tracer le graphe de la PACF
dev.new()
par(mfrow = c(2, 1))
pacf(volatilite1, main = "PACF de la Volatilité 1881-1990", lag.max = 100)
pacf(volatilite2, main = "PACF de la Volatilité 1991-2024", lag.max = 100)
par(mfrow = c(1, 1))

#pour rendre la volatilité stationnaire, on fait la dérivé premier
dataset1dif1=diff(DataPeriode1$Volatility) #periode 1
dataset2dif1=diff(DataPeriode2$Volatility) #periode 2
time_index1d1 <- 1:(nrow(DataPeriode1)-1)
time_index2d1 <- 1:(nrow(DataPeriode2)-1)
# Tracer la volatilitéd1 en fonction du temps
dev.new()
par(mfrow = c(2, 1))
#periode 1
plot(time_index1d1, dataset1dif1, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Dérivé Volatilité",
     main = "dérivé volatilité (Janvier 1881 - Décembre 1990)",
     xaxt = "n")  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (années)
years1 <- seq(1881, 1990, by = 10)  # Générer des années tous les 10 ans
year_indices1 <- seq(1, nrow(DataPeriode1), length.out = length(years))  # Correspond aux indices
axis(1, at = year_indices, labels = years1)
#periode 2
plot(time_index2d1, dataset2dif1, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Dérivé Volatilité",
     main = "dérivé volatilité (Janvier 1991 - Séptembre 2024)",
     xaxt = "n")  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (années)
years2 <- seq(1991, 2024, by = 10)  # Générer des années tous les 10 ans
year_indices2 <- seq(1, nrow(DataPeriode2), length.out = length(years))  # Correspond aux indices
axis(1, at = year_indices, labels = years2)
par(mfrow = c(1, 1))



# Tracer le graphe de l'ACF volatilitéd1
dev.new()
par(mfrow = c(2, 1))
#periode 1
acf(dataset1dif1, main = "ACF de la Volatilité 1881-1990", lag.max = 40)
#periode 2
acf(dataset2dif1, main = "ACF de la Volatilité 1990-2024", lag.max = 40)
par(mfrow = c(1, 1))

# Tracer le graphe de l'PACF volatilitéd1 
dev.new()
par(mfrow = c(2, 1))
pacf(dataset1dif1, main = "PACF de la Volatilité 1881-1990", lag.max = 40)
pacf(dataset2dif1, main = "PACF de la Volatilité 1991-2024", lag.max = 40)
par(mfrow = c(1, 1))

#diviser les données en train et test (aléatoire)
install.packages("caret")
install.packages("pillar")
library(caret)
# Créer les indices de partitionnement
train_indexes1 <- createDataPartition(dataset1dif1, p = 0.8, list = FALSE)
train_indexes2 <- createDataPartition(dataset2dif1, p = 0.8, list = FALSE)
# Créer les ensembles d'entraînement et de test
train1 <- dataset1dif1[train_indexes1]
test1 <- dataset1dif1[-train_indexes1]
train2 <- dataset2dif1[train_indexes2]
test2 <- dataset2dif1[-train_indexes2]

#modèle arima
#periode 1
modelARIMA1=arima(train1,order=c(1,1,2))
modelARIMA2=arima(train2,order=c(0,1,1))
install.packages("forecast")
library(forecast)
# validation du modèle ARIMA1
# analyse des résidus
dev.new()
checkresiduals(modelARIMA1) 
residualsARIMA1=residuals(modelARIMA1)
plot(residualsARIMA1)
modelARIMA1 
# Tester la capacité de prédiction
fcARIMA1=forecast(modelARIMA1,263)
errorsARIMA1 <- test1 - fcARIMA1$mean
mseARIMA1 <- mean(errorsARIMA1^2)  # Mean Squared Error
rmseARIMA1 <- sqrt(mseARIMA1)      # Root Mean Squared Error

# validation du modèle ARIMA2
# analyse des résidus
dev.new()
checkresiduals(modelARIMA2) 
residualsARIMA2=residuals(modelARIMA2)
plot(residualsARIMA2)
modelARIMA2
# Tester la capacité de prédiction
fcARIMA2=forecast(modelARIMA2,80)
errorsARIMA2 <- test2 - fcARIMA2$mean
mseARIMA2 <- mean(errorsARIMA2^2)  # Mean Squared Error
rmseARIMA2 <- sqrt(mseARIMA2)      # Root Mean Squared Error



#Modèle GARCH
install.packages("FinTS")

ArchTest(residualsARIMA1)
ArchTest(residualsARIMA2)
#déterminer p et q de GARCH
#ACF
dev.new()
par(mfrow = c(2, 1))
acf(residualsARIMA1, main = "ACF residual^2 1881-1990", lag.max = 40)
acf(residualsARIMA2, main = "ACF residual^2 1991-2024", lag.max = 40)
par(mfrow = c(1, 1))
#PACF
dev.new()
par(mfrow = c(2, 1))
pacf(residualsARIMA1, main = "PACF residual^2 1881-1990", lag.max = 40)
pacf(residualsARIMA2, main = "PACF residual^2 1991-2024", lag.max = 40)
par(mfrow = c(1, 1))

install.packages("rugarch")  # Installer le package 
library(rugarch)             # Charger le package
#modèle
spec1 <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 2), include.mean = TRUE),
  distribution.model = "norm")
spec2 <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(0, 1)),
  mean.model = list(armaOrder = c(0, 1), include.mean = TRUE),
  distribution.model = "norm")


# Ajuster le modèle ARIMA-EGARCH1
model_ARIMA1egarch <- ugarchfit(spec = spec1, data = dataset1dif1)
summary(model_ARIMA1egarch)  # Résumé des résultats du modèle
coef(model_ARIMA1egarch)
# Afficher un résumé détaillé
show(model_ARIMA1egarch)

# Ajuster le modèle ARIMA-EGARCH2
model_ARIMA2egarch <- ugarchfit(spec = spec2, data = dataset2dif1)
summary(model_ARIMA2egarch)  # Résumé des résultats du modèle
coef(model_ARIMA2egarch)

show(model_ARIMA2egarch)



# Prévoir avec le modèle EGARCH
#periode 1
fcARIMA_EGARCH1 <- ugarchforecast(model_ARIMA1egarch, n.ahead = length(test1))  # Prévisions pour les données de test
fc_mean_ARIMAEGARCH1 <- fitted(fcARIMA_EGARCH1)
#periode 2
fcARIMA_EGARCH2 <- ugarchforecast(model_ARIMA2egarch, n.ahead = length(test2))  # Prévisions pour les données de test
fc_mean_ARIMAEGARCH2 <- fitted(fcARIMA_EGARCH2)


#validation du modèle
#periode1
errorsARIMAEGARCH1 <- test1 - fc_mean_ARIMAEGARCH1
mseARIMAEGARCH1 <- mean(errorsARIMAEGARCH1^2)  # Mean Squared Error
rmseARIMAEGARCH1 <- sqrt(mseARIMAEGARCH1)      # Root Mean Squared Error
#periode2
errorsARIMAEGARCH2 <- test2 - fc_mean_ARIMAEGARCH2
mseARIMAEGARCH2 <- mean(errorsARIMAEGARCH2^2)  # Mean Squared Error
rmseARIMAEGARCH2 <- sqrt(mseARIMAEGARCH2)      # Root Mean Squared Error







##Partie Machine learning
data1dif1 <- as.data.frame(lapply(dataset1, diff))
data2dif1 <- as.data.frame(lapply(dataset2, diff))
#diviser data en train et test

TrainIndexML1=createDataPartition(data1dif1$Volatility,p=0.8,list=FALSE)
trainML1=data1dif1[TrainIndexML1,]
testML1=data1dif1[-TrainIndexML1,]
TrainIndexML2=createDataPartition(data2dif1$Volatility,p=0.8,list=FALSE)
trainML2=data2dif1[TrainIndexML2,]
testML2=data2dif1[-TrainIndexML2,]

#enlever la colonne volatilité (pour que le modèle ne prend pas les valeur reelles de la volatilité)
x_train1=scale(subset(trainML1,select=-c(5)))
y_train1=trainML1$Volatility
x_test1=scale(subset(testML1,select=-c(5)))
y_test1=trainML1$Volatility
x_train2=scale(subset(trainML2,select=-c(5)))
y_train2=trainML2$Volatility
x_test2=scale(subset(testML2,select=-c(5)))
y_test2=trainML2$Volatility

##regression Ridge
install.packages("glmnet")
library(glmnet)
set.seed(12)
modelRidge1=cv.glmnet(x=as.matrix(x_train1),y=as.matrix(y_train1),type.measure="mse",
                     alpha=0,family="gaussian",nlambda=200)
modelRidge2=cv.glmnet(x=as.matrix(x_train2),y=as.matrix(y_train2),type.measure="mse",
                      alpha=0,family="gaussian",nlambda=200)
dev.new()
par(mfrow = c(2, 1))
plot(modelRidge1,main="1881-1990
     ")
plot(modelRidge2,main="1990-2024
     ")
par(mfrow = c(1, 1))

#afficher lambda
lambdaRidge1=tail(modelRidge1$lambda,1)
lambdaRidge1
lambdaRidge2=tail(modelRidge2$lambda,1)
lambdaRidge2
set.seed(421)
modelRidge11=glmnet(x=as.matrix(x_train1),y=as.matrix(y_train1),type.measure="mse",
                      alpha=0,family="gaussian",lambda=lambdaRidge1)
modelRidge22=glmnet(x=as.matrix(x_train2),y=as.matrix(y_train2),type.measure="mse",
                      alpha=0,family="gaussian",lambda=lambdaRidge2)
coef(modelRidge11)
coef(modelRidge22)
#prediction
testML1$pred=predict(modelRidge1,lambdaRidge1,new=as.matrix(x_test1))
testML2$pred=predict(modelRidge2,lambdaRidge2,new=as.matrix(x_test2))
erreursRidge1=testML1$Volatility-testML1$pred
erreursRidge2=testML2$Volatility-testML2$pred

#mse
mseRidge1=mean((testML1$Volatility-testML1$pred)^2)
mseRidge1
mseRidge2=mean((testML2$Volatility-testML2$pred)^2)
mseRidge2
#rmse
rmseRidge1=sqrt(mseRidge1)
rmseRidge1
rmseRidge2=sqrt(mseRidge2)
rmseRidge2





#regression lasso
set.seed(1234) #pour avoir les meme resultats apres les execution
modelLasso1=cv.glmnet(x=as.matrix(x_train1),y=as.matrix(y_train1),
                     type.measure="mse",alpha=1,family="gaussian",nlambda=200)
modelLasso2=cv.glmnet(x=as.matrix(x_train2),y=as.matrix(y_train2),
                      type.measure="mse",alpha=1,family="gaussian",nlambda=200)
dev.new()
par(mfrow = c(2, 1))
plot(modelLasso1,main="1881-1990
     ")
plot(modelLasso2,main="1990-2024
     ")
par(mfrow = c(1, 1))
#afficher lambda
lambdaLasso1 = tail(modelLasso1$lambda, 1)
lambdaLasso1
lambdaLasso2 = tail(modelLasso2$lambda, 1)
lambdaLasso2

modelLasso11=glmnet(x=as.matrix(x_train1),y=as.matrix(y_train1),type.measure="mse",
                    alpha=1,family="gaussian",lambda=lambdaLasso1)
modelLasso22=glmnet(x=as.matrix(x_train2),y=as.matrix(y_train2),type.measure="mse",
                    alpha=1,family="gaussian",lambda=lambdaLasso2)

coef(modelLasso11)
coef(modelLasso22)

predLasso1=predict(modelLasso1,lambdaLasso1,new=as.matrix(x_test1))
predLasso2=predict(modelLasso2,lambdaLasso2,new=as.matrix(x_test2))
erreursLasso1=testML1$Volatility-predLasso1
erreursLasso2=testML2$Volatility-predLasso2

#validation
mseLasso1=mean((testML1$Volatility-predLasso1)^2)
mseLasso1
rmseLasso1=sqrt(mseLasso1)
rmseLasso1
mseLasso2=mean((testML2$Volatility-predLasso2)^2)
mseLasso2
rmseLasso2=sqrt(mseLasso2)
rmseLasso2


#regression elastic net
alpha1=seq(0,1,0.001)
df1=data.frame(alpha1)
df1$mseElasticNet=0
alpha2=seq(0,1,0.001)
df2=data.frame(alpha2)
df2$mseElasticNet=0

for(i in 1:dim(df1)[1]){
  modelElasticNet1=cv.glmnet(x=as.matrix(x_train1),y=as.matrix(y_train1),
                            type.measure="mse",alpha=df1$alpha1[i],
                            family="gaussian",nlambda=200)
  predElasticNet1=predict(modelElasticNet1,tail(modelElasticNet1$lambda,1),
                          new=as.matrix(x_test1))
  mse=mean((testML1$Volatility-predElasticNet1)^2)
  df1$mseElasticNet[i]=mse
}

for(i in 1:dim(df2)[1]){
  modelElasticNet2=cv.glmnet(x=as.matrix(x_train2),y=as.matrix(y_train2),
                             type.measure="mse",alpha=df2$alpha2[i],
                             family="gaussian",nlambda=200)
  predElasticNet2=predict(modelElasticNet2,tail(modelElasticNet2$lambda,1),
                          new=as.matrix(x_test2))
  mse=mean((testML2$Volatility-predElasticNet2)^2)
  df2$mseElasticNet[i]=mse
}

#trier df
library(dplyr)
df1=arrange(df1,mseElasticNet)
df2=arrange(df2,mseElasticNet)
dev.new()
par(mfrow = c(2, 1))
plot(df1,main="1881-1990
     ")
plot(df2,main="1990-2024
     ")
par(mfrow = c(1, 1))
ElasticNet1=cv.glmnet(x=as.matrix(x_train1),y=as.matrix(y_train1),type.measure="mse",
               alpha=df1$alpha1[1],family="gaussian",nlambda=200)
ElasticNet2=cv.glmnet(x=as.matrix(x_train2),y=as.matrix(y_train2),type.measure="mse",
                alpha=df2$alpha2[1],family="gaussian",nlambda=200)
predElasticNet1=predict(modelElasticNet1,tail(ElasticNet1$lambda,1),
                        new=as.matrix(x_test1))
predElasticNet2=predict(modelElasticNet2,tail(ElasticNet2$lambda,1),
                        new=as.matrix(x_test2))
erreursElasticNet1=testML1$Volatility-predElasticNet1
erreursElasticNet2=testML2$Volatility-predElasticNet2

dev.new()
par(mfrow = c(2, 1))
plot(ElasticNet1,main="1881-1990
     ")
plot(ElasticNet2,main="1990-2024
     ")
par(mfrow = c(1, 1))
LambdaElasticNet1=tail(ElasticNet1$lambda,1)
LambdaElasticNet1
LambdaElasticNet2=tail(ElasticNet2$lambda,1)
LambdaElasticNet2

ElasticNet11=glmnet(x=as.matrix(x_train1),y=as.matrix(y_train1),type.measure="mse",
                      alpha=df1$alpha1[1],family="gaussian",lambda=LambdaElasticNet1)
ElasticNet22=glmnet(x=as.matrix(x_train2),y=as.matrix(y_train2),type.measure="mse",
                      alpha=df2$alpha2[1],family="gaussian",lambda=LambdaElasticNet2)

coef(ElasticNet11)
coef(ElasticNet22)
#predElasticNet1=predict(ElasticNet1,ElasticNet1$lambda.min,new=as.matrix(x_test1))
#predElasticNet2=predict(ElasticNet2,ElasticNet2$lambda.min,new=as.matrix(x_test2))

mseElasticNet1=df1$mseElasticNet[1]
mseElasticNet1
rmseElasticNet1=sqrt(mseElasticNet1)
rmseElasticNet1
mseElasticNet2=df2$mseElasticNet[1]
mseElasticNet2
rmseElasticNet2=sqrt(mseElasticNet2)
rmseElasticNet2






#Reseaux de neuronnes
# install library
install.packages("neuralnet")


library(neuralnet)

# Normalisation des données (Min-Max Scaling)
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Appliquer la normalisation sur les variables explicatives et la cible
trainML1_norm <- as.data.frame(lapply(trainML1, normalize))
testML1_norm <- as.data.frame(lapply(testML1, normalize))
trainML2_norm <- as.data.frame(lapply(trainML2, normalize))
testML2_norm <- as.data.frame(lapply(testML2, normalize))

#une seule couche cachée
dfNN1 = data.frame(NbrNeurones = rep(0, 12), mseNN1 = rep(0, 12))
dfNN2 = data.frame(NbrNeurones = rep(0, 12), mseNN2 = rep(0, 12))
for(i in 1:12){
  set.seed(432)  # Assurer la reproductibilité
  
  modelNN1 = neuralnet(Volatility ~ DividendGrowthRate + PolicyRate + InterestRate + Yield,
                       data = trainML1_norm, hidden = i, linear.output = TRUE)
  
  modelNN2 = neuralnet(Volatility ~ DividendGrowthRate + PolicyRate + InterestRate + Yield,
                       data = trainML2_norm, hidden = i, linear.output = TRUE)
  
  # Prédictions
  predNN1 = compute(modelNN1, testML1_norm[, c("DividendGrowthRate", "PolicyRate", "InterestRate", "Yield")])$net.result
  predNN2 = compute(modelNN2, testML2_norm[, c("DividendGrowthRate", "PolicyRate", "InterestRate", "Yield")])$net.result
  
  # Dénormalisation des prédictions
  minV1 = min(trainML1$Volatility)
  maxV1 = max(trainML1$Volatility)
  predNN1 = predNN1 * (maxV1 - minV1) + minV1
  
  minV2 = min(trainML2$Volatility)
  maxV2 = max(trainML2$Volatility)
  predNN2 = predNN2 * (maxV2 - minV2) + minV2
  
  # Calcul du MSE
  mse1 = mean((testML1$Volatility - predNN1)^2)
  mse2 = mean((testML2$Volatility - predNN2)^2)
  
  # Stocker les résultats
  dfNN1$NbrNeurones[i] = i
  dfNN1$mseNN1[i] = mse1
  dfNN2$NbrNeurones[i] = i
  dfNN2$mseNN2[i] = mse2
}
dev.new()
par(mfrow = c(2, 1))
plot(dfNN1$NbrNeurones, dfNN1$mseNN1, type = "b", col = "blue", pch = 19,
     xlab = "Nombre de neurones cachés", ylab = "MSE",
     main = "MSE en fonction du nombre de neurones cachés
            1881-1990", xaxt = "n")
axis(1, at = dfNN1$NbrNeurones, labels = dfNN1$NbrNeurones)

plot(dfNN2$NbrNeurones, dfNN2$mseNN2, type = "b", col = "blue", pch = 19,
     xlab = "Nombre de neurones cachés", ylab = "MSE",
     main = "MSE en fonction du nombre de neurones cachés
            1991-2024", xaxt = "n")
axis(1, at = dfNN2$NbrNeurones, labels = dfNN2$NbrNeurones)
par(mfrow = c(1, 1))

dfNN1 <- dfNN1[order(dfNN1$mseNN1), ]
dfNN2 <- dfNN2[order(dfNN2$mseNN2), ]
set.seed(43211)
modelNN1 = neuralnet(Volatility ~ DividendGrowthRate + PolicyRate + InterestRate + Yield,
                     data = trainML1_norm, hidden = dfNN1$NbrNeurones[1], linear.output = TRUE)


dev.new()
plot(modelNN1)
set.seed(43200)
modelNN2 = neuralnet(Volatility ~ DividendGrowthRate + PolicyRate + InterestRate + Yield,
                     data = trainML2_norm, hidden = dfNN2$NbrNeurones[1], linear.output = TRUE)

dev.new()
plot(modelNN2)

predNN1 = compute(modelNN1, testML1_norm[, c("DividendGrowthRate", "PolicyRate", "InterestRate", "Yield")])$net.result
predNN2 = compute(modelNN2, testML2_norm[, c("DividendGrowthRate", "PolicyRate", "InterestRate", "Yield")])$net.result
predNN1 = predNN1 * (maxV1 - minV1) + minV1
predNN2 = predNN2 * (maxV2 - minV2) + minV2
erreursNN1=testML1$Volatility - predNN1
erreursNN2=testML2$Volatility - predNN2






#deux couches cachées
dfNN11 <- data.frame(NbrNeurones1 = integer(), NbrNeurones2 = integer(), mseNN1 = numeric())
dfNN22 <- data.frame(NbrNeurones1 = integer(), NbrNeurones2 = integer(), mseNN2 = numeric())

for(i in 1:6){  # Première couche cachée : 1 à 6 neurones
  for(j in 1:6){  # Deuxième couche cachée : 1 à 6 neurones
    set.seed(242)  
    
    modelNN11 = neuralnet(Volatility ~ DividendGrowthRate + PolicyRate + InterestRate + Yield,
                         data = trainML1_norm, hidden = c(i, j), linear.output = TRUE)
    modelNN22 = neuralnet(Volatility ~ DividendGrowthRate + PolicyRate + InterestRate + Yield,
                         data = trainML2_norm, hidden = c(i, j), linear.output = TRUE)
    
    # Prédictions
    predNN11 = compute(modelNN11, testML1_norm[, c("DividendGrowthRate", "PolicyRate", "InterestRate", "Yield")])$net.result
    predNN22 = compute(modelNN22, testML2_norm[, c("DividendGrowthRate", "PolicyRate", "InterestRate", "Yield")])$net.result
    
    # Dénormalisation des prédictions
    minV11 = min(trainML1$Volatility)
    maxV11 = max(trainML1$Volatility)
    predNN11 = predNN11 * (maxV11 - minV11) + minV11
    
    minV22 = min(trainML2$Volatility)
    maxV22 = max(trainML2$Volatility)
    predNN22 = predNN22 * (maxV22 - minV22) + minV22
    
    # Calcul du MSE
    mse1 = mean((testML1$Volatility - predNN11)^2)
    mse2 = mean((testML2$Volatility - predNN22)^2)
    
    # Stocker les résultats
    dfNN11[nrow(dfNN11) + 1, ] <- c(i, j, mse1)
    dfNN22[nrow(dfNN22) + 1, ] <- c(i, j, mse2)
  }
}

# Créer une nouvelle variable combinée pour les étiquettes de l'axe X
dfNN11$CombinedLabels <- paste("(", dfNN11$NbrNeurones1, ",", dfNN11$NbrNeurones2, ")")
dfNN22$CombinedLabels <- paste("(", dfNN22$NbrNeurones1, ",", dfNN22$NbrNeurones2, ")")

dev.new()
par(mfrow = c(2, 1))
# Tracer le graphique en utilisant les étiquettes combinées
plot(dfNN11$NbrNeurones1, dfNN11$mseNN1, type = "b", col = "blue", pch = 19,
     xlab = "Couches cachées", ylab = "MSE",
     main = "MSE en fonction du nombre de neurones cachés
        1881-1990", xaxt = "n")

# Personnaliser les étiquettes de l'axe X
axis(1, at = dfNN11$NbrNeurones1, labels = dfNN11$CombinedLabels)

plot(dfNN22$NbrNeurones1, dfNN22$mseNN2, type = "b", col = "blue", pch = 19,
     xlab = "Couches cachées", ylab = "MSE",
     main = "MSE en fonction du nombre de neurones cachés
        1991-2024", xaxt = "n")

# Personnaliser les étiquettes de l'axe X
axis(1, at = dfNN22$NbrNeurones1, labels = dfNN22$CombinedLabels)

par(mfrow = c(1, 1))

dfNN11 <- dfNN11[order(dfNN11$mseNN1), ]
dfNN22 <- dfNN22[order(dfNN22$mseNN2), ]

set.seed(111)
modelNN11 = neuralnet(Volatility ~ DividendGrowthRate + PolicyRate + InterestRate + Yield,
                      data = trainML1_norm, 
                      hidden = c(5,1), linear.output = TRUE)

plot(modelNN11)
set.seed(222)
modelNN22 = neuralnet(Volatility ~ DividendGrowthRate + PolicyRate + InterestRate + Yield,
                     data = trainML2_norm, 
                     hidden = c(5,1), linear.output = TRUE)

plot(modelNN22)


predNN11 = compute(modelNN11, testML1_norm[, c("DividendGrowthRate", "PolicyRate", "InterestRate", "Yield")])$net.result
predNN22 = compute(modelNN22, testML2_norm[, c("DividendGrowthRate", "PolicyRate", "InterestRate", "Yield")])$net.result
predNN11 = predNN11 * (maxV11 - minV11) + minV11
predNN22 = predNN22 * (maxV22 - minV22) + minV22
erreursNN11=testML1$Volatility - predNN11
erreursNN22=testML2$Volatility - predNN22

dev.new()
plot(testML2$Volatility, type = "o", col = "blue", xlab = "Index", ylab = "Valeurs",
     main = "Valeurs Réelles vs Prédites", pch = 16)

# Ajouter les valeurs prédites
lines(predNN2, type = "o", col = "red", pch = 16)

# Ajouter une légende
legend("topleft", legend = c("Valeurs Réelles", "Valeurs Prédites"),
       col = c("blue", "red"), lty = 1, pch = 16)






# Comparaison
erreursP1 <- data.frame(Modèles=rep(0,2),MSE1 = rep(0, 2), RMSE1 = rep(0, 2),
                      MSE2 = rep(0, 2), RMSE2 = rep(0, 2))
erreursP1[1, ] <- c("ARIMA", mseARIMA1/10000,rmseARIMA1/10000,mseARIMA2/10000,rmseARIMA2/10000)
erreursP1[2, ] <- c("ARIMA-EGARCH", mseARIMAEGARCH1/10000,rmseARIMAEGARCH1/10000,mseARIMAEGARCH2/10000,rmseARIMAEGARCH2/10000)


erreursP2 <- data.frame(Modèles=rep(0,5),MSE1 = rep(0, 5), RMSE1 = rep(0, 5),
                        MSE2 = rep(0, 5), RMSE2 = rep(0, 5))
erreursP2[1, ] <- c("Ridge", mseRidge1/10000,rmseRidge1/10000,mseRidge2/10000,rmseRidge2/10000)
erreursP2[2, ] <- c("Lasso", mseLasso1/10000,rmseLasso1/10000,mseLasso2/10000,rmseLasso2/10000)
erreursP2[3, ] <- c("ElasticNet", mseElasticNet1/10000,rmseElasticNet1/10000,mseElasticNet2/10000,rmseElasticNet2/10000)
erreursP2[4, ] <- c("NN1", dfNN1$mseNN1[1]/10000,sqrt(dfNN1$mseNN1[1])/10000,dfNN2$mseNN2[1]/10000,sqrt(dfNN2$mseNN2[1])/10000)
erreursP2[5, ] <- c("NN2", dfNN11$mseNN1[1]/10000,sqrt(dfNN11$mseNN1[1])/10000,dfNN22$mseNN2[1]/10000,sqrt(dfNN22$mseNN2[1])/10000)

erreurs <- data.frame(Modèles=rep(0,7),MSE1 = rep(0, 7), RMSE1 = rep(0, 7),
                      MSE2 = rep(0, 7), RMSE2 = rep(0, 7))
erreurs[1, ] <- c("ARIMA", mseARIMA1/10000,rmseARIMA1/10000,mseARIMA2/10000,rmseARIMA2/10000)
erreurs[2, ] <- c("ARIMA-EGARCH", mseARIMAEGARCH1/10000,rmseARIMAEGARCH1/10000,mseARIMAEGARCH2/10000,rmseARIMAEGARCH2/10000)
erreurs[3, ] <- c("Ridge", mseRidge1/10000,rmseRidge1/10000,mseRidge2/10000,rmseRidge2/10000)
erreurs[4, ] <- c("Lasso", mseLasso1/10000,rmseLasso1/10000,mseLasso2/10000,rmseLasso2/10000)
erreurs[5, ] <- c("ElasticNet", mseElasticNet1/10000,rmseElasticNet1/10000,mseElasticNet2/10000,rmseElasticNet2/10000)
erreurs[6, ] <- c("NN1", dfNN1$mseNN1[1]/10000,sqrt(dfNN1$mseNN1[1])/10000,dfNN2$mseNN2[1]/10000,sqrt(dfNN2$mseNN2[1])/10000)
erreurs[7, ] <- c("NN2", dfNN11$mseNN1[1]/10000,sqrt(dfNN11$mseNN1[1])/10000,dfNN22$mseNN2[1]/10000,sqrt(dfNN22$mseNN2[1])/10000)


#test Diebold marrion
EcartReelPrediction1 <- data.frame(ARIMA=rep(0, 263),ARIMAEGARCH = rep(0, 263),
                        Ridge = rep(0, 263),Lasso = rep(0, 263), 
                        ElasticNet = rep(0, 263),NN1 = rep(0, 263),NN2 = rep(0, 263))

EcartReelPrediction2 <- data.frame(ARIMA=rep(0,80),ARIMAEGARCH = rep(0, 80),
                        Ridge = rep(0, 80),Lasso = rep(0, 80), 
                        ElasticNet = rep(0, 80),NN1 = rep(0, 80),NN2 = rep(0, 80))

EcartReelPrediction1$ARIMA=errorsARIMA1/100
EcartReelPrediction1$ARIMAEGARCH=errorsARIMAEGARCH1/100
EcartReelPrediction1$Ridge=erreursRidge1/100
EcartReelPrediction1$Lasso=erreursLasso1/100
EcartReelPrediction1$ElasticNet=erreursElasticNet1/100
EcartReelPrediction1$NN1=erreursNN1/100
EcartReelPrediction1$NN2=erreursNN11/100
colnames(EcartReelPrediction1) <- c("ARIMA", "ARIMAEGARCH", "Ridge",
                                    "Lasso","ElasticNet","NN1","NN2")
# Matrice des erreurs
mat_erreurs1 <- as.matrix(EcartReelPrediction1)

# Tracer toutes les courbes d'erreurs
dev.new()
matplot(mat_erreurs1, type = "l", lty = 1, col = 1:ncol(mat_erreurs1),
        xlab = "Temps", ylab = "Erreur Normalisée", 
        main = "Comparaison des erreurs des modèles")

# Ajouter la légende
legend("topright", legend = colnames(mat_erreurs1), col = 1:ncol(mat_erreurs1), 
       lty = 1, cex = 0.8)



EcartReelPrediction2$ARIMA=errorsARIMA2/100
EcartReelPrediction2$ARIMAEGARCH=errorsARIMAEGARCH2/100
EcartReelPrediction2$Ridge=erreursRidge2/100
EcartReelPrediction2$Lasso=erreursLasso2/100
EcartReelPrediction2$ElasticNet=erreursElasticNet2/100
EcartReelPrediction2$NN1=erreursNN2/100
EcartReelPrediction2$NN2=erreursNN22/100
colnames(EcartReelPrediction2) <- c("ARIMA", "ARIMAEGARCH", "Ridge",
                                    "Lasso","ElasticNet","NN1","NN2")

# Matrice des erreurs
mat_erreurs2 <- as.matrix(EcartReelPrediction2)

# Tracer toutes les courbes d'erreurs
dev.new()
matplot(mat_erreurs2, type = "l", lty = 1, col = 1:ncol(mat_erreurs2),
        xlab = "Temps", ylab = "Erreur Normalisée", 
        main = "Comparaison des erreurs des modèles")

# Ajouter la légende
legend("topright", legend = colnames(mat_erreurs2), col = 1:ncol(mat_erreurs2), 
       lty = 1, cex = 0.8)


#DM TEST période 1
modeles1 <- colnames(EcartReelPrediction1)

# Initialisation de la matrice des p-values du test Diebold-Mariano
dm_matrix1 <- matrix(NA, nrow = length(modeles1), ncol = length(modeles1))
rownames(dm_matrix1) <- modeles1
colnames(dm_matrix1) <- modeles1

# Boucles pour comparer chaque modèle avec les autres
for (i in 1:(length(modeles1) - 1)) {
  for (j in (i + 1):length(modeles1)) {
    # les erreurs de chaque modèle
    erreur1 <- EcartReelPrediction1[[modeles1[i]]]
    erreur2 <- EcartReelPrediction1[[modeles1[j]]]
    
    # Vérifier si les erreurs sont identiques
    if (var(erreur1 - erreur2) == 0) {
      dm_matrix1[i, j] <- NA
      dm_matrix1[j, i] <- NA
    } else {
      # Test Diebold-Mariano
      test_dm1 <- dm.test(erreur1, erreur2, alternative = "two.sided", h = 1)
      dm_matrix1[i, j] <- test_dm1$p.value
      dm_matrix1[j, i] <- test_dm1$p.value
    }
  }
}



#DM TEST période 2
modeles2 <- colnames(EcartReelPrediction2)

# Initialisation de la matrice des p-values du test Diebold-Mariano
dm_matrix2 <- matrix(NA, nrow = length(modeles2), ncol = length(modeles2))
rownames(dm_matrix2) <- modeles2
colnames(dm_matrix2) <- modeles2

# Boucles pour comparer chaque modèle avec les autres
for (i in 1:(length(modeles2) - 1)) {
  for (j in (i + 1):length(modeles2)) {
    # les erreurs de chaque modèle
    erreur1 <- EcartReelPrediction2[[modeles2[i]]]
    erreur2 <- EcartReelPrediction2[[modeles2[j]]]
    
    # Vérifier si les erreurs sont identiques
    if (var(erreur1 - erreur2) == 0) {
      dm_matrix2[i, j] <- NA
      dm_matrix2[j, i] <- NA
    } else {
      # Test Diebold-Mariano
      test_dm2 <- dm.test(erreur1, erreur2, alternative = "two.sided", h = 1)
      dm_matrix2[i, j] <- (test_dm2$p.value)
      dm_matrix2[j, i] <- (test_dm2$p.value)
    }
  }
}








# Charger la librairie pour le test de Levene
install.packages("DescTools")
library(DescTools)


# Initialiser les matrices pour stocker les p-values
models <- colnames(EcartReelPrediction2)
n <- length(models)

# Matrices de résultats
wilcoxon_matrix <- matrix(NA, n, n, dimnames = list(models, models))
levene_matrix <- matrix(NA, n, n, dimnames = list(models, models))

# Boucles for pour comparer chaque modèle avec les autres
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    model1 <- models[i]
    model2 <- models[j]
    
    # Test de Wilcoxon
    wilcox_test <- wilcox.test(EcartReelPrediction2[[model1]], EcartReelPrediction2[[model2]], paired = TRUE)
    wilcoxon_matrix[i, j] <- wilcox_test$p.value
    wilcoxon_matrix[j, i] <- wilcox_test$p.value  # Symétrique
    
    # Test de Levene
    group <- factor(rep(c(model1, model2), each = nrow(EcartReelPrediction2)))
    errors <- c(EcartReelPrediction2[[model1]], EcartReelPrediction2[[model2]])
    levene_test <- leveneTest(errors ~ group)
    
    levene_matrix[i, j] <- levene_test$`Pr(>F)`[1]
    levene_matrix[j, i] <- levene_test$`Pr(>F)`[1]  # Symétrique
  }
}

# Afficher les matrices de résultats
print("Matrice des p-values du test de Wilcoxon:")
print(wilcoxon_matrix)

print("Matrice des p-values du test de Levene:")
print(levene_matrix)
