
#Mod?le GARCH
install.packages("FinTS")

ArchTest(residualsARIMA1)
ArchTest(residualsARIMA2)
#d?terminer p et q de GARCH
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
#mod?le
spec1 <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 2), include.mean = TRUE),
  distribution.model = "norm")
spec2 <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(0, 1)),
  mean.model = list(armaOrder = c(0, 1), include.mean = TRUE),
  distribution.model = "norm")


# Ajuster le mod?le ARIMA-EGARCH1
model_ARIMA1egarch <- ugarchfit(spec = spec1, data = dataset1dif1)
summary(model_ARIMA1egarch)  # R?sum? des r?sultats du mod?le
coef(model_ARIMA1egarch)
# Afficher un r?sum? d?taill?
show(model_ARIMA1egarch)

# Ajuster le mod?le ARIMA-EGARCH2
model_ARIMA2egarch <- ugarchfit(spec = spec2, data = dataset2dif1)
summary(model_ARIMA2egarch)  # R?sum? des r?sultats du mod?le
coef(model_ARIMA2egarch)

show(model_ARIMA2egarch)



# Pr?voir avec le mod?le EGARCH
#periode 1
fcARIMA_EGARCH1 <- ugarchforecast(model_ARIMA1egarch, n.ahead = length(test1))  # Pr?visions pour les donn?es de test
fc_mean_ARIMAEGARCH1 <- fitted(fcARIMA_EGARCH1)
#periode 2
fcARIMA_EGARCH2 <- ugarchforecast(model_ARIMA2egarch, n.ahead = length(test2))  # Pr?visions pour les donn?es de test
fc_mean_ARIMAEGARCH2 <- fitted(fcARIMA_EGARCH2)


#validation du mod?le
#periode1
errorsARIMAEGARCH1 <- test1 - fc_mean_ARIMAEGARCH1
mseARIMAEGARCH1 <- mean(errorsARIMAEGARCH1^2)  # Mean Squared Error
rmseARIMAEGARCH1 <- sqrt(mseARIMAEGARCH1)      # Root Mean Squared Error
#periode2
errorsARIMAEGARCH2 <- test2 - fc_mean_ARIMAEGARCH2
mseARIMAEGARCH2 <- mean(errorsARIMAEGARCH2^2)  # Mean Squared Error
rmseARIMAEGARCH2 <- sqrt(mseARIMAEGARCH2)      # Root Mean Squared Error
