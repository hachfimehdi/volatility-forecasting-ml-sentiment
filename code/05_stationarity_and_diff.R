
ts.plot(dataset1)
volatilite1 <- dataset1[, 5] #prendre seulement la colonne Volatilit?
ts.plot(dataset2)
volatilite2 <- dataset2[, 5] #prendre seulement la colonne Volatilit?
# Tracer le graphe de l'ACF
dev.new()
par(mfrow = c(2, 1))
acf(volatilite1, main = "ACF de la Volatilit? 1881-1990", lag.max = 100)
acf(volatilite2, main = "ACF de la Volatilit? 1991-2024", lag.max = 100)
par(mfrow = c(1, 1))

# Tracer le graphe de la PACF
dev.new()
par(mfrow = c(2, 1))
pacf(volatilite1, main = "PACF de la Volatilit? 1881-1990", lag.max = 100)
pacf(volatilite2, main = "PACF de la Volatilit? 1991-2024", lag.max = 100)
par(mfrow = c(1, 1))

#pour rendre la volatilit? stationnaire, on fait la d?riv? premier
dataset1dif1=diff(DataPeriode1$Volatility) #periode 1
dataset2dif1=diff(DataPeriode2$Volatility) #periode 2
time_index1d1 <- 1:(nrow(DataPeriode1)-1)
time_index2d1 <- 1:(nrow(DataPeriode2)-1)
# Tracer la volatilit?d1 en fonction du temps
dev.new()
par(mfrow = c(2, 1))
#periode 1
plot(time_index1d1, dataset1dif1, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "D?riv? Volatilit?",
     main = "d?riv? volatilit? (Janvier 1881 - D?cembre 1990)",
     xaxt = "n")  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (ann?es)
years1 <- seq(1881, 1990, by = 10)  # G?n?rer des ann?es tous les 10 ans
year_indices1 <- seq(1, nrow(DataPeriode1), length.out = length(years))  # Correspond aux indices
axis(1, at = year_indices, labels = years1)
#periode 2
plot(time_index2d1, dataset2dif1, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "D?riv? Volatilit?",
     main = "d?riv? volatilit? (Janvier 1991 - S?ptembre 2024)",
     xaxt = "n")  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (ann?es)
years2 <- seq(1991, 2024, by = 10)  # G?n?rer des ann?es tous les 10 ans
year_indices2 <- seq(1, nrow(DataPeriode2), length.out = length(years))  # Correspond aux indices
axis(1, at = year_indices, labels = years2)
par(mfrow = c(1, 1))



# Tracer le graphe de l'ACF volatilit?d1
dev.new()
par(mfrow = c(2, 1))
#periode 1
acf(dataset1dif1, main = "ACF de la Volatilit? 1881-1990", lag.max = 40)
#periode 2
acf(dataset2dif1, main = "ACF de la Volatilit? 1990-2024", lag.max = 40)
par(mfrow = c(1, 1))

# Tracer le graphe de l'PACF volatilit?d1 
dev.new()
par(mfrow = c(2, 1))
pacf(dataset1dif1, main = "PACF de la Volatilit? 1881-1990", lag.max = 40)
pacf(dataset2dif1, main = "PACF de la Volatilit? 1991-2024", lag.max = 40)
par(mfrow = c(1, 1))
