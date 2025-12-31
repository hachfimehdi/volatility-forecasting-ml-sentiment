
time_index1 <- 1:nrow(DataPeriode1) #nombre des mois dans la p?riode 1
time_index2 <- 1:nrow(DataPeriode2) #nombre des mois dans la p?riode 2

#Tra?age du rendement
dev.new()
par(mfrow = c(2, 1))  # Diviser la fen?tre en 2 lignes, 1 colonne
min_yield <- min(DataPeriode1$Yield, DataPeriode2$Yield)
max_yield <- max(DataPeriode1$Yield, DataPeriode2$Yield)

# Tracer la premi?re p?riode (1881-1990) en bleu (en haut)
plot(time_index1, DataPeriode1$Yield, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Rendement (%)",
     main = "?volution du rendement (Janvier 1881 - D?cembre 1990)",
     xaxt = "n",ylim = c(min_yield, max_yield))  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (ann?es)
years1 <- seq(1881, 1990, by = 10)  # G?n?rer des ann?es tous les 20 ans
year_indices1 <- seq(1, nrow(DataPeriode1), length.out = length(years1))  # Correspond aux indices
axis(1, at = year_indices1, labels = years1)

# Tracer la deuxi?me p?riode (1991-2024) en bleu (en bas)
plot(time_index2, DataPeriode2$Yield, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Rendement (%)",
     main = "?volution du rendement (Janvier 1991 - Septembre 2024)",
     xaxt = "n",ylim = c(min_yield, max_yield))  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (ann?es)
years2 <- seq(1990, 2024 , by = 10)  # G?n?rer des ann?es tous les 20 ans
year_indices2 <- seq(1, nrow(DataPeriode2), length.out = length(years2))  # Correspond aux indices
axis(1, at = year_indices2, labels = years2)

# R?initialiser les param?tres de la fen?tre graphique
par(mfrow = c(1, 1))



#Tra?age du taux d'int?r?t
dev.new()
par(mfrow = c(2, 1))  # Diviser la fen?tre en 2 lignes, 1 colonne
min_InterestRate <- min(DataPeriode1$InterestRate, DataPeriode2$InterestRate)
max_InterestRate <- max(DataPeriode1$InterestRate, DataPeriode2$InterestRate)

# Tracer la premi?re p?riode (1881-1990) en bleu (en haut)
plot(time_index1, DataPeriode1$InterestRate, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Taux d'int?r?t (%)",
     main = "?volution du taux d'int?r?t (Janvier 1881 - D?cembre 1990)",
     xaxt = "n",ylim = c(min_InterestRate, max_InterestRate))  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (ann?es)
years1 <- seq(1881, 1990, by = 10)  # G?n?rer des ann?es tous les 20 ans
year_indices1 <- seq(1, nrow(DataPeriode1), length.out = length(years1))  # Correspond aux indices
axis(1, at = year_indices1, labels = years1)

# Tracer la deuxi?me p?riode (1991-2024) en bleu (en bas)
plot(time_index2, DataPeriode2$InterestRate, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Taux d'int?r?t (%)",
     main = "?volution du taux d'int?r?t (Janvier 1991 - Septembre 2024)",
     xaxt = "n",ylim = c(min_InterestRate, max_InterestRate))  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (ann?es)
years2 <- seq(1990, 2024 , by = 10)  # G?n?rer des ann?es tous les 20 ans
year_indices2 <- seq(1, nrow(DataPeriode2), length.out = length(years2))  # Correspond aux indices
axis(1, at = year_indices2, labels = years2)

# R?initialiser les param?tres de la fen?tre graphique
par(mfrow = c(1, 1))



#Tra?age du taux directeur
dev.new()
par(mfrow = c(2, 1))  # Diviser la fen?tre en 2 lignes, 1 colonne
min_PolicyRate <- min(DataPeriode1$PolicyRate, DataPeriode2$PolicyRate)
max_PolicyRate <- max(DataPeriode1$PolicyRate, DataPeriode2$PolicyRate)

# Tracer la premi?re p?riode (1881-1990) en bleu (en haut)
plot(time_index1, DataPeriode1$PolicyRate, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Taux directeur (%)",
     main = "?volution du taux directeur (Janvier 1881 - D?cembre 1990)",
     xaxt = "n",ylim = c(min_PolicyRate, max_PolicyRate))  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (ann?es)
years1 <- seq(1881, 1990, by = 10)  # G?n?rer des ann?es tous les 20 ans
year_indices1 <- seq(1, nrow(DataPeriode1), length.out = length(years1))  # Correspond aux indices
axis(1, at = year_indices1, labels = years1)

# Tracer la deuxi?me p?riode (1991-2024) en bleu (en bas)
plot(time_index2, DataPeriode2$PolicyRate, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Taux directeur (%)",
     main = "?volution du taux directeur (Janvier 1991 - Septembre 2024)",
     xaxt = "n",ylim = c(min_PolicyRate, max_PolicyRate))  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (ann?es)
years2 <- seq(1990, 2024 , by = 10)  # G?n?rer des ann?es tous les 20 ans
year_indices2 <- seq(1, nrow(DataPeriode2), length.out = length(years2))  # Correspond aux indices
axis(1, at = year_indices2, labels = years2)

# R?initialiser les param?tres de la fen?tre graphique
par(mfrow = c(1, 1))



#Tra?age du taux de croissance de dividende
dev.new()
par(mfrow = c(2, 1))  # Diviser la fen?tre en 2 lignes, 1 colonne
min_DividendGrowthRate <- min(DataPeriode1$DividendGrowthRate, DataPeriode2$DividendGrowthRate)
max_DividendGrowthRate <- max(DataPeriode1$DividendGrowthRate, DataPeriode2$DividendGrowthRate)

# Tracer la premi?re p?riode (1881-1990) en bleu (en haut)
plot(time_index1, DataPeriode1$DividendGrowthRate, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Taux de croissance de dividende (%)",
     main = "?volution du taux de croissance de dividende 
     (Janvier 1881 - D?cembre 1990)",
     xaxt = "n",ylim = c(min_DividendGrowthRate, max_DividendGrowthRate))  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (ann?es)
years1 <- seq(1881, 1990, by = 10)  # G?n?rer des ann?es tous les 20 ans
year_indices1 <- seq(1, nrow(DataPeriode1), length.out = length(years1))  # Correspond aux indices
axis(1, at = year_indices1, labels = years1)

# Tracer la deuxi?me p?riode (1991-2024) en bleu (en bas)
plot(time_index2, DataPeriode2$DividendGrowthRate, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Taux de croissance de dividende (%)",
     main = "?volution du taux de croissance de dividende 
     (Janvier 1991 - Septembre 2024)",
     xaxt = "n",ylim = c(min_DividendGrowthRate, max_DividendGrowthRate))  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (ann?es)
years2 <- seq(1990, 2024 , by = 10)  # G?n?rer des ann?es tous les 20 ans
year_indices2 <- seq(1, nrow(DataPeriode2), length.out = length(years2))  # Correspond aux indices
axis(1, at = year_indices2, labels = years2)

# R?initialiser les param?tres de la fen?tre graphique
par(mfrow = c(1, 1))



#Tra?age volatilit?
dev.new()
par(mfrow = c(2, 1))  # Diviser la fen?tre en 2 lignes, 1 colonne
min_Volatility <- min(DataPeriode1$Volatility, DataPeriode2$Volatility)
max_Volatility <- max(DataPeriode1$Volatility, DataPeriode2$Volatility)

# Tracer la premi?re p?riode (1881-1990) en bleu (en haut)
plot(time_index1, DataPeriode1$Volatility, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Volatilit? (%)",
     main = "?volution de la volatilit? 
     (Janvier 1881 - D?cembre 1990)",
     xaxt = "n",ylim = c(min_Volatility, max_Volatility))  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (ann?es)
years1 <- seq(1881, 1990, by = 10)  # G?n?rer des ann?es tous les 20 ans
year_indices1 <- seq(1, nrow(DataPeriode1), length.out = length(years1))  # Correspond aux indices
axis(1, at = year_indices1, labels = years1)

# Tracer la deuxi?me p?riode (1991-2024) en bleu (en bas)
plot(time_index2, DataPeriode2$Volatility, type = "l", col = "blue", lwd = 2,
     xlab = "Temps", ylab = "Volatilit? (%)",
     main = "?volution de la volatilit? 
     (Janvier 1991 - Septembre 2024)",
     xaxt = "n",ylim = c(min_Volatility, max_Volatility))  # Supprime les ticks de l'axe X pour personnaliser
# Ajouter des labels temporels (ann?es)
years2 <- seq(1990, 2024 , by = 10)  # G?n?rer des ann?es tous les 20 ans
year_indices2 <- seq(1, nrow(DataPeriode2), length.out = length(years2))  # Correspond aux indices
axis(1, at = year_indices2, labels = years2)
par(mfrow = c(1, 1))

