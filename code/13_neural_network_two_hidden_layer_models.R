
#deux couches cach?es
dfNN11 <- data.frame(NbrNeurones1 = integer(), NbrNeurones2 = integer(), mseNN1 = numeric())
dfNN22 <- data.frame(NbrNeurones1 = integer(), NbrNeurones2 = integer(), mseNN2 = numeric())

for(i in 1:6){  # Premi?re couche cach?e : 1 ? 6 neurones
  for(j in 1:6){  # Deuxi?me couche cach?e : 1 ? 6 neurones
    set.seed(242)  
    
    modelNN11 = neuralnet(Volatility ~ DividendGrowthRate + PolicyRate + InterestRate + Yield,
                          data = trainML1_norm, hidden = c(i, j), linear.output = TRUE)
    modelNN22 = neuralnet(Volatility ~ DividendGrowthRate + PolicyRate + InterestRate + Yield,
                          data = trainML2_norm, hidden = c(i, j), linear.output = TRUE)
    
    # Pr?dictions
    predNN11 = compute(modelNN11, testML1_norm[, c("DividendGrowthRate", "PolicyRate", "InterestRate", "Yield")])$net.result
    predNN22 = compute(modelNN22, testML2_norm[, c("DividendGrowthRate", "PolicyRate", "InterestRate", "Yield")])$net.result
    
    # D?normalisation des pr?dictions
    minV11 = min(trainML1$Volatility)
    maxV11 = max(trainML1$Volatility)
    predNN11 = predNN11 * (maxV11 - minV11) + minV11
    
    minV22 = min(trainML2$Volatility)
    maxV22 = max(trainML2$Volatility)
    predNN22 = predNN22 * (maxV22 - minV22) + minV22
    
    # Calcul du MSE
    mse1 = mean((testML1$Volatility - predNN11)^2)
    mse2 = mean((testML2$Volatility - predNN22)^2)
    
    # Stocker les r?sultats
    dfNN11[nrow(dfNN11) + 1, ] <- c(i, j, mse1)
    dfNN22[nrow(dfNN22) + 1, ] <- c(i, j, mse2)
  }
}

# Cr?er une nouvelle variable combin?e pour les ?tiquettes de l'axe X
dfNN11$CombinedLabels <- paste("(", dfNN11$NbrNeurones1, ",", dfNN11$NbrNeurones2, ")")
dfNN22$CombinedLabels <- paste("(", dfNN22$NbrNeurones1, ",", dfNN22$NbrNeurones2, ")")

dev.new()
par(mfrow = c(2, 1))
# Tracer le graphique en utilisant les ?tiquettes combin?es
plot(dfNN11$NbrNeurones1, dfNN11$mseNN1, type = "b", col = "blue", pch = 19,
     xlab = "Couches cach?es", ylab = "MSE",
     main = "MSE en fonction du nombre de neurones cach?s
        1881-1990", xaxt = "n")

# Personnaliser les ?tiquettes de l'axe X
axis(1, at = dfNN11$NbrNeurones1, labels = dfNN11$CombinedLabels)

plot(dfNN22$NbrNeurones1, dfNN22$mseNN2, type = "b", col = "blue", pch = 19,
     xlab = "Couches cach?es", ylab = "MSE",
     main = "MSE en fonction du nombre de neurones cach?s
        1991-2024", xaxt = "n")

# Personnaliser les ?tiquettes de l'axe X
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
     main = "Valeurs R?elles vs Pr?dites", pch = 16)

# Ajouter les valeurs pr?dites
lines(predNN2, type = "o", col = "red", pch = 16)

# Ajouter une l?gende
legend("topleft", legend = c("Valeurs R?elles", "Valeurs Pr?dites"),
       col = c("blue", "red"), lty = 1, pch = 16)
