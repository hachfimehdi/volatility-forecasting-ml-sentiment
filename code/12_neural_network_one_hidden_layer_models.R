
#Reseaux de neuronnes
# install library
install.packages("neuralnet")


library(neuralnet)

# Normalisation des donn?es (Min-Max Scaling)
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Appliquer la normalisation sur les variables explicatives et la cible
trainML1_norm <- as.data.frame(lapply(trainML1, normalize))
testML1_norm <- as.data.frame(lapply(testML1, normalize))
trainML2_norm <- as.data.frame(lapply(trainML2, normalize))
testML2_norm <- as.data.frame(lapply(testML2, normalize))

#une seule couche cach?e
dfNN1 = data.frame(NbrNeurones = rep(0, 12), mseNN1 = rep(0, 12))
dfNN2 = data.frame(NbrNeurones = rep(0, 12), mseNN2 = rep(0, 12))
for(i in 1:12){
  set.seed(432)  # Assurer la reproductibilit?
  
  modelNN1 = neuralnet(Volatility ~ DividendGrowthRate + PolicyRate + InterestRate + Yield,
                       data = trainML1_norm, hidden = i, linear.output = TRUE)
  
  modelNN2 = neuralnet(Volatility ~ DividendGrowthRate + PolicyRate + InterestRate + Yield,
                       data = trainML2_norm, hidden = i, linear.output = TRUE)
  
  # Pr?dictions
  predNN1 = compute(modelNN1, testML1_norm[, c("DividendGrowthRate", "PolicyRate", "InterestRate", "Yield")])$net.result
  predNN2 = compute(modelNN2, testML2_norm[, c("DividendGrowthRate", "PolicyRate", "InterestRate", "Yield")])$net.result
  
  # D?normalisation des pr?dictions
  minV1 = min(trainML1$Volatility)
  maxV1 = max(trainML1$Volatility)
  predNN1 = predNN1 * (maxV1 - minV1) + minV1
  
  minV2 = min(trainML2$Volatility)
  maxV2 = max(trainML2$Volatility)
  predNN2 = predNN2 * (maxV2 - minV2) + minV2
  
  # Calcul du MSE
  mse1 = mean((testML1$Volatility - predNN1)^2)
  mse2 = mean((testML2$Volatility - predNN2)^2)
  
  # Stocker les r?sultats
  dfNN1$NbrNeurones[i] = i
  dfNN1$mseNN1[i] = mse1
  dfNN2$NbrNeurones[i] = i
  dfNN2$mseNN2[i] = mse2
}
dev.new()
par(mfrow = c(2, 1))
plot(dfNN1$NbrNeurones, dfNN1$mseNN1, type = "b", col = "blue", pch = 19,
     xlab = "Nombre de neurones cach?s", ylab = "MSE",
     main = "MSE en fonction du nombre de neurones cach?s
            1881-1990", xaxt = "n")
axis(1, at = dfNN1$NbrNeurones, labels = dfNN1$NbrNeurones)

plot(dfNN2$NbrNeurones, dfNN2$mseNN2, type = "b", col = "blue", pch = 19,
     xlab = "Nombre de neurones cach?s", ylab = "MSE",
     main = "MSE en fonction du nombre de neurones cach?s
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

