
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
        xlab = "Temps", ylab = "Erreur Normalis?e", 
        main = "Comparaison des erreurs des mod?les")

# Ajouter la l?gende
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
        xlab = "Temps", ylab = "Erreur Normalis?e", 
        main = "Comparaison des erreurs des mod?les")

# Ajouter la l?gende
legend("topright", legend = colnames(mat_erreurs2), col = 1:ncol(mat_erreurs2), 
       lty = 1, cex = 0.8)


#DM TEST p?riode 1
modeles1 <- colnames(EcartReelPrediction1)

# Initialisation de la matrice des p-values du test Diebold-Mariano
dm_matrix1 <- matrix(NA, nrow = length(modeles1), ncol = length(modeles1))
rownames(dm_matrix1) <- modeles1
colnames(dm_matrix1) <- modeles1

# Boucles pour comparer chaque mod?le avec les autres
for (i in 1:(length(modeles1) - 1)) {
  for (j in (i + 1):length(modeles1)) {
    # les erreurs de chaque mod?le
    erreur1 <- EcartReelPrediction1[[modeles1[i]]]
    erreur2 <- EcartReelPrediction1[[modeles1[j]]]
    
    # V?rifier si les erreurs sont identiques
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



#DM TEST p?riode 2
modeles2 <- colnames(EcartReelPrediction2)

# Initialisation de la matrice des p-values du test Diebold-Mariano
dm_matrix2 <- matrix(NA, nrow = length(modeles2), ncol = length(modeles2))
rownames(dm_matrix2) <- modeles2
colnames(dm_matrix2) <- modeles2

# Boucles pour comparer chaque mod?le avec les autres
for (i in 1:(length(modeles2) - 1)) {
  for (j in (i + 1):length(modeles2)) {
    # les erreurs de chaque mod?le
    erreur1 <- EcartReelPrediction2[[modeles2[i]]]
    erreur2 <- EcartReelPrediction2[[modeles2[j]]]
    
    # V?rifier si les erreurs sont identiques
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
