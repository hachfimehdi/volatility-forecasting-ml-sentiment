
# Charger la librairie pour le test de Levene
install.packages("DescTools")
library(DescTools)


# Initialiser les matrices pour stocker les p-values
models <- colnames(EcartReelPrediction2)
n <- length(models)

# Matrices de r?sultats
wilcoxon_matrix <- matrix(NA, n, n, dimnames = list(models, models))
levene_matrix <- matrix(NA, n, n, dimnames = list(models, models))

# Boucles for pour comparer chaque mod?le avec les autres
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    model1 <- models[i]
    model2 <- models[j]
    
    # Test de Wilcoxon
    wilcox_test <- wilcox.test(EcartReelPrediction2[[model1]], EcartReelPrediction2[[model2]], paired = TRUE)
    wilcoxon_matrix[i, j] <- wilcox_test$p.value
    wilcoxon_matrix[j, i] <- wilcox_test$p.value  # Sym?trique
    
    # Test de Levene
    group <- factor(rep(c(model1, model2), each = nrow(EcartReelPrediction2)))
    errors <- c(EcartReelPrediction2[[model1]], EcartReelPrediction2[[model2]])
    levene_test <- leveneTest(errors ~ group)
    
    levene_matrix[i, j] <- levene_test$`Pr(>F)`[1]
    levene_matrix[j, i] <- levene_test$`Pr(>F)`[1]  # Sym?trique
  }
}

# Afficher les matrices de r?sultats
print("Matrice des p-values du test de Wilcoxon:")
print(wilcoxon_matrix)

print("Matrice des p-values du test de Levene:")
print(levene_matrix)