
dataset1=subset(DataPeriode1,select=-c(1)) #enlever la colonne DATE
dataset2=subset(DataPeriode2,select=-c(1)) #enlever la colonne DATE
#matrice de corr?lation
install.packages("corrplot")
library(corrplot) 
# Calculer les matrices de corr?lation et les convertir en pourcentage
correlation_matrix1 <- cor(dataset1, use = "complete.obs", method = "pearson") 
correlation_matrix2 <- cor(dataset2, use = "complete.obs", method = "pearson") 

dev.new()
par(mfrow = c(2, 1))

# Premier graphique pour la premi?re p?riode
corrplot(correlation_matrix1, 
         method = "number", type = "upper",addCoef.col = "black", tl.col = "black",
         number.cex = 0.9, cl.pos = "n", col = colorRampPalette(c("black"))(200))
title("Janvier 1881 - D?cembre 1990                                                                     ",
      line = 1.2)

# Deuxi?me graphique pour la deuxi?me p?riode
corrplot(correlation_matrix2, 
         method = "number", type = "upper", addCoef.col = "black",tl.col = "black",
         number.cex = 0.9, cl.pos = "n", col = colorRampPalette(c("black"))(200))
title("Janvier 1991 - Septembre 2024                                                                     ", 
      line = 1.2)
par(mfrow = c(1, 1))
