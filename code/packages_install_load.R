
#Installer et charger tous les packages nécessaires


# Liste des packages à installer
packages <- c(
  "TSstudio", "dplyr", "readxl", "FinTS", "caret",
  "glmnet", "forecast", "corrplot", "pillar",
  "rugarch", "neuralnet", "DescTools"
)

# Installer les packages manquants
installed_packages <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Charger tous les packages
library(TSstudio)
library(dplyr)
library(readxl)
library(FinTS)
library(caret)
library(glmnet)
library(forecast)
library(corrplot)
library(pillar)
library(rugarch)
library(neuralnet)
library(DescTools)
