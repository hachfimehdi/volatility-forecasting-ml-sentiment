
DataPeriode1 <- read_excel("C:/Users/hp/Desktop/M2 ERCF/M?moire/Donn?es/DataPeriode1.xlsx")
DataPeriode2 <- read_excel("C:/Users/hp/Desktop/M2 ERCF/M?moire/Donn?es/DataPeriode2.xlsx")

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

time_index1 <- 1:nrow(DataPeriode1) #nombre des mois dans la p?riode 1
time_index2 <- 1:nrow(DataPeriode2) #nombre des mois dans la p?riode 2

#Tra?age du rendement
dev.new()
par(mfrow = c(2, 1))  # Diviser la fen?tre en 2 lignes, 1 colonne
min_yield <- min(DataPeriode1$Yield, DataPeriode2$Yield)
max_yield <- max(DataPeriode1$Yield, DataPeriode2$Yield)
