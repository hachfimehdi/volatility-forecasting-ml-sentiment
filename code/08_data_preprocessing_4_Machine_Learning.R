
##Partie Machine learning
data1dif1 <- as.data.frame(lapply(dataset1, diff))
data2dif1 <- as.data.frame(lapply(dataset2, diff))
#diviser data en train et test

TrainIndexML1=createDataPartition(data1dif1$Volatility,p=0.8,list=FALSE)
trainML1=data1dif1[TrainIndexML1,]
testML1=data1dif1[-TrainIndexML1,]
TrainIndexML2=createDataPartition(data2dif1$Volatility,p=0.8,list=FALSE)
trainML2=data2dif1[TrainIndexML2,]
testML2=data2dif1[-TrainIndexML2,]

#enlever la colonne volatilit? (pour que le mod?le ne prend pas les valeur reelles de la volatilit?)
x_train1=scale(subset(trainML1,select=-c(5)))
y_train1=trainML1$Volatility
x_test1=scale(subset(testML1,select=-c(5)))
y_test1=trainML1$Volatility
x_train2=scale(subset(trainML2,select=-c(5)))
y_train2=trainML2$Volatility
x_test2=scale(subset(testML2,select=-c(5)))
y_test2=trainML2$Volatility
