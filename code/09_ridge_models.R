
##regression Ridge
install.packages("glmnet")
library(glmnet)
set.seed(12)
modelRidge1=cv.glmnet(x=as.matrix(x_train1),y=as.matrix(y_train1),type.measure="mse",
                      alpha=0,family="gaussian",nlambda=200)
modelRidge2=cv.glmnet(x=as.matrix(x_train2),y=as.matrix(y_train2),type.measure="mse",
                      alpha=0,family="gaussian",nlambda=200)
dev.new()
par(mfrow = c(2, 1))
plot(modelRidge1,main="1881-1990
     ")
plot(modelRidge2,main="1990-2024
     ")
par(mfrow = c(1, 1))

#afficher lambda
lambdaRidge1=tail(modelRidge1$lambda,1)
lambdaRidge1
lambdaRidge2=tail(modelRidge2$lambda,1)
lambdaRidge2
set.seed(421)
modelRidge11=glmnet(x=as.matrix(x_train1),y=as.matrix(y_train1),type.measure="mse",
                    alpha=0,family="gaussian",lambda=lambdaRidge1)
modelRidge22=glmnet(x=as.matrix(x_train2),y=as.matrix(y_train2),type.measure="mse",
                    alpha=0,family="gaussian",lambda=lambdaRidge2)
coef(modelRidge11)
coef(modelRidge22)
#prediction
testML1$pred=predict(modelRidge1,lambdaRidge1,new=as.matrix(x_test1))
testML2$pred=predict(modelRidge2,lambdaRidge2,new=as.matrix(x_test2))
erreursRidge1=testML1$Volatility-testML1$pred
erreursRidge2=testML2$Volatility-testML2$pred

#mse
mseRidge1=mean((testML1$Volatility-testML1$pred)^2)
mseRidge1
mseRidge2=mean((testML2$Volatility-testML2$pred)^2)
mseRidge2
#rmse
rmseRidge1=sqrt(mseRidge1)
rmseRidge1
rmseRidge2=sqrt(mseRidge2)
rmseRidge2

