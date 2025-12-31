
#regression lasso
set.seed(1234) #pour avoir les meme resultats apres les execution
modelLasso1=cv.glmnet(x=as.matrix(x_train1),y=as.matrix(y_train1),
                      type.measure="mse",alpha=1,family="gaussian",nlambda=200)
modelLasso2=cv.glmnet(x=as.matrix(x_train2),y=as.matrix(y_train2),
                      type.measure="mse",alpha=1,family="gaussian",nlambda=200)
dev.new()
par(mfrow = c(2, 1))
plot(modelLasso1,main="1881-1990
     ")
plot(modelLasso2,main="1990-2024
     ")
par(mfrow = c(1, 1))
#afficher lambda
lambdaLasso1 = tail(modelLasso1$lambda, 1)
lambdaLasso1
lambdaLasso2 = tail(modelLasso2$lambda, 1)
lambdaLasso2

modelLasso11=glmnet(x=as.matrix(x_train1),y=as.matrix(y_train1),type.measure="mse",
                    alpha=1,family="gaussian",lambda=lambdaLasso1)
modelLasso22=glmnet(x=as.matrix(x_train2),y=as.matrix(y_train2),type.measure="mse",
                    alpha=1,family="gaussian",lambda=lambdaLasso2)

coef(modelLasso11)
coef(modelLasso22)

predLasso1=predict(modelLasso1,lambdaLasso1,new=as.matrix(x_test1))
predLasso2=predict(modelLasso2,lambdaLasso2,new=as.matrix(x_test2))
erreursLasso1=testML1$Volatility-predLasso1
erreursLasso2=testML2$Volatility-predLasso2

#validation
mseLasso1=mean((testML1$Volatility-predLasso1)^2)
mseLasso1
rmseLasso1=sqrt(mseLasso1)
rmseLasso1
mseLasso2=mean((testML2$Volatility-predLasso2)^2)
mseLasso2
rmseLasso2=sqrt(mseLasso2)
rmseLasso2

