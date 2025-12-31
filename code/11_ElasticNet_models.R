
#regression elastic net
alpha1=seq(0,1,0.001)
df1=data.frame(alpha1)
df1$mseElasticNet=0
alpha2=seq(0,1,0.001)
df2=data.frame(alpha2)
df2$mseElasticNet=0

for(i in 1:dim(df1)[1]){
  modelElasticNet1=cv.glmnet(x=as.matrix(x_train1),y=as.matrix(y_train1),
                             type.measure="mse",alpha=df1$alpha1[i],
                             family="gaussian",nlambda=200)
  predElasticNet1=predict(modelElasticNet1,tail(modelElasticNet1$lambda,1),
                          new=as.matrix(x_test1))
  mse=mean((testML1$Volatility-predElasticNet1)^2)
  df1$mseElasticNet[i]=mse
}

for(i in 1:dim(df2)[1]){
  modelElasticNet2=cv.glmnet(x=as.matrix(x_train2),y=as.matrix(y_train2),
                             type.measure="mse",alpha=df2$alpha2[i],
                             family="gaussian",nlambda=200)
  predElasticNet2=predict(modelElasticNet2,tail(modelElasticNet2$lambda,1),
                          new=as.matrix(x_test2))
  mse=mean((testML2$Volatility-predElasticNet2)^2)
  df2$mseElasticNet[i]=mse
}

#trier df
library(dplyr)
df1=arrange(df1,mseElasticNet)
df2=arrange(df2,mseElasticNet)
dev.new()
par(mfrow = c(2, 1))
plot(df1,main="1881-1990
     ")
plot(df2,main="1990-2024
     ")
par(mfrow = c(1, 1))
ElasticNet1=cv.glmnet(x=as.matrix(x_train1),y=as.matrix(y_train1),type.measure="mse",
                      alpha=df1$alpha1[1],family="gaussian",nlambda=200)
ElasticNet2=cv.glmnet(x=as.matrix(x_train2),y=as.matrix(y_train2),type.measure="mse",
                      alpha=df2$alpha2[1],family="gaussian",nlambda=200)
predElasticNet1=predict(modelElasticNet1,tail(ElasticNet1$lambda,1),
                        new=as.matrix(x_test1))
predElasticNet2=predict(modelElasticNet2,tail(ElasticNet2$lambda,1),
                        new=as.matrix(x_test2))
erreursElasticNet1=testML1$Volatility-predElasticNet1
erreursElasticNet2=testML2$Volatility-predElasticNet2

dev.new()
par(mfrow = c(2, 1))
plot(ElasticNet1,main="1881-1990
     ")
plot(ElasticNet2,main="1990-2024
     ")
par(mfrow = c(1, 1))
LambdaElasticNet1=tail(ElasticNet1$lambda,1)
LambdaElasticNet1
LambdaElasticNet2=tail(ElasticNet2$lambda,1)
LambdaElasticNet2

ElasticNet11=glmnet(x=as.matrix(x_train1),y=as.matrix(y_train1),type.measure="mse",
                    alpha=df1$alpha1[1],family="gaussian",lambda=LambdaElasticNet1)
ElasticNet22=glmnet(x=as.matrix(x_train2),y=as.matrix(y_train2),type.measure="mse",
                    alpha=df2$alpha2[1],family="gaussian",lambda=LambdaElasticNet2)

coef(ElasticNet11)
coef(ElasticNet22)
#predElasticNet1=predict(ElasticNet1,ElasticNet1$lambda.min,new=as.matrix(x_test1))
#predElasticNet2=predict(ElasticNet2,ElasticNet2$lambda.min,new=as.matrix(x_test2))

mseElasticNet1=df1$mseElasticNet[1]
mseElasticNet1
rmseElasticNet1=sqrt(mseElasticNet1)
rmseElasticNet1
mseElasticNet2=df2$mseElasticNet[1]
mseElasticNet2
rmseElasticNet2=sqrt(mseElasticNet2)
rmseElasticNet2
