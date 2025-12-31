

# Comparaison
erreursP1 <- data.frame(Mod?les=rep(0,2),MSE1 = rep(0, 2), RMSE1 = rep(0, 2),
                        MSE2 = rep(0, 2), RMSE2 = rep(0, 2))
erreursP1[1, ] <- c("ARIMA", mseARIMA1/10000,rmseARIMA1/10000,mseARIMA2/10000,rmseARIMA2/10000)
erreursP1[2, ] <- c("ARIMA-EGARCH", mseARIMAEGARCH1/10000,rmseARIMAEGARCH1/10000,mseARIMAEGARCH2/10000,rmseARIMAEGARCH2/10000)


erreursP2 <- data.frame(Mod?les=rep(0,5),MSE1 = rep(0, 5), RMSE1 = rep(0, 5),
                        MSE2 = rep(0, 5), RMSE2 = rep(0, 5))
erreursP2[1, ] <- c("Ridge", mseRidge1/10000,rmseRidge1/10000,mseRidge2/10000,rmseRidge2/10000)
erreursP2[2, ] <- c("Lasso", mseLasso1/10000,rmseLasso1/10000,mseLasso2/10000,rmseLasso2/10000)
erreursP2[3, ] <- c("ElasticNet", mseElasticNet1/10000,rmseElasticNet1/10000,mseElasticNet2/10000,rmseElasticNet2/10000)
erreursP2[4, ] <- c("NN1", dfNN1$mseNN1[1]/10000,sqrt(dfNN1$mseNN1[1])/10000,dfNN2$mseNN2[1]/10000,sqrt(dfNN2$mseNN2[1])/10000)
erreursP2[5, ] <- c("NN2", dfNN11$mseNN1[1]/10000,sqrt(dfNN11$mseNN1[1])/10000,dfNN22$mseNN2[1]/10000,sqrt(dfNN22$mseNN2[1])/10000)

erreurs <- data.frame(Mod?les=rep(0,7),MSE1 = rep(0, 7), RMSE1 = rep(0, 7),
                      MSE2 = rep(0, 7), RMSE2 = rep(0, 7))
erreurs[1, ] <- c("ARIMA", mseARIMA1/10000,rmseARIMA1/10000,mseARIMA2/10000,rmseARIMA2/10000)
erreurs[2, ] <- c("ARIMA-EGARCH", mseARIMAEGARCH1/10000,rmseARIMAEGARCH1/10000,mseARIMAEGARCH2/10000,rmseARIMAEGARCH2/10000)
erreurs[3, ] <- c("Ridge", mseRidge1/10000,rmseRidge1/10000,mseRidge2/10000,rmseRidge2/10000)
erreurs[4, ] <- c("Lasso", mseLasso1/10000,rmseLasso1/10000,mseLasso2/10000,rmseLasso2/10000)
erreurs[5, ] <- c("ElasticNet", mseElasticNet1/10000,rmseElasticNet1/10000,mseElasticNet2/10000,rmseElasticNet2/10000)
erreurs[6, ] <- c("NN1", dfNN1$mseNN1[1]/10000,sqrt(dfNN1$mseNN1[1])/10000,dfNN2$mseNN2[1]/10000,sqrt(dfNN2$mseNN2[1])/10000)
erreurs[7, ] <- c("NN2", dfNN11$mseNN1[1]/10000,sqrt(dfNN11$mseNN1[1])/10000,dfNN22$mseNN2[1]/10000,sqrt(dfNN22$mseNN2[1])/10000)

