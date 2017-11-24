library(neuralnet)
library(e1071)

num_data = big_table[, sapply(big_table, class) %in% c('integer', 'numeric')]
full_data = na.omit(num_data)

n  = names(full_data)
f  = as.formula(paste("MCYST_TL_UGL ~", paste(n[!n %in% "MCYST_TL_UGL"], collapse = " + ")))
nn = neuralnet(f, data=full_data, linear.output=T, hidden=c(20, 5))


pr.nn <- compute(nn,test_[,1:13])

pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

print(paste(MSE.lm,MSE.nn))

par(mfrow=c(1,2))

plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)