library(dplyr)
library(neuralnet) 
#other useful NN packages: RSNNS, NeuralNetTools, nnet, H2O, DARCH, deepnet and mxnet
#if you want to go all-in, https://tensorflow.rstudio.com/



###############################################################################
# Load and prep data
###############################################################################
big_table = read.csv('data/g19_2007_bigtable.csv', as.is=TRUE)

# First, we'll remove all categorial data
num_data = big_table[, sapply(big_table, class) %in% c('integer', 'numeric')]
full_data = na.omit(num_data)

## data for the NN needs to be noramlized. Use `scale`
scale_data = as.data.frame(scale(full_data))
scale_data$VISIT_NO = NULL  #Dropping this variable. Not informative

###############################################################################
# Build model and run neural network
###############################################################################

train_i = sample(1:nrow(scale_data), floor(nrow(scale_data)/2))

train = scale_data[train_i,]
test  = scale_data[-train_i,]

n  = names(train)
f  = as.formula(paste("DOC ~", paste(n[!n %in% "DOC"], collapse = " + ")))

nn = neuralnet(f, data=train, linear.output=T, hidden=c(15, 4))

pr.nn = compute(nn, select(test, -DOC))

RMSE(pr.nn$net.result, test$DOC)

plot(pr.nn$net.result, test$DOC, log='xy')


###############################################################################
# k-fold cross validation (takes a while to run)
###############################################################################

k=10
errs = rep(NA, k)
for(i in 1:k){
  train_i = sample(1:nrow(scale_data), floor(nrow(scale_data)*(k-1)/k))
  
  train = scale_data[train_i,]
  test  = scale_data[-train_i,]
  
  n  = names(train)
  f  = as.formula(paste("DOC ~", paste(n[!n %in% "DOC"], collapse = " + ")))
  
  nn = neuralnet(f, data=train, linear.output=T, hidden=c(15, 4))
  
  pr.nn = compute(nn, select(test, -DOC))
  
  errs[i] = RMSE(pr.nn$net.result, test$DOC)
}

boxplot(errs)

