
library(dplyr)
library(e1071)
library(class)
library(caret)

###############################################################################
# Load and prep data
###############################################################################
d = read.csv('data/g19_2007_bigtable.csv', as.is=TRUE)

d = filter(d, TROPHIC_STATE != '') %>% mutate(TROPHIC_STATE = factor(TROPHIC_STATE)) %>% na.omit

# trophic_info = d %>% select(TROPHIC_STATE, NTL, PTL, CHLA, TURB, DO_SURF, DOC) %>%
#   filter(TROPHIC_STATE != '') %>% na.omit
# trophic_info$TROPHIC_STATE = factor(trophic_info$TROPHIC_STATE)

troph_lu = select(d, TROPHIC_STATE, ends_with('_BSN'))


###############################################################################
# Split data into training and testing datasets
###############################################################################
train_i = sample(1:nrow(troph_lu), size = floor(nrow(troph_lu)/2))

train = troph_lu[train_i, ]
test = troph_lu[-train_i, ]


model_svm <- svm(TROPHIC_STATE ~ . , train, scale=TRUE)

table(predict(model_svm, train), train$TROPHIC_STATE)

table(predict(model_svm, test), test$TROPHIC_STATE)

###############################################################################
# Use `tune` to guestimate best gamma and cost parameters
###############################################################################
svm_tune = tune(svm, train.x=TROPHIC_STATE~., data=train, 
              kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(0.1,0.2,0.5,1,2)))

svm_tune

##look at the confusion matrix for the best model
table(predict(svm_tune$best.model, train), train$TROPHIC_STATE)

mean(predict(svm_tune$best.model, train) == train$TROPHIC_STATE)

table(predict(svm_tune$best.model, test), test$TROPHIC_STATE)
mean(predict(svm_tune$best.model, test) == test$TROPHIC_STATE)


###############################################################################
# Try a different set of parameters
###############################################################################

trophic_info = d %>% select(TROPHIC_STATE, NTL, PTL, CHLA, TURB, DO_SURF, DOC) %>%
  filter(TROPHIC_STATE != '') %>% na.omit

#split in half
train_i = sample(1:nrow(trophic_info), size = floor(nrow(trophic_info)/2))

train = trophic_info[train_i, ]
test = trophic_info[-train_i, ]

model_svm <- svm(TROPHIC_STATE ~ . , train, scale=TRUE)


table(predict(model_svm, train), train$TROPHIC_STATE)
mean(predict(model_svm, train) == train$TROPHIC_STATE)

table(predict(model_svm, test), test$TROPHIC_STATE)
mean(predict(model_svm, test) == test$TROPHIC_STATE)


