library(dplyr)
library(class)
library(caret)

###############################################################################
# Load and prep data
###############################################################################
d = read.csv('data/g19_2007_bigtable.csv', as.is=TRUE)

trophic_info = d %>% select(TROPHIC_STATE, NTL, PTL, CHLA, TURB, DO_SURF, DOC) %>%
  filter(TROPHIC_STATE != '') %>% na.omit
trophic_info$TROPHIC_STATE = factor(trophic_info$TROPHIC_STATE)


###############################################################################
# Split data into training and testing datasets
###############################################################################
train_i = sample(1:nrow(trophic_info), size = floor(nrow(trophic_info)/2))

train = trophic_info[train_i, ]
test = trophic_info[-train_i, ]


###############################################################################
# Run against k-nearest neighbors and examing results
###############################################################################

## number of K can be changed for different results
est = knn(train[,-1], test[,-1], train$TROPHIC_STATE, k = 3, prob=FALSE)  

confusionMatrix(data=est, test$TROPHIC_STATE)

View(data.frame(data=est, reference=test$TROPHIC_STATE))
