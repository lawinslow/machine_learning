###############################################################################
# load libraries
###############################################################################
library(dplyr)

###############################################################################
# Load data and run simple model of DOC and Total P
###############################################################################
big_table = read.csv('data/g19_2007_bigtable.csv', as.is=TRUE)


## grab just the variables we want, remove missing data rows, and normalize
cluster_vars = select(big_table, DOC, PTL)
full_data    = na.omit(cluster_vars)
norm_data    = as.data.frame(scale(full_data))


cl = kmeans(norm_data, 3)
plot(norm_data, col = cl$cluster)


###############################################################################
# Look at k effect on within group squared error (elbow test)
###############################################################################

ngroups = 2:15
sse = c()
for(i in seq_along(ngroups)){
  
  vardata = select(big_table, DOC, PTL, NTL, CHLA, DEPTHMAX) %>% filter(DOC < 30 & PTL < 400) %>% 
    na.omit %>% scale %>% as.data.frame
  cl = kmeans(vardata, ngroups[i])
  sse[i] = cl$tot.withinss
}

plot(ngroups, sse)

###############################################################################
# Examine more complicated clustering using multiple variables
###############################################################################

cluster_vars = select(big_table, DOC, PTL, NTL, CHLA, DEPTHMAX) %>% filter(DOC < 30 & PTL < 400) %>% na.omit
norm_data = cluster_vars %>% scale %>% as.data.frame

cl = kmeans(vardata, clusters=4) ## try different number of clusters here
plot(cluster_vars, col = cl$cluster) # use non-scaled data for visualization

## look at clusters to see which dominant clusters we have
cl$centers

