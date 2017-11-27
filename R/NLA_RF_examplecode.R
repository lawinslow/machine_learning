###############################################################################
#load libraries
###############################################################################
library(dplyr)
library(randomForest)

###############################################################################
# read in data 
###############################################################################
dat.all = read.csv(file = "https://raw.githubusercontent.com/lawinslow/machine_learning/master/data/g19_2007_bigtable.csv")

# get rid of data that don't make sense (ex. lake name, site ID, categories)
dat = select(dat.all, -SITE_ID, -VISIT_NO, -LAKENAME, -DATE_COL, -TOC
             , -TROPHIC_STATE, -RECREATIONAL_VALUE, -BIOTIC_INTEGRITY
             , -HYDRO_TYPE, -OUTLET_DAMS, -SWIMMABILITY, -MBOAT_DENSITY)

# create the RF model
RF1 = randomForest(dat$DOC ~ ., data = dat
#, mtry =  # default for regression is p/3, classification is sqrt(p)
, importance = TRUE
, na.action = na.omit # if you use the rough.fix or impute here you want to make sure you specify it elsewhere
, ntree = 1000
)

# examine the model object
RF1

# shows the MSE for all trees in the forest (should stabilize)
plot(RF1)

###############################################################################
# creates a plot of variable importance
# set scale = FALSE, setting scale = TRUE provides a z-score but this is not 
# recommended because it has odd statistical properties and can inflate variable 
# importance of highly correlated predictors. See Strobl and Zeileis (2008).
###############################################################################
varImpPlot(RF1, scale = FALSE)

###############################################################################
# create partial dependency plots for predictors of interest, you can plot an
# predictor you want. Pay attention to the scale of the y-axis, they won't cover 
# a wide range for unimportanace variables.
###############################################################################
partialPlot(x = RF1, pred.data = dat, x.var = K, n.pt = 50)
partialPlot(x = RF1, pred.data = dat, x.var = NTL, n.pt = 50)
partialPlot(x = RF1, pred.data = dat, x.var = ANC, n.pt = 50)

################################################################################
# Making prettier figures. Not necessary for analysis but helpful for publication
################################################################################

### Variable Importance
# extract variable importance metrics
xx = importance(RF1, type=1, scale =  FALSE)
yy = as.vector(dimnames(xx))
VarImport = data.frame(Var = unlist(as.vector(yy[1]))
                          , Per.IncMSE = as.vector(xx[,1]))
VarImport$Var = as.character(VarImport$Var)

# plot the variable importance
ggplot(data = VarImport, aes(y = reorder(Var, Per.IncMSE), x = Per.IncMSE))+
        geom_point(size = 3, fill = "black") +
        labs(y = "", x = "Variable Importance \n(Relative Increase in MSE)")+
        theme_bw()+
        theme(panel.border = element_blank()
      , panel.background = element_blank()
      , panel.grid.minor = element_blank()
      , panel.grid.major.x = element_blank()
      , panel.grid.major.y = element_line(color = "grey50", size = 0.25, linetype = "dotted")
      , axis.line.x = element_line(colour = "black")
      , axis.line.y = element_line(colour = "black")
        )

### Partial dependencies
# extract the values (this is slow - best to just select the variables of interest)
Pd.plot.data = data.frame()
RF.x = RF1 # name of RF object to be used
pred.data = na.roughfix(dat) # should match the na.action used in the model.

for(i in 1:nrow(VarImport)){
      # run partialPlot function from randomForest
      tmp1 = partialPlot(x = RF1, pred.data = pred.data, x.var = VarImport[i, "Var"], n.pt = 25, plot = FALSE)
      y   = unlist(tmp1$y) #values for the y axis
      x   = unlist(tmp1$x) # values for the x axis
      # write x.forplots and y to a data frame
      var = rep(as.character(VarImport[i, "Var"]), length(y)) # name of variable
      tmp2 = data.frame(var = var, y = y, x = x)
      Pd.plot.data = rbind(Pd.plot.data, tmp2)
} # end for loop

################################################################################
# PRETTIER PLOTS
################################################################################
library(ggplot2)
# plot the variable importance
varim = ggplot(data = VarImport, aes(y = reorder(Var, Per.IncMSE), x = Per.IncMSE))+
        geom_point(size = 3, fill = "black") +
        labs(y = "", x = "Variable Importance \n(Relative Increase in MSE)")+
        theme_bw()+
        theme(panel.border = element_blank()
      , panel.background = element_blank()
      , panel.grid.minor = element_blank()
      , panel.grid.major.x = element_blank()
      , panel.grid.major.y = element_line(color = "grey50", size = 0.25, linetype = "dotted")
      , axis.line.x = element_line(colour = "black")
      , axis.line.y = element_line(colour = "black")
        )
  

# plot the partial dependencies

Pd = ggplot(data = Pd.plot.data , aes(x, y))+
        geom_line()+
        #facet_wrap(~ var, ncol = 2)+
        labs(x = "", y = "Mean Response (m)")+
        theme_bw()+
        theme(panel.grid.major.y = element_line(color = "grey50", size = 0.25, linetype = "dotted")
        , panel.grid.major.x = element_blank()
        , panel.grid.minor = element_blank()
        , strip.background = element_blank()
        , axis.line = element_line(colour = "black")
        )





