library(caret)
library(AppliedPredictiveModeling)
### Chapter 10: Case Study: Compressive Strength of Concrete Mixtures
###
### Load the data and plot the data

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
library(plyr)
featurePlot(concrete[, -9], concrete$CompressiveStrength,
            between = list(x = 1, y = 1),
            type = c("g", "p", "smooth"))
### Section 10.1 Model Building Strategy
### There are replicated mixtures, so take the average per mixture

averaged <- ddply(mixtures,
                  .(Cement, BlastFurnaceSlag, FlyAsh, Water, 
                    Superplasticizer, CoarseAggregate, 
                    FineAggregate, Age),
                  function(x) c(CompressiveStrength = 
                                  mean(x$CompressiveStrength)))

### Split the data and create a control object for train()

set.seed(975)
inTrain <- createDataPartition(averaged$CompressiveStrength, p = 3/4)[[1]]
training <- averaged[ inTrain,]
testing  <- averaged[-inTrain,]

ctrl <- trainControl(method = "repeatedcv", repeats = 5, number = 10)

### Create a model formula that can be used repeatedly

modForm <- paste("CompressiveStrength ~ (.)^2 + I(Cement^2) + I(BlastFurnaceSlag^2) +",
                 "I(FlyAsh^2)  + I(Water^2) + I(Superplasticizer^2)  +",
                 "I(CoarseAggregate^2) +  I(FineAggregate^2) + I(Age^2)")
modForm <- as.formula(modForm)
#

