library(dplyr)
library(tree)
library(randomForest)


treehouse = read.csv('treehouse.csv')
treehouse = treehouse[-1]
treehouse = data.frame(treehouse, stringsAsFactors = T)

traint = sample(1:nrow(treehouse), 8*nrow(treehouse)/10)


# Initial Random Forest ----
set.seed(0)
rf.treehouse = randomForest(SalePrice_Log ~ ., data = treehouse,
                            subset = traint, importance = TRUE)
rf.treehouse

sqrt(0.01644914)

# Graphing variable importance
importance(rf.treehouse)
varImpPlot(rf.treehouse)
