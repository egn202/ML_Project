## Setup ----
library(dplyr)

housing = read.csv('treehouse.csv')



# housing %>%
#   select(GrLivArea, SalePrice, MSSubClass, LotFrontage, LotArea, OverallQual, OverallCond,
#          YearBuilt, YearRemodAdd, MasVnrArea, BsmtFinSF1, BsmtFinSF2, BsmtUnfSF,
#          X1stFlrSF, X2ndFlrSF, BsmtFullBath, BsmtHalfBath, FullBath, HalfBath,
#          BedroomAbvGr, KitchenAbvGr, TotRmsAbvGrd, Fireplaces, GarageYrBlt, GarageCars,
#          GarageArea, WoodDeckSF, OpenPorchSF, EnclosedPorch, X3SsnPorch, ScreenPorch,
#          PoolArea, MiscVal, MoSold, YrSold) -> housing

summary(housing)
sapply(housing, sd)
cor(housing)

## Dealing with NA's ----
# mean(housing$LotFrontage, na.rm = TRUE)
# housing$LotFrontage = ifelse(is.na(housing$LotFrontage), mean(housing$LotFrontage, na.rm = TRUE), 
#                              housing$LotFrontage)
# 
# housing$MasVnrArea = ifelse(is.na(housing$MasVnrArea), 0, housing$MasVnrArea)
# housing$BsmtFinSF1 = ifelse(is.na(housing$BsmtFinSF1), 0, housing$BsmtFinSF1)
# housing$BsmtFinSF2 = ifelse(is.na(housing$BsmtFinSF2), 0, housing$BsmtFinSF2)
# housing$BsmtUnfSF = ifelse(is.na(housing$BsmtUnfSF), 0, housing$BsmtUnfSF)
# housing$TotalBsmtSF = ifelse(is.na(housing$TotalBsmtSF), 0, housing$TotalBsmtSF)
# housing$LowQualFinSF = ifelse(is.na(housing$LowQualFinSF), 0, housing$LowQualFinSF)
# housing$BsmtFullBath = ifelse(is.na(housing$BsmtFullBath), 0, housing$BsmtFullBath)
# housing$BsmtHalfBath = ifelse(is.na(housing$BsmtHalfBath), 0, housing$BsmtHalfBath)
# housing$GarageYrBlt = ifelse(is.na(housing$GarageYrBlt), 0, housing$GarageYrBlt)
# housing$GarageCars = ifelse(is.na(housing$GarageCars), 0, housing$GarageCars)
# housing$GarageArea = ifelse(is.na(housing$GarageArea), 0, housing$GarageArea)

# LowQualFinSF and TotalBsmtSF are showing NA's but no NA's..

# housing$SalePrice = log(housing$SalePrice)

full.model = lm(SalePrice_Log ~ ., data = housing)

summary(full.model)

## vif's ----
library(car)
vif(full.model)



#avPlots(full.model)

## Stepwise ----

housing.empty = lm(SalePrice_Log ~ 1, data = housing) #The model with an intercept ONLY.
housing.full = lm(SalePrice_Log ~ ., data = housing) #The model with ALL variables.
scope = list(
  lower = formula(housing.empty),
  upper = formula(housing.full))

library(MASS) #The Modern Applied Statistics library.

#Stepwise regression using AIC as the criteria (the penalty k = 2).
bothAIC.empty = step(housing.empty, scope, direction = "both", k = 2)
bothAIC.full = step(housing.full, scope, direction = "both", k = 2)

summary(bothAIC.empty)

## Ridge ----
library(glmnet)
library(ISLR)

x = model.matrix(SalePrice_Log ~ ., housing)[, -1] #Dropping the intercept column.
y = housing$SalePrice_Log

#Values of lambda over which to check.
grid = 10^seq(10, -3, length = 100)

set.seed(0)
train = sample(1:nrow(x), 7*nrow(x)/10)
test = (-train)
y.test = y[test]

length(train)/nrow(x)
length(y.test)/nrow(x)

ridge.models.train = glmnet(x[train, ], y[train], alpha = 0, lambda = grid)
ridge.lambda5 = predict(ridge.models.train, s = 5, newx = x[test, ])
mean((ridge.lambda5 - y.test)^2)


cv.ridge.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 0, nfolds = 10)
plot(cv.ridge.out, main = 'Ridge Regression')
bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge
log(bestlambda.ridge)

ridge.bestlambdatrain = predict(ridge.models.train, s = bestlambda.ridge, newx = x[test, ])
sqrt(mean((ridge.bestlambdatrain - y.test)^2))


library(caret)
set.seed(0)
train_control = trainControl(method = 'cv', number=10)
tune.grid = expand.grid(lambda = grid, alpha=c(0))
ridge.caret = train(x[train, ], y[train],
                    method = 'glmnet',
                    trControl = train_control, tuneGrid = tune.grid)

plot(ridge.caret, xTrans = log)

pred = predict.train(ridge.caret, newdata = x[test, ])
sqrt(mean((pred - y[test])^2))

## Lasso ----

lasso.models.train = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
lasso.lambda5 = predict(lasso.models.train, s = 5, newx = x[test, ])
mean((lasso.lambda5 - y.test)^2)


cv.lasso.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 1, nfolds = 10)
plot(cv.lasso.out, main = 'Lasso Regression')
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)

lasso.bestlambdatrain = predict(lasso.models.train, s = bestlambda.lasso, newx = x[test, ])
sqrt(mean((lasso.bestlambdatrain - y.test)^2))
