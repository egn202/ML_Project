housing = read.csv("housing_LM.csv")
housing=as.data.frame(housing)
housing=housing[-c(1407,2367,154,2423,998),] #severe outlier

summary(housing)
sapply(housing, sd) #check std dev for df
cor(housing)
plot(housing)

model.saturated = lm(target ~ ., data = housing)

summary(model.saturated) #Many predictor variables are not significant, yet the
#overall regression is significant.

plot(model.saturated) #Assessing the assumptions of the model.

library(car)
influencePlot(model.saturated)
vif(model.saturated) 
avPlots(model.saturated)

#removing GarageArea due to high VIF and high P-vals
model2 = lm(target ~ . - GarageCars, data = housing)

anova(model2, model.saturated)


model.empty = lm(target ~ 1, data = housing) #The model with an intercept ONLY. (~1 *arg)
model.full = lm(target ~ ., data = housing)
scope = list(lower = formula(model.empty), upper = formula(model.full))

library(MASS) #The Modern Applied Statistics library.
forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
backwardAIC = step(model.full, scope, direction = "backward", k = 2)
bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
bothAIC.full = step(model.full, scope, direction = "both", k = 2)


summary(forwardAIC)
plot(forwardAIC)
influencePlot(forwardAIC)
vif(forwardAIC)
avPlots(forwardAIC)
confint(forwardAIC)

library(ggplot2)
forwardAIC$fitted.values
ggplot()+geom_point(aes(x=forwardAIC$fitted.values, y=housing$target,col="#CC33FF", alpha=.5))+
  geom_smooth(aes(x=forwardAIC$fitted.values, y=housing$target,col="#666666", se=FALSE))+
  xlab("fitted values")+ ylab("actual values")+ theme_bw()+theme(legend.position = "None")