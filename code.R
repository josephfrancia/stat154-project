# data
load("TrainTest.RData")
data = as.data.frame(cbind(as.matrix(X), y))

index = sample(1:50000, 1000)
train = index[1:500]
test = index[501:1000]

# random forest
library(randomForest)

rf_model = randomForest(x = data[train,1:1000], 
                        y = as.factor(data$y[train]),
                        ntree = 20, 
                        do.trace = FALSE)

yhat_rf = predict(rf_model, data[test, ])

sum(yhat_rf == data$y[test])/length(data$y[test])

# kaggle data scoring
yhat_rf_xtest = predict(rf_model, Xtest)

### 3. Gradient Boosting 
library(gbm)

gbm_model = gbm.fit(x = data[train,1:1000], 
                    y = as.factor(data$y[train]), 
                    n.trees = 20, 
                    distribution = "multinomial", 
                    verbose = TRUE, shrinkage = 0.6)

yhat_gbm = predict(gbm_model, data[test,], 
                   n.trees = 10, type = "response")[,,1]

