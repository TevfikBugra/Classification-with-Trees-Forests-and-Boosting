library(caret)
library(caTools)
library(randomForest)
library(gbm)
library(Metrics)

bank <- read.csv2("bank.csv", quote="\"") #the temperature column should be renamed

bank$marital = as.factor(bank$marital)
bank$education = as.factor(bank$education)
bank$default = as.factor(bank$default)
bank$housing = as.factor(bank$housing)
bank$loan = as.factor(bank$loan)
bank$contact = as.factor(bank$contact)
bank$poutcome = as.factor(bank$poutcome)
bank$y = as.factor(bank$y)

#Question 1

#a
set.seed(425)
split=sample.split(bank$y,SplitRatio=0.8)
banktr=subset(bank,split==TRUE)
bankte=subset(bank,split==FALSE)


#b
set.seed(425)
models <- list() # An empty list for different models 
ntrees = c(5,10,25,50,100,250,500) # A vector for ntree values
for (i in 1:length(ntrees)){
  ntree <- ntrees[i]
  ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
  rf_model <- train(y ~ ., data = banktr, metric = "Accuracy", method = "rf", trControl = ctrl, ntree=ntree,tuneGrid = expand.grid(.mtry = (3:6)), importance = TRUE)
  models[[i]] = rf_model
}


#c
models[[6]]$finalModel
importance(models[[6]]$finalModel)

#d
predictions = predict(models[[6]], newdata = bankte)
confusionMatrix(predictions, bankte$y)

#e
set.seed(425)
ctrl1 <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
gbmGrid=expand.grid(interaction.depth = c(1, 3, 5), 
                    n.trees = (1:10)*10, 
                    shrinkage = (1:9)*0.1,
                    n.minobsinnode = c(10, 20, 30))
gbm_bank = train(y~., data=banktr, 
               method="gbm", metric="Accuracy",verbose = FALSE,
               trControl = ctrl1,tuneGrid = gbmGrid)


max_index = which.max(gbm_bank$results$Accuracy)
max_index # get the index of the case where the accuracy is the largest 
gbm_bank$results$Accuracy[max_index]


#f
best_gbm_bank = gbm_bank
predict_gbm_bank = predict(best_gbm_bank, newdata = bankte)
confusionMatrix(predict_gbm_bank, bankte$y)


#Question 2

seoul <- read.csv("SeoulBikeData.csv") #The degree symbol is deleted from the data. Because r cannot read it otherwise.

seoul$Seasons = as.factor(seoul$Seasons)
seoul$Holiday = as.factor(seoul$Holiday)
seoul$Functioning.Day = as.factor(seoul$Functioning.Day)


#a
set.seed(425)
split=sample.split(seoul$Rented.Bike.Count,SplitRatio=0.8)
seoultr=subset(seoul,split==TRUE)
seoulte=subset(seoul,split==FALSE)

#b
set.seed(425)
models <- list() # An empty list for different models to be put into
ntrees = c(5,10,25,50,100,250,500) # A list for ntree values
for (i in 1:length(ntrees)){
  set.seed(425)
  ntree <- ntrees[i]
  ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
  rf_model <- train(Rented.Bike.Count ~ ., data = seoultr, metric = "RMSE", method = "rf", trControl = ctrl, ntree=ntree,tuneGrid = expand.grid(.mtry = (3:6)), importance = TRUE)
  models[[i]] = rf_model
}
rf_seoul = models[[7]]
rf_seoul$finalModel


#c
imp_rf_seoul = models[[7]]$finalModel
importance(imp_rf_seoul)


#d
predictions_rf = predict(models[[7]], newdata = seoulte)

rmse_seoul = rmse(actual = seoulte$Rented.Bike.Count,predicted = predictions_rf) # RMSE
mae_seoul = mae(actual = seoulte$Rented.Bike.Count, predicted = predictions_rf) #MAE
paste("RMSE:", rmse_seoul)
paste("MAE:", mae_seoul)

#e
set.seed(425)
ctrl1 <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
gbmGrid=expand.grid(interaction.depth = c(1, 3, 5), 
                    n.trees = (1:10)*10, 
                    shrinkage = (1:9)*0.1,
                    n.minobsinnode = c(10, 20, 30))
gbm_seoul = train(Rented.Bike.Count~., data=seoultr, 
                 method="gbm", metric="RMSE",verbose = FALSE,
                 trControl = ctrl1,tuneGrid = gbmGrid)

gbm_seoul


#f
best_gbm_seoul = gbm_seoul
predict_gbm_seoul = predict(best_gbm_seoul, newdata = seoulte)

rmse_seoul = rmse(actual = seoulte$Rented.Bike.Count,predicted = predict_gbm_seoul) # RMSE
mae_seoul = mae(actual = seoulte$Rented.Bike.Count, predicted = predict_gbm_seoul) #MAE
paste("RMSE:", rmse_seoul)
paste("MAE", mae_seoul)
