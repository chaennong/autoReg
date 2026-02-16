# Accuracy of binary response variable
# General logistic model
model_logistic <- simpleEnsembleGroup19::fitLogisticModel(bin.x,bin.y)
test_X <- train_test(bin.x,bin.y,0.8)$test_X
test_y <- train_test(bin.x,bin.y,0.8)$test_y
probabilities <- cbind(1,test_X) %*% model_logistic$coefficients
predicted_class <- ifelse(probabilities>0.5,1,0)
confusionMatrix(factor(predicted_class),factor(test_y),positive="1")

# Bagging
baggedLogistic <- simpleEnsembleGroup19::bagging_logistic(bin.x,bin.y,100)
pred_bagging <- baggedLogistic$final.prediction
test_y <- baggedLogistic$test_y
confusionMatrix(factor(pred_bagging),factor(test_y))

# Ensemble
ensemble_binary <- simpleEnsembleGroup19::ensemblelearning(bin.x,bin.y)
pred_ensemble <- ensemble_binary$final_predictions
test_y <- ensemble_binary$test_y
confusionMatrix(factor(pred_ensemble),factor(test_y),positive="1")

# Accuracy of continuous response variable
fitLinear <- simpleEnsembleGroup19::fitLinearModel(cont.x,cont.y)
test_X <- train_test(cont.x,cont.y,0.8)$test_X
test_y <- train_test(cont.x,cont.y,0.8)$test_y
pred_linear <- cbind(1,test_X) %*% coef(fitLinear)
RMSE1 <- RMSE(pred_linear,test_y)

# Bagging
baggingLinear <- simpleEnsembleGroup19::bagging_linear(cont.x,cont.y,50)
pred_bagging <- baggingLinear$final.prediction
test_y <- baggingLinear$test_y
RMSE2 <- RMSE(pred_bagging,test_y)

# Ensemble
ensemble_continuous <- simpleEnsembleGroup19::ensemblelearning(cont.x,cont.y)
pred_ensemble <- ensemble_continuous$final_predictions
test_y <- ensemble_continuous$test_y
RMSE3 <- RMSE(pred_ensemble,test_y)

