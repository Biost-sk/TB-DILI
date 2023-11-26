#### XGBoost

library(caret)
library(xgboost)

param_grid <- expand.grid(
  nrounds = c(20, 40, 60),
  max_depth = c(6, 10, 14),
  eta = c(0.1, 0.01, 0.001),
  gamma = 0,
  colsample_bytree = c(0.8, 0.9, 1.0),
  min_child_weight = 1,
  subsample = c(0.8, 0.9, 1.0)
)



ctrl <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = FALSE
)

xgb_model <- train(
  x = train_matrix,
  y = as.factor(train_xgb$DILI_Status),
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = param_grid)

best_params <- xgb_model$bestTune
best_model <- xgb_model$finalModel


train_xgb<-train_data
test_xgb<-test_data
library(xgboost)
library(Matrix)
library(Ckmeans.1d.dp)
train_matrix <- sparse.model.matrix(DILI_Status ~ .-1, data=train_xgb)
test_matrix <- sparse.model.matrix(DILI_Status ~ .-1, data=test_xgb)
train_label <- as.numeric(train_xgb$DILI_Status>0)
test_label <- as.numeric(test_xgb$DILI_Status>0)
train_fin <- list(data=train_matrix,label=train_label)
test_fin <- list(data=test_matrix,label=test_label)
dtrain <- xgb.DMatrix(data=train_fin$data,label=train_fin$label)
dtest <- xgb.DMatrix(data=test_fin$data,label=test_fin$label)
xgb <- xgboost(data=dtrain,max_depth=6,eta=0.1,nthread=2,objective='binary:logistic',nrounds =20,gamma = 0,colsample_bytree=0.8,min_child_weight=1,subsample=0.8)
importance <- xgb.importance(train_matrix@Dimnames[[2]], model=xgb)
e<-as.data.frame(importance)
write.csv(e,"importance.csv")
head.im<-head(importance)

validation_pred_XGBoost <- predict(xgb, newdata = dtest, type = "prob")

# 模型评估
# 可以使用混淆矩阵、准确率、召回率、AUC等指标进行评估

# AUROC
auc_XGB <- roc(test_xgb$DILI_Status, validation_pred_XGBoost)
plot(auc_XGB, main = "Receiver Operating Characteristic (ROC) Curve",
     xlab = "False Positive Rate", ylab = "True Positive Rate")
# Calculate Youden's Index
youden_index_XGB <- auc_XGB$sensitivities + auc_XGB$specificities - 1
# Find the cutoff value that maximizes Youden's Index
best_cutoff_XGB <- auc_XGB$thresholds[which.max(youden_index_XGB)]

# AUPR
library(modEvA)
aupr_XGB <- AUC(obs = test_data$DILI_Status,pred = validation_pred_XGBoost,curve = 'PR')
aupr_XGB$thresholds$precision
aupr_XGB$thresholds$sensitivity
aupr_XGB$AUC

#最佳截断值
predictions_XGB <- ifelse(validation_pred_XGBoost >= best_cutoff_XGB, 1, 0)

# 混淆矩阵
confusion_XGB <- confusionMatrix(data = factor(predictions_XGB, levels = c(0, 1)),
                                 reference = factor(test_xgb$DILI_Status, levels = c(0, 1)))
confusion_XGB
# 准确率
accuracy_XGB <- sum(test_data$DILI_Status == predictions_XGB) / length(test_data$DILI_Status)

precision_XGB <- sum(test_data$DILI_Status == 1 & predictions_XGB == 1) / sum(predictions_XGB == 1)
# 召回率
recall_XGB <- sum(test_data$DILI_Status == 1 & predictions_XGB == 1) / sum(test_data$DILI_Status == 1)
# FI score
f1_score_XGB <- 2 * (precision_XGB * recall_XGB) / (precision_XGB + recall_XGB)



# calibration
n_bins <- 10
bin_width <- 1 / n_bins
bins_XGB <- cut(validation_pred_XGBoost, breaks = seq(0, 1, by = bin_width), include.lowest = TRUE)

observed_proportions_XGB <- tapply(test_data$DILI_Status, bins_XGB, mean)

predicted_proportions_XGB <- tapply(validation_pred_XGBoost, bins_XGB, mean)

plot(predicted_proportions_XGB, observed_proportions_XGB, xlab = "Mean Predicted Probability", ylab = "Observed Proportion", pch = 19, col = "blue")
abline(a = 0, b = 1, col = "red")

brier_score_XGB <- mean((test_data$DILI_Status - validation_pred_XGBoost)^2)


# DCA
library(rmda)
data_XGB <- data.frame(validation_pred_XGBoost, test_data$DILI_Status)
simple_XGB <- decision_curve(test_data.DILI_Status~validation_pred_XGBoost,data = data_XGB,
                        family = binomial(link ='logit'),
                        thresholds= seq(0,1, by = 0.01),
                        confidence.intervals = 0.95)
plot_decision_curve(list(simple, simple_XGB,simple_LR),
                    confidence.intervals = FALSE,  #remove confidence intervals
                    cost.benefit.axis = FALSE, #remove cost benefit axis
                    curve.names = c("Random Forest","XGBoost","Logistic"),
                    col = c("#000000","#000000","#000000"),
                    lty = c(1,2,3,7,7),
                    lwd = c(2),
                    xlab = c("Risk Threshold"),
                    ylab = c("Net Benefit"),
                    ylim = c(0.00, 0.15),
                    standardize = FALSE, #plot Net benefit instead of standardized net benefit
                    legend.position = "none")
                    #main = "DCA")
legend("topright", legend = c("Random Forest","XGBoost","Logistic"),  lty = c(1,2,3),xpd = T,inset = c(0.02, 0.05, 0.02, 0.05),bg = "white")
