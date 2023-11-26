#########################数据切分###########################
#将数据分成训练集和测试集
library(tidyverse)
library(dplyr)
library(splitstackshape)
library(caret)
setwd("C:/Users/Shaw/Desktop/DILI restart/fresh local/原始数据")
data<-read.csv("id_list_1230.csv")
training <- data %>% stratified(., group = "DILI_Status", size = 0.70)
training$index=1
bridgetrain<- training[,c(4,429)]
data<-left_join(data,bridgetrain,by="idcard")
testing<-data[is.na(data$index),]
table(training$DILI_Status)
table(data$DILI_Status)
train<- subset(training,select = -idcard)
train<- subset(train,select = -index)
test<- subset(testing,select = -idcard)
test<- subset(test,select = -index)
a<-as.data.frame(train)
write.csv(a,"train.csv",row.names = F)
b<-as.data.frame(test)
write.csv(b,"test.csv",row.names = F)


#########################LASSO##################################

#加载需要的包
library(glmnet)
#设置随机种子以确保结果可重复
set.seed(123)
#读取训练集和验证集
Train<-read.csv("train.csv")
Test<- read.csv("test.csv")
#转换成矩阵形式
X1 <- data.matrix (Train[,4:427])
X2 <- data.matrix (Test[,4:427])
#提取结果变量
Y1 <- Train [,2]
Y2 <- Test [,2]

#训练模型,通过交叉验证选择最佳的正则化参数‘lambda’
cv_model <- cv.glmnet(X1,Y1,alpha = 1)
best_lambda <- cv_model$lambda.min
best_lambda
easy_lambda <- cv_model$lamba.1se
#根据所选lambda值，获取LASSO模型的系数（特征重要性）
Lasso_model <- glmnet(X1,Y1,alpha = 1,lambda = best_lambda,family = "binomial")
coef(Lasso_model)
sort(abs(Lasso_coef))
#观察正则化效果
plot(Lasso_model, xvar = "lambda", label = TRUE)

#评估模型性能
Lasso_prediction <- predict (Lasso_model, newx = X2, s = best_lambda)
Y2<-as.numeric(Y2)
lasso_performance<-(Lasso_prediction-Y2)^2
Lasso_performance <-mean(lasso_performance)

#重构数据
train_after_lasso<-Train[,c("DILI_Grade","DILI_Status","DILI_Triple","Sex","Race","Profession","Dx_type","Dx_TB_type","Educ","age_cat","ALT_max_ratio_baseline",
                            "ALP_max_ratio_baseline","Tbil_max_ratio_baseline","ALT_abn_pre","ALP_abn_pre","Tbil_abn_pre","TCM_ini","hepato_ini_count","dia_bas","liver_bas","hta_bas","hbv_bas","alco_bas","bqxa_ini","lfp_ini","yyj_ini"
                            ,"D01V040Once","D01V042Once","D01V053Once","D01V066Freq","D01V070Once","D01V071Once","D01V082Once","D01V088Once","D01V090Freq",
                            "D01V090Once","D01V090Spor","D01V1307Once","D02V003Once","D03V109Freq")]
test_after_lasso<-Test[,c("DILI_Grade","DILI_Status","DILI_Triple","Sex","Race","Profession","Dx_type","Dx_TB_type","Educ","age_cat","ALT_max_ratio_baseline",
                          "ALP_max_ratio_baseline","Tbil_max_ratio_baseline","ALT_abn_pre","ALP_abn_pre","Tbil_abn_pre","TCM_ini","hepato_ini_count","dia_bas","liver_bas","hta_bas","hbv_bas","alco_bas","bqxa_ini","lfp_ini","yyj_ini"
                          ,"D01V040Once","D01V042Once","D01V053Once","D01V066Freq","D01V070Once","D01V071Once","D01V082Once","D01V088Once","D01V090Freq",
                          "D01V090Once","D01V090Spor","D01V1307Once","D02V003Once","D03V109Freq")]
a<-as.data.frame(train_after_lasso)
write.csv(a,"train_after_lasso.csv",row.names = F)
b<-as.data.frame(test_after_lasso)
write.csv(b,"test_after_lasso.csv",row.names = F)

#######################Random Forest#############################
library(randomForest)
library(caret)
library(pROC)

train_data <- read.csv("train_after_lasso.csv")
test_data <- read.csv("test_after_lasso.csv")

train_data<-train_data[,c(2,4:40)]
test_data<-test_data[,c(2,4:40)]

# 参数调优
# 使用caret包的train()函数进行参数调优
ctrl <- trainControl(method = "cv", number = 10)  # 交叉验证设置
tune_grid <- expand.grid(mtry = c(2, 4, 6))  # 调优参数网格


rf_model <- train(
  DILI_Status ~ .,
  data = train_data,
  method = "rf",
  trControl = ctrl,
  tuneGrid = tune_grid
)

# 使用最优参数重新训练模型
final_model <- randomForest(
  DILI_Status ~ .,
  data = train_data,
  mtry = rf_model$bestTune$mtry,
  ntree = 100
)

# 在验证集上进行预测
validation_pred <- predict(final_model, newdata = test_data)

# 模型评估
# 可以使用混淆矩阵、准确率、召回率、AUC等指标进行评估

# AUROC
auc <- roc(test_data$DILI_Status, validation_pred)
plot(auc, main = "Receiver Operating Characteristic (ROC) Curve",
     xlab = "False Positive Rate", ylab = "True Positive Rate")
# Calculate Youden's Index
youden_index <- auc$sensitivities + auc$specificities - 1
# Find the cutoff value that maximizes Youden's Index
best_cutoff <- auc$thresholds[which.max(youden_index)]

#最佳截断值
predictions <- ifelse(validation_pred >= best_cutoff, 1, 0)

# 混淆矩阵
table(test_data$DILI_Status)
confusion <- confusionMatrix(data = factor(predictions, levels = c(0, 1)),
                             reference = factor(test_data$DILI_Status, levels = c(0, 1)))
confusion
# 准确率
accuracy <- sum(test_data$DILI_Status == predictions) / length(test_data$DILI_Status)

precision <- sum(test_data$DILI_Status == 1 & predictions == 1) / sum(predictions == 1)
# 召回率
recall <- sum(test_data$DILI_Status == 1 & predictions == 1) / sum(test_data$DILI_Status == 1)
# FI score
f1_score <- 2 * (precision * recall) / (precision + recall)



# calibration
n_bins <- 10
bin_width <- 1 / n_bins
bins <- cut(validation_pred, breaks = seq(0, 1, by = bin_width), include.lowest = TRUE)

observed_proportions <- tapply(test_data$DILI_Status, bins, mean)

predicted_proportions <- tapply(validation_pred, bins, mean)

plot(predicted_proportions, observed_proportions, xlab = "Mean Predicted Probability", ylab = "Observed Proportion", pch = 19, col = "blue")
abline(a = 0, b = 1, col = "red")

brier_score <- mean((test_data$DILI_Status - validation_pred)^2)



# DCA
install.packages("rmda")
library(rmda)
data <- data.frame(validation_pred, test_data$DILI_Status)
simple<- decision_curve(test_data.DILI_Status~validation_pred,data = data,
                        family = binomial(link ='logit'),
                        thresholds= seq(0,1, by = 0.01),
                        confidence.intervals = 0.95)
plot_decision_curve(simple)


#### Logistic
library(pscl)
library(car)
library(pROC)
set.seed(0711)
colnames(train_data)

train_data$Sex<-as.factor(train_data$Sex)
train_data$Race<-as.factor(train_data$Race)
train_data$Profession<-as.factor(train_data$Profession)
train_data$Dx_type<-as.factor(train_data$Dx_type)
train_data$Dx_TB_type<-as.factor(train_data$Dx_TB_type)
train_data$Educ<-as.factor(train_data$Educ)
train_data$age_cat<-as.factor(train_data$age_cat)
train_data$ALP_abn_pre<-as.factor(train_data$ALP_abn_pre)
train_data$Tbil_abn_pre<-as.factor(train_data$Tbil_abn_pre)
train_data$ALT_abn_pre<-as.factor(train_data$ALT_abn_pre)
train_data$TCM_ini<-as.factor(train_data$TCM_ini)
train_data$dia_bas<-as.factor(train_data$dia_bas)
train_data$liver_bas<-as.factor(train_data$liver_bas)
train_data$hta_bas<-as.factor(train_data$hta_bas)
train_data$hbv_bas<-as.factor(train_data$hbv_bas)
train_data$alco_bas<-as.factor(train_data$alco_bas)
train_data$bqxa_ini<-as.factor(train_data$bqxa_ini)
train_data$lfp_ini<-as.factor(train_data$lfp_ini)
train_data$yyj_ini<-as.factor(train_data$yyj_ini)

test_data$Sex<-as.factor(test_data$Sex)
test_data$Race<-as.factor(test_data$Race)
test_data$Profession<-as.factor(test_data$Profession)
test_data$Dx_type<-as.factor(test_data$Dx_type)
test_data$Dx_TB_type<-as.factor(test_data$Dx_TB_type)
test_data$Educ<-as.factor(test_data$Educ)
test_data$age_cat<-as.factor(test_data$age_cat)
test_data$ALP_abn_pre<-as.factor(test_data$ALP_abn_pre)
test_data$Tbil_abn_pre<-as.factor(test_data$Tbil_abn_pre)
test_data$ALT_abn_pre<-as.factor(test_data$ALT_abn_pre)
test_data$TCM_ini<-as.factor(test_data$TCM_ini)
test_data$dia_bas<-as.factor(test_data$dia_bas)
test_data$liver_bas<-as.factor(test_data$liver_bas)
test_data$hta_bas<-as.factor(test_data$hta_bas)
test_data$hbv_bas<-as.factor(test_data$hbv_bas)
test_data$alco_bas<-as.factor(test_data$alco_bas)
test_data$bqxa_ini<-as.factor(test_data$bqxa_ini)
test_data$lfp_ini<-as.factor(test_data$lfp_ini)
test_data$yyj_ini<-as.factor(test_data$yyj_ini)

LR_fml<-as.formula(paste('DILI_Status~',paste(colnames(train_data)[c(2:38)],collapse='+')))
model <- train(X, Y,
               method = "glm",
               trControl = trainControl(method = "cv", number = 10),
               metric = "accuracy")
model_LR=glm(LR_fml, family = "binomial"("logit"),data = train_data)
summary(model_LR)
exp(coef(model_LR))
options(scipen = 999)
pscl::pR2(model_LR)["McFadden"]
caret::varImp(model_LR)
car::vif(model_LR)
validation_pred_LR <- predict(model_LR, newdata = test_data,type = "response")

# 模型评估
# 可以使用混淆矩阵、准确率、召回率、AUC等指标进行评估

# AUROC
auc_LR <- roc(test_data$DILI_Status, validation_pred_LR)
plot(auc_LR, main = "Receiver Operating Characteristic (ROC) Curve",
     xlab = "False Positive Rate", ylab = "True Positive Rate")
# Calculate Youden's Index
youden_index_LR <- auc_LR$sensitivities + auc_LR$specificities - 1
# Find the cutoff value that maximizes Youden's Index
best_cutoff_LR <- auc_LR$thresholds[which.max(youden_index_LR)]

#最佳截断值
predictions_LR <- ifelse(validation_pred_LR >= best_cutoff_LR, 1, 0)

# 混淆矩阵
confusion_LR <- confusionMatrix(data = factor(predictions_LR, levels = c(0, 1)),
                             reference = factor(test_data$DILI_Status, levels = c(0, 1)))
confusion_LR
# 准确率
accuracy_LR <- sum(test_data$DILI_Status == predictions_LR) / length(test_data$DILI_Status)

precision_LR <- sum(test_data$DILI_Status == 1 & predictions_LR == 1) / sum(predictions_LR == 1)
# 召回率
recall_LR <- sum(test_data$DILI_Status == 1 & predictions_LR == 1) / sum(test_data$DILI_Status == 1)
# FI score
f1_score_LR <- 2 * (precision_LR * recall_LR) / (precision_LR + recall_LR)



# calibration
n_bins <- 10
bin_width <- 1 / n_bins
bins_LR <- cut(validation_pred_LR, breaks = seq(0, 1, by = bin_width), include.lowest = TRUE)

observed_proportions_LR <- tapply(test_data$DILI_Status, bins_LR, mean)

predicted_proportions_LR <- tapply(validation_pred_LR, bins_LR, mean)

plot(predicted_proportions_LR, observed_proportions_LR, xlab = "Mean Predicted Probability", ylab = "Observed Proportion", pch = 19, col = "blue")
abline(a = 0, b = 1, col = "red")

brier_score_LR <- mean((test_data$DILI_Status - validation_pred_LR)^2)


#EO
library(dplyr)

df <- data.frame(predictions, test_data$DILI_Status, sensitive_attributes)
grouped_data <- group_by(df, sensitive_attributes)

# Calculate true positive rate (TPR) for each group
tp_rate <- summarise(grouped_data, tpr = mean(predicted_outcomes[y == 1]))

# Calculate false positive rate (FPR) for each group
fp_rate <- summarise(grouped_data, fpr = mean(predicted_outcomes[y == 0]))

max_tpr <- max(tp_rate$tpr)
min_fpr <- min(fp_rate$fpr)

eo_ratio <- max_tpr / min_fpr



# DCA
install.packages("rmda")
library(rmda)
data <- data.frame(validation_pred_LR, test_data$DILI_Status)
simple<- decision_curve(test_data.DILI_Status~validation_pred_LR,data = data,
                        family = binomial(link ='logit'),
                        thresholds= seq(0,1, by = 0.01),
                        confidence.intervals = 0.95)
plot_decision_curve(simple)




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
xgb <- xgboost(data=dtrain,max_depth=6,eta=0.01,nthread=2,objective='binary:logistic',nrounds =40,gamma = 0,colsample_bytree=0.9,min_child_weight=1,subsample=0.8)
importance <- xgb.importance(train_matrix@Dimnames[[2]], model=xgb)
e<-as.data.frame(importance)
write.csv(e,"importance.csv")
head.im<-head(importance)

validation_pred_XGBoost <- predict(xgb, newdata = dtest)

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


#EO
library(dplyr)

df <- data.frame(predictions, test_data$DILI_Status, sensitive_attributes)
grouped_data <- group_by(df, sensitive_attributes)

# Calculate true positive rate (TPR) for each group
tp_rate <- summarise(grouped_data, tpr = mean(predicted_outcomes[y == 1]))

# Calculate false positive rate (FPR) for each group
fp_rate <- summarise(grouped_data, fpr = mean(predicted_outcomes[y == 0]))

max_tpr <- max(tp_rate$tpr)
min_fpr <- min(fp_rate$fpr)

eo_ratio <- max_tpr / min_fpr



# DCA
install.packages("rmda")
library(rmda)
data <- data.frame(validation_pred_XGBoost, test_data$DILI_Status)
simple<- decision_curve(test_data.DILI_Status~validation_pred_XGBoost,data = data,
                        family = binomial(link ='logit'),
                        thresholds= seq(0,1, by = 0.01),
                        confidence.intervals = 0.95)
plot_decision_curve(simple)

data1 <- data.frame(predicted = validation_pred, outcome = test_data$DILI_Status)
data2 <- data.frame(predicted = validation_pred_LR, outcome = test_data$DILI_Status)
data3 <- data.frame(predicted = validation_pred_XGBoost, outcome = test_data$DILI_Status)
library(dplyr)

data1 <- data1 %>% mutate(Curve = "Random forest")
data2 <- data2 %>% mutate(Curve = "Logistic")
data3 <- data3 %>% mutate(Curve = "XGBoost")
combined_data <- rbind(data1, data2, data3)


library(ggplot2)

ggplot(data = combined_data) +
  geom_line(aes(x = predicted, y = outcome, color = "Curve")) +
  xlab("Predicted Probabilities") +
  ylab("Actual Outcomes") +
  ggtitle("Decision Curves") +
  scale_color_manual(values = c("Random forest" = "#0072B2", "Logistic" = "#009E73", "XGBoost" = "#D55E00")) +
  custom_theme

auc
auc_LR
auc_XGB
library(pROC)

data1 <- data.frame(FPR = coords(auc, best.method = "fpr"), TPR = coords(auc, best.method = "tpr"))
data2 <- data.frame(FPR = coords(auc_LR, best.method = "fpr"), TPR = coords(auc_LR, best.method = "tpr"))
data3 <- data.frame(FPR = coords(auc_XGB, best.method = "fpr"), TPR = coords(auc_XGB, best.method = "tpr"))
library(dplyr)

data1 <- data1 %>% mutate(Curve = "Random forest")
data2 <- data2 %>% mutate(Curve = "Logistic")
data3 <- data3 %>% mutate(Curve = "XGBoost")
combined_data <- rbind(data1, data2, data3)

custom_theme <- theme(
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(size = 14, face = "bold"),
  axis.title = element_text(size = 12),
  axis.text = element_text(size = 10),
  legend.title = element_blank(),
  legend.text = element_text(size = 10)
)

ggplot(data = combined_data) +
  geom_line(aes(x = 1 - FPR.specificity, y = TPR.sensitivity,color = Curve)) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  ggtitle("AUROC Curve") +
  scale_color_manual(values = c("Random forest" = "#0072B2", "Logistic" = "#009E73", "XGBoost" = "#D55E00")) +
  custom_theme

confusion
confusion_LR
confusion_XGB

library(SHAPforxgboost)
shapdata<-read.csv("clinicforshap.csv")
y_var<- shapdata$DILI_clinic
datax<-as.matrix(shapdata[,-train$DILI_clinic])
param_list<-list(objective = "binary:logistic",
                 eat=0.1,
                 max_depth=10,
                 gamma=1,
                 subsample=1)
mod<-xgboost::xgboost(data = datax,
                      label=as.matrix(y_var),
                      params = param_list, nrounds = 40,
                      verbose = FALSE,nthread= parallel::detectCores() - 2,
                      early_stopping_rounds = 8)
shap_values <- shap.values(xgb_model = mod, X_train = datax)
shap_values$mean_shap_score
shap_long<-shap.prep(xgb_model = mod, X_train = datax)
shap.plot.summary(shap_long)

fig_list<-lapply(names(shap_values$mean_shap_score)[1:10],
                 shap.plot.dependence,data_long=shap_long)
gridExtra::grid.arrange(grobs = fig_list, ncol =2)

shap_int<-shap.prep.interaction(xgb_mod=mod, X_train = datax)
fig_interaction_list<-lapply(names(shap_values$mean_shap_score)[1:4],
                             shap.plot.dependence,data_long=shap_long, data_int=shap_int)
gridExtra::grid.arrange(grobs = fig_list, ncol =2)

plot_data<-shap.prep.stack.data(shap_contrib = shap_values$shap_score, top_n = 4, n_groups = 6)
shap.plot.force_plot_bygroup(plot_data)

install.packages("SHAPforxgboost")
library(SHAPforxgboost)
shapdata<-read.csv("train_after_lasso_title.csv")
shapdata<-shapdata[,c(2,4:40)]
y_var<- shapdata$DILI_Status
datax<-as.matrix(shapdata[,-1])
param_list_shap<-list(objective = "binary:logistic",
                 eat=0.1,
                 max_depth=6,
                 gamma=0,
                 subsample=0.8)
mod<-xgboost::xgboost(data = datax,
                      label=as.matrix(y_var),
                      params = param_list_shap, nrounds = 20,
                      verbose = FALSE,nthread= parallel::detectCores() - 2,
                      early_stopping_rounds = 8)
shap_values <- shap.values(xgb_model = mod, X_train = datax)
shap_values$mean_shap_score
shap_long<-shap.prep(xgb_model = mod, X_train = datax)
shap.plot.summary(shap_long)

fig_list<-lapply(names(shap_values$mean_shap_score)[1:10],
                 shap.plot.dependence,data_long=shap_long)
gridExtra::grid.arrange(grobs = fig_list, ncol =2)

shap_int<-shap.prep.interaction(xgb_mod=mod, X_train = datax)
fig_interaction_list<-lapply(names(shap_values$mean_shap_score)[1:4],
                             shap.plot.dependence,data_long=shap_long, data_int=shap_int)
gridExtra::grid.arrange(grobs = fig_list, ncol =2)

plot_data<-shap.prep.stack.data(shap_contrib = shap_values$shap_score, top_n = 4, n_groups = 6)
shap.plot.force_plot_bygroup(plot_data)

