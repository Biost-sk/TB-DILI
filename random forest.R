setwd("C:/Users/Shaw/Desktop/DILI restart/fresh local/原始数据")
library(randomForest)
library(caret)
library(pROC)

train_data <- read.csv("train_after_lasso.csv")
test_data <- read.csv("test_after_lasso.csv")

train_data<-train_data[,c(2,4:40)]
test_data<-test_data[,c(2,4:40)]

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


# 参数调优
# 使用caret包的train()函数进行参数调优
ctrl_rf <- trainControl(method = "cv", number = 10)  # 交叉验证设置
tune_grid_rf <- expand.grid(mtry = c(2, 4, 6))  # 调优参数网格


rf_model <- train(
  DILI_Status ~ .,
  data = train_data,
  method = "rf",
  trControl = ctrl_rf,
  tuneGrid = tune_grid_rf
)

# 使用最优参数重新训练模型
set.seed(0804)
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

# AUPR
library(modEvA)
aupr <- AUC(obs = test_data$DILI_Status,pred = validation_pred,curve = 'PR')
aupr$thresholds$precision
aupr$thresholds$sensitivity
aupr$AUC

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
library(rmda)
data <- data.frame(validation_pred, test_data$DILI_Status)
simple<- decision_curve(test_data.DILI_Status~validation_pred,data = data,
                        family = binomial(link ='logit'),
                        thresholds= seq(0,1, by = 0.01),
                        confidence.intervals = 0.95,
                        )
plot_decision_curve(simple, 
                    confidence.intervals = FALSE,  #remove confidence intervals
                    cost.benefit.axis = FALSE, #remove cost benefit axis
                    curve.names = c("Random forest"),
                    lty = c(3,1,1),
                    lwd = c(2),
                    xlab = c("Risk threshold"),
                    ylab = c("Net benefit"),
                    ylim = c(0.00, 0.15),
                    standardize = FALSE, #plot Net benefit instead of standardized net benefit
                    legend.position = "none",
                    main = "DCA")
legend("topright", legend = c("Random forest"), col = c("red"), lty = c(1),xpd = T,inset = c(0.02, 0.05, 0.02, 0.05),bg = "white")

plot_decision_curve( list(baseline.model, full.model),
                     curve.names = c("Baseline model", "Full model"),
                     col = c("blue", "red"),
                     ylim = c(-0.05, 0.15), #set ylim
                     confidence.intervals = FALSE,  #remove confidence intervals
                     cost.benefit.axis = FALSE, #remove cost benefit axis
                     lty = c(2,1),
                     lwd = c(3,2, 2, 1),
                     standardize = FALSE, #plot Net benefit instead of standardized net benefit
                     legend.position = "topright")

install.packages("DALEX")
library(DALEX)

rf_exp <- DALEX::explain(final_model,
                         data = train_data[,-1],
                         y = train_data$DILI_Status=="1",
                         label = "randomForest")
library(ingredients)
imp_rf <- feature_importance(rf_exp)
plot(imp_rf)
plot(rf_exp,type = "bars")


lg_exp <- DALEX::explain(lg,
                         data = titanic[,-9],
                         y=titanic$survived=="yes",
                         label = "Logistic")

describe(imp_rf)

pdp_rf <- model_profile(rf_exp,variables = "ALT_max_ratio_baseline",groups = "age_cat")
pdp_rf

plot(pdp_rf)+
  ggtitle("Partial-dependence profile for age")

pdp_lg <- model_profile(lg_exp, variables = "age")

plot(pdp_rf, pdp_lg) +
  ggtitle("")

install.packages("vivo")
library(vivo)
p <- model_profile(rf_exp)
plot(p)

measure<-global_variable_importance(p)
plot(measure)

install.packages("pdp")
library(pdp)
partial(final_model,
        pred.var = "Sex",
        ice = T,
        plot = T,
        alpha = 0.2)

library(auditor)
install.packages("auditor")
rf_eva <- auditor::model_evaluation(rf_exp)
plot(rf_eva)
plot(rf_eva,type = "lift")
rf_mp<-auditor::model_performance(rf_exp)
plot(rf_mp)
rf_mr<-auditor::model_residual(rf_exp)
plot_prediction(rf_mr,abline = T)
