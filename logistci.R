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
model_LR <- train(train_data[,-1], factor(train_data$DILI_Status),
               method = "glm",
               trControl = trainControl(method = "cv", number = 10),
               metric = "Accuracy")
summary(model_LR)


validation_pred_LR <- predict(model_LR, newdata = test_data,type = "prob")
validation_pred_LR <- validation_pred_LR[,2]

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


# AUPR
library(modEvA)
aupr_LR <- AUC(obs = test_data$DILI_Status,pred = validation_pred_LR,curve = 'PR')
aupr_LR$thresholds$precision
aupr_LR$thresholds$sensitivity
aupr_LR$AUC

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


# DCA
library(rmda)
data_LR <- data.frame(validation_pred_LR, test_data$DILI_Status)
simple_LR <- decision_curve(test_data.DILI_Status~validation_pred_LR,data = data_LR,
                        family = binomial(link ='logit'),
                        thresholds= seq(0,1, by = 0.01),
                        confidence.intervals = 0.95)
plot_decision_curve(list(simple, simple_LR),
                    confidence.intervals = FALSE,  #remove confidence intervals
                    cost.benefit.axis = FALSE, #remove cost benefit axis
                    curve.names = c("Random forest","Logistic"),
                    col = c("red","blue"),
                    lty = c(3,3,1,1),
                    lwd = c(2),
                    xlab = c("Risk threshold"),
                    ylab = c("Net benefit"),
                    ylim = c(0.00, 0.15),
                    standardize = FALSE, #plot Net benefit instead of standardized net benefit
                    legend.position = "none",
                    main = "DCA")
legend("topright", legend = c("Random forest","Logistic"), col = c("red","blue"), lty = c(1,1),xpd = T,inset = c(0.02, 0.05, 0.02, 0.05),bg = "white")
