rm(list = ls())
library(tidyverse)
library(dplyr)
library(glmnet)
library(caret)
library(xgboost)
library(randomForest)
library(pROC)
library(modEvA)
library(rms)
library(rmda)
library(shapviz)

### Data preparation 1
data <- read.csv("id_list_1230.csv")
data <- data %>% 
  select(-DILI_Grade, -DILI_Triple, -idcard)
training <- data %>% stratified(., group = "DILI_Status", size = 0.70)
training$index = 1
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

train <- read.csv("train.csv") %>% 
  select(-DILI_Grade, -DILI_Triple) %>% 
  rename(IDILIO= D02V033Once,
         IDILIS= D02V033Spor,
         ODIHO= D01V040Once,
         ODIHF= D01V040Freq,
         ODILIO = D01V070Once,
         ODILIF = D01V070Freq,
         IDIHF= D02V003Freq,
         IDIHO= D02V003Once,
         ODF = D01V005Freq,
         ODIHS= D01V040Spor,
         ODIHF= D01V040Freq,
         ODILIS= D01V070Spor,
         OH= D01V010Spor,
         ODT = D01V022Freq,
         OPB= D01V017Once,
         OLGI= D01V019Spor,
         ODO = D01V005Once,
         ODIAO = D01V008Once,
         ODIAF = D01V008Freq,
         OAB = D01V009Once,
         OHUA = D01V378Spor,
         EDUCATION = Educ,
         ALT = ALT_max_ratio_baseline,
         ALP = ALP_max_ratio_baseline,
         TBIL = Tbil_max_ratio_baseline,
         AGE = Age_cal,
         FLD= liver_bas,
         AHI = D03V153Once,
         RIF = D03V967Once)
test <- read.csv("test.csv") %>% 
  select(-DILI_Grade, -DILI_Triple) %>% 
  mutate(DILI_Status = as.factor(DILI_Status)) %>% 
  rename(IDILIO= D02V033Once,
         IDILIS= D02V033Spor,
         ODIHO= D01V040Once,
         ODIHF= D01V040Freq,
         ODILIO = D01V070Once,
         ODILIF = D01V070Freq,
         IDIHF= D02V003Freq,
         IDIHO= D02V003Once,
         ODF = D01V005Freq,
         ODIAO = D01V008Once,
         ODIAF = D01V008Freq,
         ODIHS= D01V040Spor,
         ODIHF= D01V040Freq,
         ODILIS= D01V070Spor,
         OH= D01V010Spor,
         ODT = D01V022Freq,
         OPB= D01V017Once,
         OLGI= D01V019Spor,
         ODO = D01V005Once,
         OAB = D01V009Once,
         OHUA = D01V378Spor,
         EDUCATION = Educ,
         ALT = ALT_max_ratio_baseline,
         ALP = ALP_max_ratio_baseline,
         TBIL = Tbil_max_ratio_baseline,
         AGE = Age_cal,
         FLD= liver_bas,
         AHI = D03V153Once,
         RIF = D03V967Once)
# write.csv(b,"test.csv",row.names = F)

X1 <- data.matrix (Train[,4:427])
X2 <- data.matrix (Test[,4:427])
Y1 <- Train [,2]
Y2 <- Test [,2]

ctrl <- trainControl(method = "cv", number = 10, classProbs = T, summaryFunction = twoClassSummary)
levels(train$DILI_Status) <- make.names(levels(train$DILI_Status))

### Model training
## Lasso-LR
cv_model <- cv.glmnet(X1,Y1,alpha = 1)
best_lambda <- cv_model$lambda.min
best_lambda
easy_lambda <- cv_model$lamba.1se
Lasso_model <- glmnet(X1,Y1,alpha = 1,lambda = best_lambda,family = "binomial")
coef(Lasso_model)

cv_model <- cv.glmnet(X1,Y1,alpha = 1)
best_lambda <- cv_model$lambda.min
best_lambda

Lasso_model <- glmnet(X1,Y1,alpha = 1,lambda = best_lambda,family = "binomial")
coef(Lasso_model)
sort(abs(Lasso_coef))
plot(Lasso_model, xvar = "lambda", label = TRUE)

train_LR <- train(DILI_Status ~ ., data = train, method = "glm", trControl = ctrl, metric = "ROC")
train_LR

## RF
tune_grid_RF <- expand.grid(mtry = seq(2, 10, 2),
                            splitrule = c("gini","extratrees","hellinger"),
                            min.node.size = seq(2, 10, 2))
train_RF <- train(DILI_Status ~ ., data = train, method = "ranger", trControl = ctrl, tuneGrid = tune_grid_RF, metric = "ROC")
train_RF

RF <- randomForest(DILI_Status ~ .,
                   data = train,
                   mtry = 10,
                   nodesize = 6)

## XGB
tune_grid_XGBoost <- expand.grid(
  nrounds = c(20, 40, 60),
  max_depth = c(2, 6, 10),
  eta = c(0.1, 0.01, 0.001),
  gamma = 0,
  colsample_bytree = c(0.6, 0.8, 1.0),
  min_child_weight = 1,
  subsample = c(0.8, 0.9, 1))

tune_grid_XGBoost <- expand.grid(
  nrounds = c(60),
  max_depth = c(6),
  eta = c(0.1),
  gamma = 0,
  colsample_bytree = c(0.6),
  min_child_weight = 1,
  subsample = c(0.9))
train_XGB <- train(DILI_Status ~ ., data = train, method = "xgbTree", trControl = ctrl, tuneGrid = tune_grid_XGBoost, metric = "ROC")
train_XGB

trainXGB <- xgb.DMatrix(data.matrix(train[,-1]), label = train[,1])
set.seed(1)
XGB <- xgb.train(params = list(max_depth = 6,
                               eta = 0.1,
                               gamma = 0,
                               colsample_bytree = 0.6,
                               min_child_weight = 1,
                               subsample = 0.9),
                 data = trainXGB, nrounds = 40)

imp <- xgb.importance(model = XGB)
imf <- read.csv("IMF.csv")
ggplot(imf, aes(x = reorder(Features, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  coord_flip() +
  labs(y ="Feature importance" ,x = NULL) +
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 14, face = "bold"), 
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank())


### Model comparision
## Model_LR
train_pred_LR <- predict(train_LR, newdata = Train, type = "prob")
train_pred_LR <- train_pred_LR[,2]
validation_pred_LR <- predict(train_LR, newdata = Test, type = "prob")
validation_pred_LR <- validation_pred_LR[,2]
auc_LR <- roc(Test$DILI_Status, validation_pred_LR)
auc_LR; ci(auc_LR, method = "bootstrap")

best_cutoff_LR <- coords(roc(Train$DILI_Status, train_pred_LR), "best", best.method="youden"); best_cutoff_LR
predictions_LR <- ifelse(validation_pred_LR >= best_cutoff_LR$threshold, 1, 0)

confusion_LR <- caret::confusionMatrix(data = factor(predictions_LR, levels = c(0, 1)),
                                       reference = factor(Test$DILI_Status, levels = c(0, 1)),
                                       positive = "1")
confusion_LR

## Model_RF
train_pred_RF <- predict(train_RF, newdata = train, type = "prob")
train_pred_RF <- train_pred_RF[,2]
validation_pred_RF <- predict(train_RF, newdata = test, type = "prob")
validation_pred_RF <- validation_pred_RF[,2]

auc_RF <- roc(test$DILI_Status, validation_pred_RF)
auc_RF; ci(auc_RF, method = "bootstrap")

best_cutoff_RF <- coords(roc(train$DILI_Status, train_pred_RF), "best", best.method="youden"); best_cutoff_RF
predictions_RF <- ifelse(validation_pred_RF >= best_cutoff_RF$threshold, 1, 0)

confusion_RF <- caret::confusionMatrix(data = factor(predictions_RF, levels = c(0, 1)),
                                       reference = factor(test$DILI_Status, levels = c(0, 1)),
                                       positive = "1")
confusion_RF

## Model_XGB
train_pred_XGB <- predict(train_XGB, newdata = train, type = "prob")
train_pred_XGB <- train_pred_XGB[,2]
validation_pred_XGB <- predict(train_XGB, newdata = test, type = "prob")
validation_pred_XGB <- validation_pred_XGB[,2]

auc_XGB <- roc(test$DILI_Status, validation_pred_XGB)
auc_XGB; ci(auc_XGB, method = "bootstrap")

best_cutoff_XGB <- coords(roc(train$DILI_Status, train_pred_XGB), "best", best.method="youden"); best_cutoff_XGB
predictions_XGB <- ifelse(validation_pred_XGB >= best_cutoff_XGB$threshold, 1, 0)

confusion_XGB <- caret::confusionMatrix(data = factor(predictions_XGB, levels = c(0, 1)),
                                        reference = factor(test$DILI_Status, levels = c(0, 1)),
                                        positive = "1")
confusion_XGB

## AUROC
data1 <- data.frame(FPR = coords(auc_LR, best.method = "fpr"), TPR = coords(auc_LR, best.method = "tpr")) %>%
  mutate(Curve = "Logistic (0.848)")
data2 <- data.frame(FPR = coords(auc_RF, best.method = "fpr"), TPR = coords(auc_RF, best.method = "tpr")) %>% 
  mutate(Curve = "Random Forest (0.877)")
data3 <- data.frame(FPR = coords(auc_XGB, best.method = "fpr"), TPR = coords(auc_XGB, best.method = "tpr")) %>% 
  mutate(Curve = "XGBoost (0.873)")
combined_data <- rbind(data1, data2, data3)

custom_theme <- theme(
  panel.background = element_rect(color = "black",fill = NA),
  plot.title = element_text(size = 14, face = "bold"),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12,face = "bold"),
  legend.title = element_blank(),
  legend.text = element_text(size = 12),
  legend.text.align = 1
)

p1 <- ggplot(data = combined_data) +
  geom_line(aes(x = 1 - FPR.specificity, y = TPR.sensitivity, color = Curve), linewidth = 1) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate")+
  xlim(c(0, 1)) + ylim(c(0, 1)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_color_manual(values = c("#390099","#e85d00","#ffc300")) +
  custom_theme + 
  theme(legend.position = c(0.7,0.15))
p1

## AUPRC
aupr_LR <- AUC(obs = Test$DILI_Status, pred = validation_pred_LR,curve = 'PR')
aupr_RF <- AUC(obs = test$DILI_Status,pred = validation_pred_RF,curve = 'PR')
aupr_XGB <- AUC(obs = test$DILI_Status,pred = validation_pred_XGB,curve = 'PR')

data1 <- data.frame(Rec = aupr_LR$thresholds$sensitivity, Pre = aupr_LR$thresholds$precision) %>%
  mutate(Curve = "Logistic (0.670)")
data2 <- data.frame(Rec = aupr_RF$thresholds$sensitivity, Pre = aupr_RF$thresholds$precision) %>% 
  mutate(Curve = "Random Forest (0.727)")
data3 <- data.frame(Rec = aupr_XGB$thresholds$sensitivity, Pre = aupr_XGB$thresholds$precision) %>% 
  mutate(Curve = "XGBoost (0.750)")
combined_data <- rbind(data1, data2, data3)

p2 <- ggplot(data = combined_data) +
  geom_line(aes(x = Rec, y = Pre, color = Curve),size = 1.0) +
  xlab("Recall") +
  ylab("Precision") +
  labs(linetype = "Curve") +
  geom_abline(intercept = 1, slope = -1, linetype = "dashed") +
  scale_x_continuous(limits = c(0, 1)) + scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(values = c("#390099","#e85d00","#ffc300")) +
  custom_theme +
  theme(legend.position = c(0.3,0.15))
p2

## Calibration
bins_LR <- cut(validation_pred_LR, breaks = seq(0, 1, by = 0.01), include.lowest = TRUE)
observed_proportions_LR <- tapply(as.numeric(Test$DILI_Status)-1, bins_LR, mean)
predicted_proportions_LR <- tapply(validation_pred_LR, bins_LR, mean)
brierscore_LR <- mean((as.numeric(Test$DILI_Status)-1 - validation_pred_LR)^2); brierscore_LR
data1 <- data.frame(predicted = predicted_proportions_LR, outcome = observed_proportions_LR) %>% 
  mutate(Curve = "Logistic (0.085)")

bins_RF <- cut(validation_pred_RF, breaks = seq(0, 1, by = 0.01), include.lowest = TRUE)
observed_proportions_RF <- tapply(as.numeric(Test$DILI_Status)-1, bins_RF, mean)
predicted_proportions_RF <- tapply(validation_pred_RF, bins_RF, mean)
brierscore_RF <- mean((as.numeric(test$DILI_Status)-1 - validation_pred_RF)^2); brierscore_RF
data2 <- data.frame(predicted = predicted_proportions_RF, outcome = observed_proportions_RF) %>% 
  mutate(Curve = "Random Forest (0.093)")

bins_XGB <- cut(validation_pred_XGB, breaks = seq(0, 1, by = 0.01), include.lowest = TRUE)
observed_proportions_XGB <- tapply(as.numeric(Test$DILI_Status)-1, bins_XGB, mean)
predicted_proportions_XGB <- tapply(validation_pred_XGB, bins_XGB, mean)
brierscore_XGB <- mean((as.numeric(test$DILI_Status)-1 - validation_pred_XGB)^2); brierscore_XGB
data3 <- data.frame(predicted = predicted_proportions_XGB, outcome = observed_proportions_XGB) %>% 
  mutate(Curve = "XGBoost (0.072)")

combined_data <- rbind(data1, data2, data3)
p3 <- ggplot(data = combined_data, aes(x = predicted, y = outcome, color = Curve)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Mean Predicted Probability", y = "Observed Proportion") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_color_manual(values = c("#390099","#e85d00","#ffc300")) +
  scale_x_continuous(limits = c(0, 1)) + scale_y_continuous(limits = c(0,1)) +
  custom_theme + 
  theme(legend.position = c(0.7,0.15))
p3

# gridExtra::grid.arrange(p1, p2, p3, ncol=3)

## DC Analysis
dcdata_LR <- data.frame(validation_pred_LR, Test$DILI_Status) %>% 
  transmute(validation_pred_LR = validation_pred_LR,
            DILI_Status = as.numeric(Test.DILI_Status)-1)
dc_LR <- decision_curve(DILI_Status ~ validation_pred_LR, data = dcdata_LR,
                        family = binomial(link = 'logit'),
                        thresholds= seq(0,1, by = 0.01),
                        confidence.intervals = 0.95)
dcdata_RF <- data.frame(validation_pred_RF, Test$DILI_Status) %>% 
  transmute(validation_pred_RF = validation_pred_RF,
            DILI_Status = as.numeric(Test.DILI_Status)-1)
dc_RF <- decision_curve(DILI_Status ~ validation_pred_RF, data = dcdata_RF,
                        family = binomial(link = 'logit'),
                        thresholds= seq(0,1, by = 0.01),
                        confidence.intervals = 0.95)
dcdata_XGB <- data.frame(validation_pred_XGB, Test$DILI_Status) %>% 
  transmute(validation_pred_XGB = validation_pred_XGB,
            DILI_Status = as.numeric(Test.DILI_Status)-1)
dc_XGB <- decision_curve(DILI_Status ~ validation_pred_XGB, data = dcdata_XGB,
                         family = binomial(link = 'logit'),
                         thresholds= seq(0,1, by = 0.01),
                         confidence.intervals = 0.95)

plot_decision_curve(list(dc_LR, dc_RF, dc_XGB), 
                    curve.names = c("Logistic", "RandomForest", "XGBoost"),
                    confidence.intervals = "none",
                    cost.benefit.axis = FALSE,
                    col = c("#390099","#e85d00","#ffc300"),
                    xlab = c("Risk threshold"),
                    ylab = c("Net benefit"),
                    ylim = c(0.00, 0.15),
                    legend.position = "none",
                    standardize = FALSE)
legend("topright", legend = c("Logistic","Random Forest","XGBoost"), col = c("#390099","#e85d00","#ffc300"), lty = 1, cex = 0.8)


## SHAP & feature importance
shap_XGB <- shapviz(XGB, X_pred = data.matrix(test[,-1]), X = test)

sv_waterfall(shap_XGB, row_id = 1)
sv_waterfall(shap_XGB, row_id = 23)

p1 <- sv_force(shap_XGB, row_id = 1)
p2 <- sv_force(shap_XGB, row_id = 23)
p <- gridExtra::grid.arrange(p1, p2)

sv_importance(shap_XGB)

sv_importance(shap_XGB, kind = "beeswarm", max_display = 20L)
