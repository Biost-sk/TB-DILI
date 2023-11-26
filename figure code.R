data1 <- data.frame(predicted = predicted_proportions, outcome = observed_proportions)
data2 <- data.frame(predicted = predicted_proportions_LR, outcome = observed_proportions_LR)
data3 <- data.frame(predicted = predicted_proportions_XGB, outcome = observed_proportions_XGB)
library(dplyr)

data1 <- data1 %>% mutate(Curve = "Random Forest")
data2 <- data2 %>% mutate(Curve = "Logistic")
data3 <- data3 %>% mutate(Curve = "XGBoost")
combined_data <- rbind(data1, data2, data3)


library(ggplot2)
ggplot(data = combined_data, aes(x = predicted, y = outcome)) +
  geom_smooth(method = "loess", se = FALSE, color = "#000000") +
  geom_point(size = 1) +
  labs(x = "Mean Predicted Probability", y = "Observed Proportion") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#000000") +
  facet_wrap(~ Curve, ncol = 1) +
  custom_theme

ggplot(data = combined_data) +
  geom_(aes(x = predicted, y = outcome, color = "Curve")) +
  xlab("Mean Predicted Probability") +
  ylab("Observed Proportion") +
  ggtitle("Calibration Curves") +
  scale_color_manual(values = c("Random Forest" = "#0072B2", "Logistic" = "#009E73", "XGBoost" = "#D55E00")) +
  custom_theme

auc
auc_LR
auc_XGB
library(pROC)
auc_value_RF <- auc(auc)

# Calculate 95% confidence interval
ci_95_RF <- ci(auc, method = "bootstrap")

# Print the results
cat("AUC:", auc_value_RF, "\n")
cat("95% CI:", ci_95_RF, "\n")

auc_value_LR <- auc(auc_LR)

# Calculate 95% confidence interval
ci_95_LR <- ci(auc_LR, method = "bootstrap")

# Print the results
cat("AUC:", auc_value_LR, "\n")
cat("95% CI:", ci_95_LR, "\n")

auc_value_XGB <- auc(auc_XGB)

# Calculate 95% confidence interval
ci_95_XGB <- ci(auc_XGB, method = "bootstrap")

# Print the results
cat("AUC:", auc_value_XGB, "\n")
cat("95% CI:", ci_95_XGB, "\n")


#AUROC
auc
auc_LR
auc_XGB
data1 <- data.frame(FPR = coords(auc, best.method = "fpr"), TPR = coords(auc, best.method = "tpr"))
data2 <- data.frame(FPR = coords(auc_LR, best.method = "fpr"), TPR = coords(auc_LR, best.method = "tpr"))
data3 <- data.frame(FPR = coords(auc_XGB, best.method = "fpr"), TPR = coords(auc_XGB, best.method = "tpr"))
library(dplyr)

data1 <- data1 %>% mutate(Curve = "Random Forest")
data2 <- data2 %>% mutate(Curve = "Logistic")
data3 <- data3 %>% mutate(Curve = "XGBoost")
combined_data <- rbind(data1, data2, data3)

custom_theme <- theme(
  panel.background = element_rect(color = "black",fill = NA),
  plot.title = element_text(size = 14, face = "bold"),
  axis.title = element_text(size = 12),
  axis.text = element_text(size = 10,face = "bold"),
  legend.title = element_blank(),
  legend.text = element_text(size = 10),
  legend.position = c(0.75,0.15),
  legend.text.align = 1
)

ggplot(data = combined_data) +
  geom_line(aes(x = 1 - FPR.specificity, y = TPR.sensitivity, linetype = Curve),size = 1.0) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
#  ggtitle("AUROC Curve") +
  scale_linetype_manual(
    values = c("Random Forest" = "solid", "XGBoost" = "dotted","Logistic" = "twodash"),
    labels = c("Logistic: 0.844 (0.818, 0.870)","Random Forest: 0.863 (0.839, 0.884)","XGBoost: 0.862 (0.836, 0.886)" )
  )+
  labs(linetype = "Curve")+
  custom_theme+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#000000")

#AUPR
aupr
aupr_LR
aupr_XGB
data1 <- data.frame(Rec = aupr$thresholds$sensitivity, Pre = aupr$thresholds$precision)
data2 <- data.frame(Rec = aupr_LR$thresholds$sensitivity, Pre = aupr_LR$thresholds$precision)
data3 <- data.frame(Rec = aupr_XGB$thresholds$sensitivity, Pre = aupr_XGB$thresholds$precision)
library(dplyr)

data1 <- data1 %>% mutate(Curve = "Random Forest")
data2 <- data2 %>% mutate(Curve = "Logistic")
data3 <- data3 %>% mutate(Curve = "XGBoost")
combined_data <- rbind(data1, data2, data3)

custom_theme <- theme(
  panel.background = element_rect(color = "black",fill = NA),
  plot.title = element_text(size = 14, face = "bold"),
  axis.title = element_text(size = 12),
  axis.text = element_text(size = 10,face = "bold"),
  legend.title = element_blank(),
  legend.text = element_text(size = 10),
  legend.position = c(0.75,0.85),
  legend.text.align = 1
)

ggplot(data = combined_data) +
  geom_line(aes(x = Rec, y = Pre, linetype = Curve),size = 1.0) +
  xlab("Recall") +
  ylab("Precision") +
  #  ggtitle("AUROC Curve") +
  scale_linetype_manual(
    values = c("Random Forest" = "solid", "XGBoost" = "dotted","Logistic" = "twodash"),
    labels = c("Logistic: 0.666","Random Forest: 0.687","XGBoost: 0.686" )
  )+
  labs(linetype = "Curve")+
  custom_theme
  geom_abline(intercept = 1.00, slope = -1, linetype = "dashed", color = "#000000")

#SHAP, feature importance
library(DALEX)

rf_exp <- DALEX::explain(final_model,
                         data = train_data[,-1],
                         y = train_data$DILI_Status=="1",
                         label = "randomForest")
library(ingredients)
imp_rf <- feature_importance(rf_exp)
imp_rf
plot(imp_rf)


lg_exp <- DALEX::explain(model_LR,
                         data = train_data[,-1],
                         y = train_data$DILI_Status=="1",
                         label = "Logistic")
xgb_exp <- DALEX::explain(xgb,
                     data = as.matrix(train_data[, -1]),
                     y = train_data$DILI_Status,
                     label = "XGBoost Model")
imp_lg <- feature_importance(lg_exp)
imp_lg


importance_matrix <- xgb.importance(model = xgb)
print(importance_matrix)

plot(imp_rf,imp_lg)

#feature importance
data_imf<-read.csv("IMF.csv")

data_imf_lr<-data_imf[data_imf$Model=="Logistic",]
data_imf_rf<-data_imf[data_imf$Model=="RandomForest",]
data_imf_xgb<-data_imf[data_imf$Model=="XGBoost",]

plot1 <- ggplot(data_imf_lr, aes(x = reorder(Features, -Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Logistic",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  coord_flip()+
  theme(
    plot.title = element_text(size = 10),  # Adjust the size and style of the title
    axis.title = element_text(size = 14, face = "bold"),  # Adjust the size of the axis titles
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  )
plot2 <- ggplot(data_imf_rf, aes(x = reorder(Features, -Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Random Forest",
       x = "Features",
       y = NULL) +
  theme_minimal() +
  coord_flip()+
  theme(
    plot.title = element_text(size = 10),  # Adjust the size and style of the title
    axis.title = element_text(size = 14, face = "bold"),  # Adjust the size of the axis titles
    axis.text = element_text(size = 10),    # Adjust the size of the axis labels
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  )
plot3 <- ggplot(data_imf_xgb, aes(x = reorder(Features, -Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "XGBoost",
       x = NULL,
       y = "Importance") +
  theme_minimal() +
  coord_flip()+
  theme(
    plot.title = element_text(size = 10),  # Adjust the size and style of the title
    axis.title = element_text(size = 14, face = "bold"),  # Adjust the size of the axis titles
    axis.text = element_text(size = 10),    # Adjust the size of the axis labels
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  )

combined_plot <- plot1 / plot2 / plot3 +
  plot_layout(ncol = 1)

# Display the combined plot
print(combined_plot)



pdp_rf <- model_profile(rf_exp,variables = "ALT_max_ratio_baseline",groups = "age_cat")
pdp_rf

ggplot(data, aes(x, y, color = group)) +
  geom_line() +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "cyan", "pink", "brown")) +
  labs(title = "Curve Plot",
       x = "X-axis",
       y = "Y-axis") +
  theme_minimal() +
  guides(color = guide_legend(title = "Group Names",  # Set the legend title
                              breaks = c("1", "2", "3", "4", "5", "6", "7", "8"),  # Set the breaks
                              labels = c("Label 1", "Label 2", "Label 3", "Label 4", 
                                         "Label 5", "Label 6", "Label 7", "Label 8")))  # Set the labels
 plot(pdp_rf)+
  ggtitle("Partial-dependence profile for age")+
  coord_cartesian(xlim = c(0, 2))+
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "cyan", "pink", "brown")) +
  theme_minimal() +
  guides(color = guide_legend(title = "Age",  # Set the legend title
                              labels = c("Label 1", "Label 2", "Label 3", "Label 4", 
                                        "Label 5", "Label 6", "Label 7", "Label 8")))   # Set the labels

pdp_lg <- model_profile(lg_exp, variables = "D01V040Once")
pdp_rf <- model_profile(rf_exp, variables = "D01V040Once")

plot(pdp_rf, pdp_lg)
a1 <- plot(pdp_rf, pdp_lg) +
  coord_cartesian(xlim = c(0, 1))+
  theme_minimal() 
print(a1)

pdp_lg <- model_profile(lg_exp, variables = "D01V070Once")
pdp_rf <- model_profile(rf_exp, variables = "D01V070Once")

plot(pdp_rf, pdp_lg)
a2 <- plot(pdp_rf, pdp_lg) +
  coord_cartesian(xlim = c(0, 1))+
  theme_minimal() 
print(a2)

pdp_lg <- model_profile(lg_exp, variables = "D02V003Once")
pdp_rf <- model_profile(rf_exp, variables = "D02V003Once")

plot(pdp_rf, pdp_lg)
a3 <- plot(pdp_rf, pdp_lg) +
  coord_cartesian(xlim = c(0, 1))+
  theme_minimal() 
print(a3)

pdp_lg <- model_profile(lg_exp, variables = "ALT_max_ratio_baseline")
pdp_rf <- model_profile(rf_exp, variables = "ALT_max_ratio_baseline")

plot(pdp_rf, pdp_lg)
a4 <- plot(pdp_rf, pdp_lg) +
  coord_cartesian(xlim = c(0, 2))+
  theme_minimal() 
print(a4)

pdp_lg <- model_profile(lg_exp, variables = "ALP_max_ratio_baseline")
pdp_rf <- model_profile(rf_exp, variables = "ALP_max_ratio_baseline")

plot(pdp_rf, pdp_lg)
a5 <- plot(pdp_rf, pdp_lg) +
  coord_cartesian(xlim = c(0, 2))+
  theme_minimal() 
print(a5)

pdp_lg <- model_profile(lg_exp, variables = "Tbil_max_ratio_baseline")
pdp_rf <- model_profile(rf_exp, variables = "Tbil_max_ratio_baseline")

plot(pdp_rf, pdp_lg)
a6 <- plot(pdp_rf, pdp_lg) +
  coord_cartesian(xlim = c(0, 2))+
  theme_minimal() 
print(a6)

library(gridExtra)
# Assuming plot1, plot2, ..., plot6 are your individual plots

grid.arrange(a1, a2, a3, a4, a5, a6, ncol = 2)


install.packages("vivo")
library(vivo)
p_rf <- model_profile(rf_exp)
p_lg <- model_profile(lg_exp)
p_xgb <- model_profile(xgb_exp)
plot(p_rf)
plot(p_lg)

measure_rf<-global_variable_importance(p_rf)
plot(measure_rf)
measure_lg<-global_variable_importance(p_lg)
plot(measure_lg)

install.packages("pdp")
library(pdp)
partial(final_model,
        pred.var = "Sex",
        ice = T,
        plot = T,
        alpha = 0.2)


#SHAP FOR XGBoost
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



