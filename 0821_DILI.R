# DILI样本数据集 描述性分析

# 清除当前环境
rm(list=ls())
# 设置路径
setwd("C:/Users/kongsy7224/Desktop/DILI raw data")
# 设置中文语言环境
Sys.setlocale(category = "LC_ALL", locale = "Chinese")
# 加载包
library(tidyverse)
library(dplyr)
library(data.table)
library(readxl)
library(stringi)
library(lubridate)
library(tableone)

# 原始数据载入
demographics <- read.csv("demographics.csv") #人口信息
vitals <- read.csv("vitals.csv") #生命体征
medical_history <- read.csv("medical_history.csv") #既往史
opt_encounter <- read.csv("opt_encounter.csv") #门诊信息
opt_drug <- read.csv("opt_drug.csv") #门诊处方
ipt_encounter <- read.csv("ipt_encounter.csv") #住院信息
ipt_drug <- read.csv("ipt_drug.csv") #住院处方
lab <- read.csv("lab.csv") #实验室检查


# 数据整理与格式转换
# 因子型变量
table(demographics$sex)
demographics$sex <- factor(demographics$sex, labels = c(0,1), levels = c("女", "男")) #sex 1 = 男, 0 = 女

table(demographics$education)
demographics <- demographics %>% 
  mutate(education = case_when(
    education %in% c("文盲或半文盲", "小学") ~ 1,
    education %in% c("初中") ~ 2,
    education %in% c("中等专业学校", "高中") ~ 3,
    education %in% c("大学专科和专科学校", "技工学校") ~ 4,
    education %in% c("大学本科", "研究生") ~ 5,
  )) %>% 
  mutate(education = factor(education)) #education 1 = 小学及以下, 2 = 初中, 3 = 高中, 4 = 大专, 5 = 大学及以上 

table(demographics$marriage)
demographics$marriage <- factor(demographics$marriage, labels = c(0, 1, 2, 3),levels = c("未婚", "已婚", "离婚", "丧偶"))
#marriage 0 = 未婚, 1 = 已婚, 2 = 离婚, 3 = 丧偶 

# 测量单位统一化
table(vitals$data_type,vitals$units)
vitals$result <- if_else(vitals$units == "m", as.character(as.numeric(vitals$result) * 100),
                         if_else(vitals$result %in% c("-", "0", "未检"), NA, vitals$result))

medical_history$med_history <- if_else(grepl("0|-|无|未知|不详", medical_history$med_history) == TRUE,
                                       NA ,medical_history$med_history)
table(medical_history$med_history)


# 分别建立使用 甘草酸二钠 水飞蓟 护肝片 的患者队列
# 筛选门诊与住院处方中的药品信息列中，三类药品的所有处方记录
Disodium.1 <- ipt_drug %>% 
  filter(grepl("甘草酸苷|甘草酸二铵",ipt_drug$item_name))
Disodium.2 <- opt_drug %>% 
  filter(grepl("甘草酸苷|甘草酸二铵",opt_drug$item_name))
Disodium <- rbindlist(list(Disodium.1, Disodium.2), fill = TRUE)
# 依据idcard列, 将人口学信息和用药信息链接
cohort1 <- semi_join(demographics, Disodium, by = "idcard") #N = 285

Silibinin.1 <- ipt_drug %>% 
  filter(grepl("水飞蓟",ipt_drug$item_name))
Silibinin.2 <- opt_drug %>% 
  filter(grepl("水飞蓟",opt_drug$item_name))
Silibinin <- rbindlist(list(Silibinin.1, Silibinin.2), fill = TRUE)
cohort2 <- semi_join(demographics, Silibinin, by = "idcard") #N = 186

Hugan.1 <- ipt_drug %>% 
  filter(grepl("护肝",ipt_drug$item_name))
Hugan.2 <- opt_drug %>% 
  filter(grepl("护肝",opt_drug$item_name))
Hugan <- rbindlist(list(Hugan.1, Hugan.2), fill = TRUE)
cohort3 <- semi_join(demographics, Hugan, by = "idcard") #N = 47


# 以甘草酸队列为例, 完整的队列信息整理步骤
cohort1$disodium <- 1
cohort1 <- left_join(demographics,cohort1)
cohort1 <- cohort1 %>% 
  mutate(disodium = replace_na(cohort1$disodium,0),
         disodium = as.factor(disodium))
# 1处理时间数据
#将字符型时间格式进行转换
cohort1$dob <- ymd(cohort1$dob)
Disodium$clinic_time <- ymd_hms(Disodium$clinic_time)
#记录用药信息中首次出现 甘草酸苷/甘草酸二铵的时间, 记为start_time
#对于未使用药物者,将门诊/住院信息中首次出现肺结核的时间, 记为start_time
time.1 <- Disodium %>% 
  group_by(idcard) %>% 
  summarise_at(vars(clinic_time), min) %>% 
  rename(start_time = clinic_time)
diag <- rbindlist(list(opt_encounter, ipt_encounter), fill = TRUE) %>% 
  transmute(idcard, org_name,
            time_point = if_else(is.na(clinic_time), adm_time, clinic_time),
            icd = if_else(is.na(icd), adm_ICD, icd),
            disease_name = if_else(is.na(disease_name), adm_disease_name, disease_name)) %>% 
  mutate(time_point = ymd_hms(time_point))#合并所有诊断信息
time.2 <- diag %>% 
  anti_join(time.1,by="idcard") %>% 
  filter(grepl("肺结核", disease_name)) %>% 
  group_by(idcard) %>% 
  summarise_at(vars(time_point), min) %>% 
  rename(start_time = time_point)
time <- rbind(time.1, time.2)
cohort1 <- left_join(cohort1, time, by="idcard")
#由dob, start_time计算出年龄, 放置在dob列之后
cohort1 <- cohort1 %>% 
  mutate(age = floor(as.numeric(difftime(start_time, dob, units = "days")/365))) %>% 
  relocate(age, .after = "dob")

# 2 链接demographics medical_history vitals表单
#吸烟与饮酒史
smodri <- medical_history %>% 
  group_by(idcard) %>% 
  summarise_at(vars(smoking, drinking), max)
cohort1<- left_join(cohort1, smodri, by = "idcard")
#vitals中身高/体重/心率/血糖/血压, 各自取距离用药起始时间最近的一条记录
height <- vitals %>% 
  mutate(time_point = ymd_hms(vitals$time_point)) %>% 
  left_join(cohort1[, c("idcard", "start_time")], by = "idcard") %>% 
  filter(data_type == "身高", time_point < start_time) %>% 
  arrange(idcard, time_point) %>% 
  group_by(idcard) %>% 
  summarise_at(vars(result), last) %>% 
  rename(height = result)
weight <- vitals %>% 
  mutate(time_point = ymd_hms(vitals$time_point)) %>% 
  left_join(cohort1[, c("idcard", "start_time")], by = "idcard") %>% 
  filter(data_type == "体重", time_point < start_time) %>% 
  arrange(idcard, time_point) %>% 
  group_by(idcard) %>% 
  summarise_at(vars(result), last) %>% 
  rename(weight = result)
hr <- vitals %>% 
  mutate(time_point = ymd_hms(vitals$time_point)) %>% 
  left_join(cohort1[, c("idcard", "start_time")], by = "idcard") %>% 
  filter(data_type == "心率", time_point < start_time) %>% 
  group_by(idcard) %>% 
  arrange(time_point) %>% 
  summarise_at(vars(result), last) %>% 
  rename(hr = result)
gly <- vitals %>% 
  mutate(time_point = ymd_hms(vitals$time_point)) %>% 
  left_join(cohort1[, c("idcard", "start_time")], by = "idcard") %>% 
  filter(data_type == "血糖", time_point < start_time) %>% 
  group_by(idcard) %>% 
  arrange(time_point) %>% 
  summarise_at(vars(result), last) %>% 
  rename(gly = result)
bp <- vitals %>% 
  mutate(time_point = ymd_hms(vitals$time_point)) %>% 
  left_join(cohort1[, c("idcard", "start_time")], by = "idcard") %>% 
  filter(data_type == "血压", time_point < start_time) %>% 
  group_by(idcard) %>% 
  arrange(time_point) %>% 
  summarise_at(vars(result), last) %>% 
  rename(bp = result)
cohort1 <- cohort1 %>% 
  left_join(height, by = "idcard") %>% 
  left_join(weight, by = "idcard") %>% 
  left_join(hr, by = "idcard") %>% 
  left_join(gly, by = "idcard") %>% 
  left_join(bp, by = "idcard")

# 3 提取该队列实验室检查数据
#筛选所有谷丙转氨酶alt 谷草转氨酶ast 总胆红素, 计算每个检测时间与start_time的差
alt <- lab %>% 
  filter(grepl(c("谷丙转氨酶|丙氨酸氨基转移酶|丙氨酸转氨酶"),item_name)) %>% 
  mutate(time_point = ymd_hms(time_point)) %>% 
  left_join(cohort1[, c("idcard","start_time")]) %>% 
  mutate(tdiff = as.numeric(difftime(start_time, time_point, units = "days")))
ast <- lab %>% 
  filter(grepl(c("谷草转氨酶|天冬氨酸氨基转移酶|门冬氨酸转移酶|天门冬氨酸氨基转移酶"),item_name)) %>% 
  mutate(time_point = ymd_hms(time_point)) %>% 
  left_join(cohort1[, c("idcard","start_time")]) %>% 
  mutate(tdiff = as.numeric(difftime(start_time, time_point, units = "days")))
tbil <- lab %>% 
  filter(grepl(c("总胆红素"),item_name)) %>% 
  mutate(time_point = ymd_hms(time_point)) %>% 
  left_join(cohort1[, c("idcard","start_time")]) %>% 
  mutate(tdiff = as.numeric(difftime(start_time, time_point, units = "days")))
#计算首次用药前, 30d后, 60d后, 90d后的指标值
alt_0 <- alt %>% 
  filter(tdiff < 0) %>% 
  arrange(idcard, time_point) %>% 
  group_by(idcard) %>% 
  summarize_at(vars(result), last) %>% 
  rename(alt_0 = result)
alt_30 <- alt %>% 
  filter(tdiff > 0 & tdiff < 30) %>% 
  arrange(idcard, time_point) %>% 
  group_by(idcard) %>% 
  summarize_at(vars(result), last) %>% 
  rename(alt_30 = result)
alt_60 <- alt %>% 
  filter(tdiff > 30 & tdiff < 60) %>% 
  arrange(idcard, time_point) %>% 
  group_by(idcard) %>% 
  summarize_at(vars(result), last) %>% 
  rename(alt_60 = result)
alt_90 <- alt %>% 
  filter(tdiff > 60) %>% 
  arrange(idcard, time_point) %>% 
  group_by(idcard) %>% 
  summarize_at(vars(result), first) %>% 
  rename(alt_90 = result)
ast_0 <- ast %>% 
  filter(tdiff < 0) %>% 
  arrange(idcard, time_point) %>% 
  group_by(idcard) %>% 
  summarize_at(vars(result), last) %>% 
  rename(ast_0 = result)
ast_30 <- ast %>% 
  filter(tdiff > 0 & tdiff < 30) %>% 
  arrange(idcard, time_point) %>% 
  group_by(idcard) %>% 
  summarize_at(vars(result), last) %>% 
  rename(ast_30 = result)
ast_60 <- ast %>% 
  filter(tdiff > 30 & tdiff < 60) %>% 
  arrange(idcard, time_point) %>% 
  group_by(idcard) %>% 
  summarize_at(vars(result), last) %>% 
  rename(ast_60 = result)
ast_90 <- ast %>% 
  filter(tdiff > 60) %>% 
  arrange(idcard, time_point) %>% 
  group_by(idcard) %>% 
  summarize_at(vars(result), first) %>% 
  rename(ast_90 = result)
tbil_0 <- tbil %>% 
  filter(tdiff < 0) %>% 
  arrange(idcard, time_point) %>% 
  group_by(idcard) %>% 
  summarize_at(vars(result), last) %>% 
  rename(tbil_0 = result)
tbil_30 <- tbil %>% 
  filter(tdiff > 0 & tdiff < 30) %>% 
  arrange(idcard, time_point) %>% 
  group_by(idcard) %>% 
  summarize_at(vars(result), last) %>% 
  rename(tbil_30 = result)
tbil_60 <- tbil %>% 
  filter(tdiff > 30 & tdiff < 60) %>% 
  arrange(idcard, time_point) %>% 
  group_by(idcard) %>% 
  summarize_at(vars(result), last) %>% 
  rename(tbil_60 = result)
tbil_90 <- tbil %>% 
  filter(tdiff > 60) %>% 
  arrange(idcard, time_point) %>% 
  group_by(idcard) %>% 
  summarize_at(vars(result), first) %>% 
  rename(tbil_90 = result)
#将所有检查值链接到cohort1上
cohort1 <- cohort1 %>% 
  left_join(alt_0) %>% left_join(alt_30) %>% left_join(alt_60) %>% left_join(alt_90) %>% 
  left_join(ast_0) %>% left_join(ast_30) %>% left_join(ast_60) %>% left_join(ast_90) %>% 
  left_join(tbil_0) %>% left_join(tbil_30) %>% left_join(tbil_60) %>% left_join(tbil_90)

# 绘制描述性表格, 按照使用甘草酸/未使用甘草酸分组
list1 <- cohort1 %>% select(age, sex, education, marriage, smoking, drinking, height, weight, hr, gly, disodium,
                            alt_0, alt_30, alt_60, alt_90, ast_0, ast_30, ast_60, ast_90, tbil_0, tbil_30, tbil_60, tbil_90) %>% 
  mutate_at(vars(smoking, drinking), as.factor) %>% 
  mutate_at(vars(height, weight, hr, gly, alt_0, alt_30, alt_60, alt_90, 
                 ast_0, ast_30, ast_60, ast_90, tbil_0, tbil_30, tbil_60, tbil_90), as.numeric)
tb1 <- CreateTableOne(strata = "disodium", data = list1, addOverall = TRUE) %>% 
  print(showAllLevels = TRUE)

