rm(list=ls())

# 引入library
library(ggplot2)
library(reshape2)
library(ROCR)

# 引入样本，划分Train与Test
diamonds$is_expensive <- diamonds$price > 2400
is_test <- runif(nrow(diamonds)) > 0.75
train <- diamonds[is_test==FALSE,]
test <- diamonds[is_test==TRUE,]

################### Logistic Model A ################### 
fit_A <- glm(is_expensive ~ carat + cut + clarity, data=train)

# 预测
prob_A <- predict(fit_A, newdata=test, type="response")
pred_A <- prediction(prob_A, test$is_expensive)
perf_A <- performance(pred_A, measure = "tpr", x.measure = "fpr")

# 计算AUC
auc_A <- performance(pred_A, measure = "auc")
auc_A <- auc_A@y.values[[1]]

# ROC曲线数据
roc.data_A <- data.frame(fpr=unlist(perf_A@x.values),
                         tpr=unlist(perf_A@y.values),
                         model="GLM_A")

################### Logistic Model B ################### 					   
fit_B <- glm(is_expensive ~ carat, data=train)

# 预测
prob_B <- predict(fit_B, newdata=test, type="response")
pred_B <- prediction(prob_B, test$is_expensive)
perf_B <- performance(pred_B, measure = "tpr", x.measure = "fpr")

# 计算AUC
auc_B <- performance(pred_B, measure = "auc")
auc_B <- auc_B@y.values[[1]]

# ROC曲线数据
roc.data_B <- data.frame(fpr=unlist(perf_B@x.values),
                         tpr=unlist(perf_B@y.values),
                         model="GLM_B")

################### 绘制ROC曲线 ################### 		

# 将两个模型的结果放在一个data frame下
roc.data <- rbind(roc.data_A,roc.data_B)  
summary(roc.data)

ggplot(roc.data, aes(x=fpr, y=tpr, color=model)) + geom_line()



################### AUC面积 ################### 		
auc_A <- performance(pred_A, measure = "auc")
auc_A <- auc_A@y.values[[1]]

auc_B <- performance(pred_B, measure = "auc")
auc_B <- auc_B@y.values[[1]]


################### Gini系数 ################### 		
gini_A <- 2*auc_A - 1
gini_B <- 2*auc_B - 1


