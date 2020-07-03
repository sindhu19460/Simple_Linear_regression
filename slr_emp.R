#LOAD EMP.CSV
empl <- read.csv(file.choose())
View(empl)
summary(empl)

#BOXPLOT
boxplot(empl$Salary_hike, empl$Churn_out_rate)

#HIST
hist(empl$Salary_hike)
hist(empl$Churn_out_rate)

plot(empl)
plot(empl$Salary_hike)
plot(empl$Churn_out_rate)

plot(density(empl$Salary_hike))
plot(density(empl$Churn_out_rate))

#SCATTER PLOT
plot(empl$Salary_hike,empl$Churn_out_rate)
plot(density(empl$Salary_hike,empl$Churn_out_rate)) #DATA NORMALIZATION
attach(empl)

#CORRELATION
cor(empl$Salary_hike,empl$Churn_out_rate)

#SIMPLE LINEAR REGRESSION
reg <- lm(Salary_hike~Churn_out_rate)
summary(reg)

#prediction
pred <- predict(reg)
reg$residuals

sum(reg$residuals)
mean(reg$residuals)
sqrt(sum(reg$residuals^2))/nrow(empl)

#CONFIDENCES
confint(reg,level=0.95)

predict(reg,interval = "predict")


library(ggplot2)
ggplot(data = empl, aes(x = Salary_hike, y = Churn_out_rate))+
  geom_point(color = "red") + 
  geom_line(color = "blue", aes(x = Salary_hike, y = Churn_out_rate))


# Logrithamic Model

plot(log(Salary_hike),Churn_out_rate)

cor(log(Salary_hike),Churn_out_rate)

#SIMPLE LINEAR REGRESSION
reg_log <- lm(Salary_hike~Churn_out_rate)
summary(reg_log)

pred_log <- predict(reg_log)
sqrt(sum(reg$residuals^2))/nrow(empl)
confint(reg_log,level = 0.95)
predict(reg_log, interval = "confidence")


# Exponential Model
# x =Salary_hike  and y =Churn_out_rate
plot(Salary_hike, log(Churn_out_rate))
cor(Salary_hike, log(Churn_out_rate))
reg_exp <- lm(log(Churn_out_rate) ~ Salary_hike)  #lm(log(Y) ~ X)

summary(reg_exp)
reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))


logat <- predict(reg_exp)
at <- exp(logat)

error = sai.cal$Churn_out_rate - at
error

sqrt(sum(error^2)/nrow(sai.cal))  #RMSE
confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

##############################
# Polynomial model with 2 degree (quadratic model)

plot(Salary_hike*Salary_hike,Churn_out_rate)

cor(Salary_hike*Salary_hike,Churn_out_rate)     #-0.9017223

plot(Salary_hike*Salary_hike, log(Churn_out_rate))

cor(Salary_hike*Salary_hike, log(Churn_out_rate)) #-0.925803

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

emp_2_dgr <- lm(log(Churn_out_rate) ~ Salary_hike + I(Salary_hike*Salary_hike))
summary(emp_2_dgr)

######################################

Emp_data <- read.csv(file.choose()) #emp_data.csv
summary(Emp_data)

# Variance and Standard deviation of Salary_hike column
var(Emp_data$Salary_hike)
sd(Emp_data$Salary_hike)

# Variance and Standard deviation of Churn_out_rate column
var(Emp_data$Churn_out_rate)
sd(Emp_data$Churn_out_rate)

#SLR LM(X~Y)
Churn_out_rate_Model <- lm(Churn_out_rate ~ Salary_hike, data = Emp_data)
summary(Churn_out_rate_Model)
plot(Churn_out_rate_Model)
