Calories_consumed <- read.csv(file.choose()) #calories_consumed.csv
summary(Calories_consumed)
# Variance and Standard deviation of Calories.Consumed column
var(Calories_consumed$Calories.Consumed)
# Variance and Standard deviation of Weight.gained..grams. column
var(Calories_consumed$Weight.gained..grams.)
sd(Calories_consumed$Weight.gained..grams.)
WeightGainModel <- lm(Weight.gained..grams. ~ Calories.Consumed, data = Calories_consumed)
summary(WeightGainModel)
plot(Calories_consumed)
delivery_time <- read.csv(file.choose()) #delivery_time.csv
summary(delivery_time)
# Variance and Standard deviation of Delivery.Time column
var(delivery_time$Delivery.Time)
sd(delivery_time$Delivery.Time)
# Variance and Standard deviation of Sorting.Time column
var(delivery_time$Sorting.Time)
sd(delivery_time$Sorting.Time)
deliverTimeModel <- lm(Delivery.Time ~ Sorting.Time, data = delivery_time)
summary(deliverTimeModel)
plot(deliverTimeModel)
library(mvinfluence)
influenceIndexPlot(deliverTimeModel)
deliverTimeModel <- lm(Delivery.Time ~ Sorting.Time, data = delivery_time[c(-5,-9,-21),])
summary(deliverTimeModel)
plot(deliverTimeModel)
Emp_data <- read.csv(file.choose()) #emp_data.csv
summary(Emp_data)
# Variance and Standard deviation of Salary_hike column
var(Emp_data$Salary_hike)
sd(Emp_data$Salary_hike)
# Variance and Standard deviation of Churn_out_rate column
var(Emp_data$Churn_out_rate)
sd(Emp_data$Churn_out_rate)
Churn_out_rate_Model <- lm(Churn_out_rate ~ Salary_hike, data = Emp_data)
summary(Churn_out_rate_Model)
plot(Churn_out_rate_Model)
Salary_hike <- read.csv(file.choose())
summary(Salary_hike)
# Variance and Standard deviation of Salary_hike column
var(Salary_hike$YearsExperience)
sd(Salary_hike$YearsExperience)
# Variance and Standard deviation of Churn_out_rate column
var(Salary_hike$Salary)
sd(Salary_hike$Salary)
#Creating Linear Model for Salary_hike

Salary_hike_Model <- lm(Salary ~ YearsExperience, data = Salary_hike)
summary(Salary_hike_Model)
plot(Salary_hike_Model)
