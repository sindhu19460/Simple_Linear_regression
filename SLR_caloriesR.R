cc.wg <- read.csv(file.choose()) # choose the Calories_Consumed.csv data set
View(cc.wg)

# 14 Observations of 2 variables

# Scatter Diagram (Plot x,y)
plot(cc.wg$Calories.Consumed,cc.wg$Weight.gained..grams.)
boxplot(cc.wg)
hist(cc.wg$Weight.gained..grams.)
summary(cc.wg)

# Correlation coefficient value for Calories Consumes and Weight Gained
cc<- cc.wg$Calories.Consumed
wg <- cc.wg$Weight.gained..grams.
cor(wg,cc)

# Simple model without using any transformation
reg<-lm(wg~cc)
summary(reg)
confint(reg,level = 0.95) # confidence interval
predict(reg,interval="predict")

# Logarthmic transformation
reg_log<-lm(wg~log(cc))  # Regression using logarthmic transformation
summary(reg_log)

confint(reg_log,level=0.95)
predict(reg_log,interval="predict")
reg_exp<-lm(log(wg)~cc) # regression using Exponential model
summary(reg_exp)
confint(reg_exp,level=0.95)
exp(predict(reg_exp,interval="predict"))

# Quadratic model
cc.wg[,"CC_sq"] = cc*cc

# Quadratic model
quad_mod <- lm(wg~cc+I(cc^2),data=cc.wg)
summary(quad_mod)
confint(quad_mod,level=0.95)
predict(quad_mod,interval="predict")

# Quadratic model
qd_model <- lm(wg~cc+CC_sq,data=cc.wg)
summary(qd_model)
confint(quad_mod,level=0.95)
predict(quad_mod,interval="predict")

# Cubic model
poly_mod <- lm(wg~cc+I(cc^2)+I(cc^3),data=cc.wg)
summary(poly_mod) # 0.9811

confint(poly_mod,level=0.95)
predict(poly_mod,interval="predict")
# Adjusted R-Squared = 0.9755
#Multiple R -Squared Value = 0.9811

model_R_Squared_values <- list(model=NULL,R_squared=NULL)
model_R_Squared_values[["model"]] <- c("reg","reg_log","reg_exp","quad_mod","poly_mod")
model_R_Squared_values[["R_squared"]] <- c(0.8968,0.7917,0.8674,0.9433,0.9755)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]])
View(model_R_Squared_values)

# Cubic  model gives the best Adjusted R-Squared value
pred_final <- predict(poly_mod)
pred_final

rmse<-sqrt(mean((pred_final-wg)^2))
rmse
plot(poly_mod)
hist(residuals(poly_mod)) # close to normal distribution


######################################
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