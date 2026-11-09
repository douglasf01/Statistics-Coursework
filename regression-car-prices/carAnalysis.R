# MSG501 project, part 1: regression analysis of car prices.
# Run from this folder: Rscript carAnalysis.R
# All plots are saved as png files in this folder.

# Data and exploratory plots
###################################################################
###################################################################

# Read the car price data
carprices <- read.csv("CarPrice.csv")

#Creating categorical variables with reference level as described
carprices$aspiration <- factor(carprices$aspiration)
carprices$carbody <- factor(carprices$carbody)
carprices$drivewheel <- factor(carprices$drivewheel)
carprices$cylindernumber <- factor(carprices$cylindernumber)
carprices$fueltype <- factor(carprices$fueltype)

carprices$aspiration <- relevel(carprices$aspiration, ref = "std")
carprices$carbody <- relevel(carprices$carbody, ref = "hatchback")
carprices$drivewheel <- relevel(carprices$drivewheel, ref = "rwd")
carprices$cylindernumber <- relevel(carprices$cylindernumber, ref = "four")
carprices$fueltype <- relevel(carprices$fueltype, ref = "gas")

# The rest of the script uses both names for the same data
CarPrice <- carprices



#Plotting
###################################################################

# Histogram of price
png("price_histogram.png", width = 500, height = 500)
hist(CarPrice$price,
     breaks = 30,               
     col = "skyblue",
     border = "black",
     main = "Distribution of Car Prices",
     xlab = "Price",
     ylab = "Count")
invisible(dev.off())


# Scatterplots of price vs key numerical predictors

# Price vs enginesize
png("price_vs_enginesize.png", width = 500, height = 500)
plot(CarPrice$enginesize, CarPrice$price,
     main="Price vs Engine Size",
     xlab="Engine Size",
     ylab="Price",
     pch=19, col="blue")
abline(lm(price ~ enginesize, data=CarPrice), col="red")  
invisible(dev.off())
# Price vs horsepower
png("price_vs_horsepower.png", width = 500, height = 500)
plot(CarPrice$horsepower, CarPrice$price,
     main="Price vs Horsepower",
     xlab="Horsepower",
     ylab="Price",
     pch=19, col="green")
abline(lm(price ~ horsepower, data=CarPrice), col="red")  
invisible(dev.off())

# Price vs curbweight
png("price_vs_curbweight.png", width = 500, height = 500)
plot(CarPrice$curbweight, CarPrice$price,
     main="Price vs Curbweight",
     xlab="Curbweight",
     ylab="Price",
     pch=19, col="red")
abline(lm(price ~ curbweight, data=CarPrice), col="blue")  
invisible(dev.off())

# Price vs wheelbase
png("price_vs_wheelbase.png", width = 500, height = 500)
plot(CarPrice$wheelbase, CarPrice$price,
     main="Price vs Wheelbase",
     xlab="Wheelbase",
     ylab="Price",
     pch=19, col="purple")
abline(lm(price ~ wheelbase, data=CarPrice), col="red")  
invisible(dev.off())


# Boxplots for selected categorical variables

# Price vs fueltype
png("price_vs_fueltype.png", width = 500, height = 500)
boxplot(price ~ fueltype, data = CarPrice,
        col = "lightgreen",
        main = "Price vs Fuel Type",
        xlab = "Fuel Type",
        ylab = "Price")
invisible(dev.off())


# Price vs carbody
png("price_vs_carbody.png", width = 500, height = 500)
boxplot(price ~ carbody, data = CarPrice,
        col = "lightblue",
        main = "Price vs Car Body",
        xlab = "Car Body",
        ylab = "Price",
        las = 0,               
        cex.axis = 1,       
        cex.lab = 1)
invisible(dev.off())

# Price vs drivewheel
png("price_vs_drivewheel.png", width = 500, height = 500)
boxplot(price ~ drivewheel, data = CarPrice,
        col = "lightblue",
        main = "Price vs drivewheel",
        xlab = "Drivewheel",
        ylab = "Price",
        las = 0,               
        cex.axis = 1)
invisible(dev.off())



# Correlation heatmap of numerical varicables
library(corrplot)

selected_vars <- CarPrice[, c("enginesize", "horsepower", "curbweight", "wheelbase")]

# Compute the correlation matrix
cor_matrix <- cor(selected_vars)

# Plot the correlation matrix
png("correlation_heatmap.png", width = 600, height = 600)
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black", 
         tl.col = "black", tl.cex = 1, 
         col = colorRampPalette(c("white", "red", "orange", "yellow", "red"))(256),
         title = "Correlation Heatmap of Key Variables",
         mar = c(0, 0, 2, 0))
invisible(dev.off())

#VIF tests
###################################################################
#Settings seed for document
set.seed(123)

#VIF tests
library("car")

train_indices <- sample(1:nrow(CarPrice), size = 0.6 * nrow(CarPrice))  # 60% for training
carprices_train <- CarPrice[train_indices, ]
carprices_test <- CarPrice[-train_indices, ]

model1 <- lm(price ~ enginesize + horsepower + curbweight + wheelbase , data = carprices_train)
vif(model1)

model2 <- lm(price ~ enginesize + horsepower + wheelbase  , data = carprices_train)
vif(model2)

#First regression
model3 <- lm(price ~ enginesize + horsepower + wheelbase + fueltype + carbody + drivewheel  , data = carprices_train)
print(summary(model3))

#Assumptions checking of model3 (in appendix)
###################################################################
#Used to check changes in slope to spot outliers
lm1 <- lm.influence(model3)

#Normality
png("normality_model1.png", width = 500, height = 500)
plot(model3, which = 2, main="First model: Normality assumptions")
invisible(dev.off())

#Cooks
png("cooks_model1.png", width = 500, height = 500)
plot(model3, which = 4, main="First model: Cooks test")
invisible(dev.off())
#Leverage
png("leverage_model1.png", width = 500, height = 500)
plot(model3, which = 5, main="First model: Leverage")
invisible(dev.off())

#Changes in slope
png("enginesize_change_model1.png", width = 500, height = 500)
#Trying to diagnose residuals using 2th and 98th percintile
plot(lm1$coeff[,1],main="First model:Change in enginesize slope")
values <- lm1$coeff[,1]
q <- quantile(values, probs = c(0.02, 0.98))
iqr <- IQR(values)
outliers <- which(values < q[1] - 1.5 * iqr | values > q[2] + 1.5 * iqr)
print(outliers)
text(x = outliers, y = values[outliers], labels = outliers, pos = 3, col = "black")
invisible(dev.off())

png("horsepower_change_model1.png", width = 500, height = 500)
plot(lm1$coeff[,2],main="First model: Change in horsepower slope")
values <- lm1$coeff[,2]
q <- quantile(values, probs = c(0.02, 0.98))
iqr <- IQR(values)
outliers <- which(values < q[1] - 1.5 * iqr | values > q[2] + 1.5 * iqr)
print(outliers)
text(x = outliers, y = values[outliers], labels = outliers, pos = 3, col = "black")
invisible(dev.off())

png("wheelbase_change_model1.png", width = 500, height = 500)
plot(lm1$coeff[,3],main="First model:Change in wheelbase slope")
values <- lm1$coeff[,3]
q <- quantile(values, probs = c(0.02, 0.98))
iqr <- IQR(values)
outliers <- which(values < q[1] - 1.5 * iqr | values > q[2] + 1.5 * iqr)
print(outliers)
text(x = outliers, y = values[outliers], labels = outliers, pos = 3, col = "black")
invisible(dev.off())

#Mean of set
print(mean(carprices_train$price))

#Dropping the outliers with row index 83,6,71,19,106,
#running regression again and checking assumptions.

carprices_train_nooutlier <- carprices_train[-c(83,6,71,19,106), ]
model3_nooutlier <- lm(price ~ enginesize + horsepower + wheelbase + fueltype + carbody + drivewheel, data = carprices_train_nooutlier)

png("assumptions_model_nooutlier.png", width = 600, height = 600)
par(mfrow = c(2,2))
plot(model3_nooutlier)
invisible(dev.off())

#Backwards-selection (assumptions with prediction vs observed)
###################################################################
model3_nooutlier_step <- step(model3_nooutlier, direction = 'backward')
print(summary(model3_nooutlier_step))

#Assumptions for the backwards-selection (appendix)
#Residuals
png("residuals_backwards.png", width = 500, height = 500)
plot(model3_nooutlier_step, which = 1, main="Backwards model: Residuals")
invisible(dev.off())

#Normality
png("normality_backwards.png", width = 500, height = 500)
plot(model3_nooutlier_step, which = 2, main="Backwards model: Normality assumptions")
invisible(dev.off())

#Cooks
png("cooks_backwards.png", width = 500, height = 500)
plot(model3_nooutlier_step, which = 4, main="Backwards model: Cooks test")
invisible(dev.off())

#Leverage
png("leverage_backwards.png", width = 500, height = 500)
plot(model3_nooutlier_step, which = 5, main="Backwards model: Leverage")
invisible(dev.off())

#Checking if log transformation improves residuals
model3_nooutlier_step_log <- lm(log(price) ~ log(enginesize) +log(horsepower) +log(wheelbase) + fueltype + carbody, data = carprices_train_nooutlier)

png("residuals_backwards_log.png", width = 500, height = 500)
plot(model3_nooutlier_step_log, which = 1, main="Backwards model (log): Residuals")
invisible(dev.off())

#Predicted vs observed using backwards-selection model (w log)
test_pred <- predict(model3_nooutlier_step_log, newdata=carprices_test)
test_pred_unlog <- exp(test_pred)

png("backwardsmodelpredlog.png", height=600,width=600)
plot(test_pred_unlog,carprices_test$price,
     xlab="Predicted", ylab="Observed",
     main="Backward selection: Predicted vs Observed, test data, untransformed log-values.")
abline(0,1, col="red")
invisible(dev.off())

#Forward-analysis (AIC/BIC/adjR2 when "forwardly" increasing complexity)
###################################################################
#Recall: 
#model3_nooutlier <- lm(price ~ enginesize + horsepower + wheelbase + fueltype + carbody + drivewheel  , data = carprices_train_nooutlier)

crit<-data.frame(nbr = c(1,2,3,4,5,6,7),R2adj=c(0,0,0,0,0,0,0), aic=c(0,0,0,0,0,0,0),bic=c(0,0,0,0,0,0,0), pmse=c(0,0,0,0,0,0,0))
rownumber_test<-nrow(carprices_test)

## 1: intercept only:
mod1<-lm(price ~ 1,data=carprices_train_nooutlier)
sum1 <-summary(mod1)
crit$R2adj[1]<-sum1$adj.r.squared # computes the adjusted R^2
crit$aic[1]<-AIC(mod1)  # computes the AIC
crit$bic[1]<-AIC(mod1,k=log(nrow(carprices_train_nooutlier)))  # computes the BIC

## 2: enginesize
mod2<-lm(price ~ enginesize,data=carprices_train_nooutlier)
sum2 <-summary(mod2)
crit$R2adj[2]<-sum2$adj.r.squared
crit$aic[2]<-AIC(mod2)
crit$bic[2]<-AIC(mod2,k=log(nrow(carprices_train_nooutlier)))

## 3: enginesize + hp
mod3<-lm(price ~ enginesize + horsepower,data=carprices_train_nooutlier)
sum3 <-summary(mod3)
crit$R2adj[3]<-sum3$adj.r.squared
crit$aic[3]<-AIC(mod3)
crit$bic[3]<-AIC(mod3,k=log(nrow(carprices_train_nooutlier)))


## 4: enginesize + hp + wheelbase:
mod4<-lm(price ~ enginesize + horsepower + wheelbase,data=carprices_train_nooutlier)
sum4 <-summary(mod4)
crit$R2adj[4]<-sum4$adj.r.squared
crit$aic[4]<-AIC(mod4)
crit$bic[4]<-AIC(mod4,k=log(nrow(carprices_train_nooutlier)))

## 5: enginesize + hp + wheelbase + fueltype:
mod5<-lm(price ~ enginesize + horsepower + wheelbase + fueltype,data=carprices_train_nooutlier)
sum5 <-summary(mod5)
crit$R2adj[5]<-sum5$adj.r.squared
crit$aic[5]<-AIC(mod5)
crit$bic[5]<-AIC(mod5,k=log(nrow(carprices_train_nooutlier)))

## 6: enginesize + hp + wheelbase + fueltype + carbody:
mod6<-lm(price ~ enginesize + horsepower + wheelbase + fueltype +carbody,data=carprices_train_nooutlier)
sum6 <-summary(mod6)
crit$R2adj[6]<-sum6$adj.r.squared
crit$aic[6]<-AIC(mod6)
crit$bic[6]<-AIC(mod6,k=log(nrow(carprices_train_nooutlier)))

#7 enginesize + hp + wheelbase + fueltype+ carbody + drivewheel
mod7<-lm(price ~ enginesize + horsepower + wheelbase + fueltype +carbody+drivewheel,data=carprices_train_nooutlier)
sum7 <-summary(mod7)
crit$R2adj[7]<-sum7$adj.r.squared
crit$aic[7]<-AIC(mod7)
crit$bic[7]<-AIC(mod7,k=log(nrow(carprices_train_nooutlier)))

png("aic_vs_bic.png",height=600,width=600)
plot(crit$nbr,crit$aic,type="b",xlab="Model complexity")
lines(crit$nbr,crit$bic,col="red")
title("AIC (black), BIC (red)")
invisible(dev.off())

png("r2adj.png",height=600, width=600)
plot(crit$nbr,crit$R2adj,type="b",xlab="model number")
invisible(dev.off())

#Subset analysis
###################################################################
library("olsrr")

model3_withoutliers_fulldataset <- lm(price ~ enginesize + horsepower + wheelbase + fueltype + carbody + drivewheel  , data = carprices)

ols <- ols_step_all_possible(model3_withoutliers_fulldataset)
png("all_possible_subsets.png", width = 900, height = 900)
plot(ols)
invisible(dev.off())

#Interesting indices
print(ols$result[1,])
print(ols$result[7,])
print(ols$result[22,])
print(ols$result[23,])
print(ols$result[42,])

# enginesize
m1_osr <- lm(data=carprices, price ~ enginesize)

#enginesize drivewheel
m2_osr <- lm(data=carprices, price ~ enginesize + drivewheel)

#enginesize horsepower drivewheel
m3_osr <- lm(data=carprices, price ~ enginesize + horsepower + drivewheel)

#enginesize horsepower wheelbase 
m4_osr <- lm(data=carprices, price ~ enginesize + horsepower + wheelbase)

#enginesize horsepower wheelbase carbody
m5_osr <- lm(data=carprices, price ~ enginesize + horsepower + wheelbase +fueltype+ carbody)

#F-tests
print(anova(m1_osr, m2_osr))
print(anova(m2_osr, m3_osr))

# Plot observed vs fitted values
fitted_values <- fitted(m2_osr)
png("observed_vs_fitted_m2_osr.png", width = 500, height = 500)
plot(carprices$price, fitted_values,
     main = "Observed vs Fitted Values, m2_osr, entire dataset",
     xlab = "Observed Values",
     ylab = "Fitted Values")

abline(0, 1, col = "red", lwd = 2)
invisible(dev.off())

#Summary
print(summary(m2_osr))
