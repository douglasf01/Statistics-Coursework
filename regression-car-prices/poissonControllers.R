# Part 2 of the MSG501 project: Poisson regression of air traffic controller
# errors. This is a separate exercise from the car price analysis.
#
# It needs the file controllers.dat (columns: experience, age, errors, size),
# which was provided by the course and is not included in this repository.
# Put controllers.dat in this folder to run the script.

if (!file.exists("controllers.dat")) {
  message("controllers.dat not found in this folder, so the Poisson part is skipped.")
  quit(save = "no", status = 0)
}


#Importing and preprocess
library("AER")
df <- read.table('controllers.dat')
colnames(df) <- c("experience", "age", "errors", "size")

df$experience <- factor(df$experience, levels=1:6)
df$experience <- relevel(df$experience, ref=5)

df$age <- factor(df$age, levels=1:2)
df$age <- relevel(df$age, ref=1)  # baseline = <=55 years

# Compute error rate per ATC for plotting
df$rate <- df$errors / df$size
df$rate


# Fit a Poisson model for the number of errors, adjusting for group size
glm_model <- glm(errors ~ experience + age,
                 family = poisson(link = "log"),
                 offset = log(size),
                 data = df, x=TRUE)

#Dispesion test
dispersiontest(glm_model, alternative = "two.sided")
summary(glm_model)

#Predicting from the model
design <- glm_model$x
beta <-glm_model$coefficients
rate=exp(design%*%beta)

df$pred_rate = predict(glm_model,type="response")/df$size
png("poissonPredvsError.png", width = 500, height = 500)
plot(df$pred_rate, df$rate,
     xlab = "Predicted error rate",
     ylab = "Observed error rate",
     main = "Observed vs Predicted Error Rates",
     pch = 19, col = "blue")
abline(0, 1, col = "red", lty = 2)
invisible(dev.off())


#Cooks distance
d = cooks.distance(glm_model)
png("poissonCooks.png",height=500, width=500)
plot(d,ylim=c(0,1.2),xlab="Index",ylab="Distance",main="Poisson model, Cook's distance",pch=16 )
invisible(dev.off())


#Standardized Pearsson residuals
infl<-influence(glm_model)

# Pearsons residuals
r_pear <- infl$pear.res

# Standardized Pearson residuals:
r_std<-r_pear/sqrt(1-infl$hat)
png("poissonStandardizedResiduals.png", width = 720, height = 300)
par(mfrow = c(1, 2), mai = c(0.6, 0.6, 0.4, 0.2), oma = c(0, 0, 2.5, 0))
plot(d,ylim=c(0,1.2),xlab="Index",ylab="Distance",main="Cook's distance",pch=16 )
title("Poisson model", outer = TRUE, cex.main = 1.3)

plot(r_std,xlab="Index",ylab="r_i",ylim=c(-4,4),main="Standardized Pearson residuals (W Bonferroni)", pch=16)
n = length(df$size)
alpha = 0.05
alpha_correction = alpha/n
z<-qnorm(alpha_correction/2)
abline(h=0)
abline(h=-z,col="red")
abline(h=z,col="red")
invisible(dev.off())

