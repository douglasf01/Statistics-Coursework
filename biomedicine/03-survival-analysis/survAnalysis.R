# MSA680 lab 3: survival analysis of the WHAS500 data.
# Run from this folder: Rscript survAnalysis.R
# All plots are saved as png files in this folder.

library(tidyverse)
library(GGally)
library(survival)
library(survminer)

# Save a ggsurvplot (curve plus risk table) to a png file
save_survplot <- function(plot, file, width = 7, height = 7) {
  png(file, width = width, height = height, units = "in", res = 150)
  print(plot)
  invisible(dev.off())
}

data <- read.csv2("whas500.csv")
print(head(data))

# Descriptive plots
# Plot the people who died during the study
data_died <- subset(data, FSTAT == 1)

mean_lenfol <- mean(data_died$LENFOL)
rate <- 1 / mean_lenfol

hist <- ggplot(data_died, aes(x=LENFOL)) +
        geom_histogram(aes(y=after_stat(density), alpha=0.6), bins=15) +
        geom_density(color="blue") +
        stat_function(fun = dexp, args = list(rate = rate), color = "red", linewidth = 1) +
        theme(legend.position = "none")


ggsave("histogram.png", plot = hist, device = "png", width = 5, height = 5, dpi = 300)


#calculating the cdf of the people who died
library(ggplot2)

ecdf_lenfol <- ecdf(data_died$LENFOL)
prob_at_200 <- ecdf_lenfol(200)

cdf_lenfol <- ggplot(data_died, aes(x = LENFOL)) +
  stat_ecdf(geom = "step", color = "black", linewidth = 1) +
  geom_point(aes(x = 200, y = prob_at_200), color = "red", size = 2) +
  labs(title = "Empirical cdf of LENFOL on subjects that died", x = "LENFOL", y = "ECDF")

ggsave("cdf_lenfol.png", plot = cdf_lenfol, device = "png", width = 5, height = 5, dpi = 300)

print(prob_at_200)

#Calculate survival probabilities
survival_probs <- 1 - ecdf_lenfol(data_died$LENFOL)

survival_at_1000 <- 1-ecdf_lenfol(1000)

print(survival_at_1000)

survival_data <- data.frame(
  LENFOL = data_died$LENFOL,
  Survival = survival_probs
)

#Plot the survival function
survival_lenfol <- ggplot(survival_data, aes(x = LENFOL, y = Survival)) +
  geom_step(color = "black", linewidth = 1) +
  geom_point(aes(x = 1000, y = survival_at_1000), color = "red", size = 2) +
  labs(title = "Survival Function of LENFOL on subjects that died", x = "LENFOL", y = "Survival Probability") +
  scale_y_reverse(limits = c(1, 0))

ggsave("survival_lenfol.png", plot = survival_lenfol, device = "png", width = 5, height = 5, dpi = 300)

# Covariates
# No missing values
missing_values <- is.na(data)
print(sum(missing_values))

#bmi is chr encoded, switch to float
data <- data %>%
  mutate(BMI = as.numeric(BMI))

summary_stats <- data %>%
  summarize(across(c(LENFOL,AGE,BMI,HR), list(mean = mean, sd = sd, min=min,max=max)))

print(summary_stats)

pairsplot <- ggpairs(data, columns = c("LENFOL", "GENDER", "AGE", "BMI", "HR"), mapping = aes(color = as.factor(FSTAT)))
ggsave("pairs_plot.png", plot = pairsplot, width = 9, height = 9, units = "in")

# Kaplan-Meier estimate
surv.data <- with(data, Surv(LENFOL,FSTAT))
fit <- survfit ( surv.data~1 , data = data, conf.type="log" )
print(fit)
print(summary(fit))

surv.data <- with(data, Surv(LENFOL,FSTAT))

fit <- survfit ( surv.data~1 , data = data, conf.type="log-log" )
print(fit)
print(summary(fit))

non_param_surv <- ggsurvplot(fit,data=data,
risk.table = TRUE ,
surv.median.line = "hv",
ggtheme = theme_bw())
save_survplot(non_param_surv, "km_overall.png")

non_param_chazard <- ggsurvplot (fit,data=data,
fun = "cumhaz",
risk.table = TRUE ,
ggtheme = theme_bw())
save_survplot(non_param_chazard, "cumhaz_overall.png")

fit <- survfit(surv.data~GENDER , data = data, conf.type="log-log")
surv.test <- survdiff (surv.data~GENDER , data = data )
print((fit))
print(summary(fit))
print(surv.test)


km_gender <- ggsurvplot (fit,data=data,
risk.table = TRUE ,
surv.median.line = "hv",
ggtheme = theme_bw(),
conf.int = TRUE)
save_survplot(km_gender, "km_by_gender.png")

cumhaz_gender <- ggsurvplot (fit,data=data,
fun = "cumhaz",
risk.table = TRUE ,
ggtheme = theme_bw(),
conf.int=TRUE)
save_survplot(cumhaz_gender, "cumhaz_by_gender.png")

# Partial likelihood for a small example

likelihood_fn <- function(Beta) {
    value <- Beta - log( 3*exp(Beta) + 3) - log(3*exp(Beta) + 1) - log(2*exp(Beta) + 1)
    return(value)
}

x_val <- seq(from=-4,to=1,length.out = 100)
y_val <- sapply(x_val, likelihood_fn)
png("likelihood.png", width = 500, height = 500)
plot(x_val,y_val, type='l',main="Likelihood function", xlab="Beta")
invisible(dev.off())

result <- optim (par =0 , fn = likelihood_fn , method ="L-BFGS-B",
control = list(fnscale = -1) ,
lower = -3 , upper =1)

print(result)

df <- data.frame(
  Patient = c(1, 2, 3, 4, 5, 6),
  Survtime = c(6, 7, 10, 15, 19, 25),
  Censor = c(1, 0, 1, 1, 0, 1),
  Group = c("C", "C", "T", "C", "T", "T")
)

survival_data <- Surv(time = df$Survtime, event = df$Censor)
model <- coxph(survival_data ~ Group, data = df)

print(summary(model))

fit <- coxph(surv.data~GENDER + AGE , data = data )
print(summary(fit))

fit <- coxph(surv.data~GENDER + AGE , data = data )
surv.fit <- survfit( fit , data = data )

cox_surv <- ggsurvplot ( surv.fit , data=data,
risk.table = TRUE ,
surv.median.line = "hv",
ggtheme = theme_bw ())
save_survplot(cox_surv, "cox_survival.png")

sex.dt <- with ( data ,
# GENDER is coded 0 = male, 1 = female in WHAS500
data.frame( GENDER = c(0, 1) ,
AGE = rep ( mean ( AGE , na.rm = TRUE ) , 2)))
surv.fit.gender <- survfit ( fit , data = data , newdata = sex.dt )

cox_gender <- ggsurvplot ( surv.fit.gender, data=data, conf.int = TRUE ,
legend.labs =c("Male", "Female") , ggtheme = theme_minimal ())
save_survplot(cox_gender, "cox_survival_by_gender.png", height = 5)

#basline
print(AIC(fit))

fit_1 <- coxph(surv.data~GENDER + AGE + BMI , data = data )
print(AIC(fit_1))

fit_2 <- coxph(surv.data~ AGE + BMI , data = data )
print(AIC(fit_2))

fit_3 <- coxph(surv.data~ BMI + HR + AGE + GENDER, data = data )
print(AIC(fit_3))

fit_4 <- coxph(surv.data~ BMI + HR + AGE, data = data )
print(AIC(fit_4))

fit_5 <- coxph(surv.data~ BMI + HR + AGE + CHF , data = data )
print(AIC(fit_5))
print(summary(fit_5))
