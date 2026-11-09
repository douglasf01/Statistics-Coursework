# MSA680 lab 1: ANCOVA models and least squares means.
# Run from this folder: Rscript ancovaR.R

library(tidyverse)
library(scales)
library(emmeans)
library(car)

############ Data and boxplots ############
diet <- read.table(file="diet.txt",header=T,sep=" ")
#Factor for later
diet$Diet <- factor(diet$Diet)

mean_table <- aggregate(. ~ Diet, data = diet, mean)
mean_table 


p1 <- ggplot(diet, aes(x = factor(Diet), y = PreChol, fill = factor(Diet))) +
  geom_boxplot() +
  labs(title = "Boxplot of PreChol by Diet", x = "Diet", y = "PreChol") +
  theme_minimal()

ggsave("boxplot_pre.png", plot = p1, device = "png", width = 4, height = 4, dpi = 200)

p2 <- ggplot(diet, aes(x = factor(Diet), y = PostChol, fill = factor(Diet))) +
  geom_boxplot() +
  labs(title = "Boxplot of PostChol by Diet", x = "Diet", y = "PostChol") +
  theme_minimal()
ggsave("boxplot_post.png", plot = p2, device = "png", width = 4, height = 4, dpi = 200)


############ One-way ANOVA ############

# ANOVA with aov
aov_diet <- aov(PostChol ~ Diet, data = diet)
summary(aov_diet)


# Same model with lm
lm_diet <- lm (PostChol ~ factor(Diet), data=diet)
anova_result <- anova(lm_diet)

print(summary(lm_diet))
print(anova_result)

# Least squares means
lsmeans <- emmeans(aov_diet, ~ Diet, data=diet)
print(summary(lsmeans))

############ ANCOVA with equal slopes ############

# ANCOVA with aov
exercise_1_aov <- aov(PostChol ~ PreChol + Diet, data = diet)
print(summary(exercise_1_aov))

# ANCOVA with lm, plot and least squares means
exercise_2_lm <- lm(PostChol ~ PreChol + Diet, data = diet)
exercise_2_anova <- anova(exercise_2_lm)

print(exercise_2_anova)
print(summary(exercise_2_lm))

exercise_2_pred <- expand.grid(PreChol = seq(min(diet$PreChol), max(diet$PreChol), length.out = 100), Diet = levels(diet$Diet))
exercise_2_pred$PostChol <- predict(exercise_2_lm, newdata = exercise_2_pred)

p3 <- ggplot(diet, aes(x = PreChol, y = PostChol, color = Diet)) +
  geom_point() +
  geom_line(data = exercise_2_pred, aes(x = PreChol, y = PostChol, color = Diet)) +
  facet_wrap(~ Diet) +
  labs(title = "Scatter Plot with Equal Slopes Regression Lines",
       x = "Pre-diet Cholesterol",
       y = "Post-diet Cholesterol")
  ggsave("ancova_equal_slopes.png", plot = p3, device = "png", width = 5, height = 5, dpi = 200)


exercise_2_lsmeans <- emmeans(exercise_2_lm, specs = ~  Diet)
print((exercise_2_lsmeans))


# Least squares means by PreChol
exercise_2_lsmeans_diet <- emmeans(exercise_2_lm, specs = ~ PreChol | Diet)
print((exercise_2_lsmeans_diet))


############ ANCOVA with unequal slopes ############

# Interaction model with aov
exercise_1_aov <- aov(PostChol ~ PreChol*Diet, data = diet)
print(summary(exercise_1_aov))

# Interaction model with lm, plot and least squares means
exercise_2_lm <- lm(PostChol ~ PreChol * Diet, data = diet)
exercise_2_anova <- anova(exercise_2_lm)

print(exercise_2_anova)
print(summary(exercise_2_lm))

exercise_2_pred <- expand.grid(PreChol = seq(min(diet$PreChol), max(diet$PreChol), length.out = 100), Diet = levels(diet$Diet))
exercise_2_pred$PostChol <- predict(exercise_2_lm, newdata = exercise_2_pred)

p4 <- ggplot(diet, aes(x = PreChol, y = PostChol, color = Diet)) +
  geom_point() +
  geom_line(data = exercise_2_pred, aes(x = PreChol, y = PostChol, color = Diet)) +
  facet_wrap(~ Diet) +
  labs(title = "Scatter Plot with Unequal Slopes Regression Lines",
       x = "Pre-diet Cholesterol",
       y = "Post-diet Cholesterol")
  ggsave("ancova_unequal_slopes.png", plot = p4, device = "png", width = 5, height = 5, dpi = 200)

exercise_2_lsmeans <- emmeans(exercise_2_lm, specs = ~ Diet)
print((exercise_2_lsmeans))

# Least squares means by PreChol
exercise_3_lsmeans <- emmeans(exercise_2_lm,  ~ Diet | PreChol)
print(exercise_3_lsmeans)

# Least squares means are taken at the mean of PreChol

# Center PreChol
diet$mPreChol = diet$PreChol - mean(diet$PreChol)

exercise_4_lm <- lm(PostChol ~ mPreChol * Diet, data = diet)
exercise_4_anova <- anova(exercise_2_lm)

exercise_4_lsmeans <- emmeans(exercise_4_lm, specs = ~ mPreChol | Diet)

print(summary(exercise_4_lsmeans))
print(summary(exercise_4_lm))

# Predictions at PreChol 190 and 250
model <- lm(PostChol ~ PreChol * Diet, data = diet)

#create a new df 
new_data <- data.frame(
  PreChol = rep(c(190, 250), each = length(levels(diet$Diet))),
  Diet = rep(levels(diet$Diet), times = 2)
)

#Make predictions using the model
predictions <- predict(model, newdata = new_data, interval = "confidence")

#Print the predictions and confidence intervals
print(predictions)

#Verify
lsmeans <- emmeans(model, specs = ~ Diet | PreChol, at = list(PreChol = c(190, 250)))

print(lsmeans)
print(model)
