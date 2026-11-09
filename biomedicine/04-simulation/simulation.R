# MSA680 lab 4: simulation of a clinical trial, power and Type I/II errors.
# Run from this folder: Rscript simulation.R
# All plots are saved as png files in this folder.

library(tidyverse)
library(pwr)
library(foreach)
library(survival)
library(survminer)
library(tibble)
library(dplyr)

set.seed(230118001)

# Simulate time to death

simulate_TTD <- function(n_patients, death_rate, follow_up) {
  #lambda solved from cdf of exp distribution,
  lambda <- -log(1 - death_rate)
  true_TTD_years <- rexp(n_patients, rate = lambda)

  #years to days
  true_TTD_days <- true_TTD_years * 365
  follow_up_days <- follow_up * 365

  alive <- true_TTD_days > follow_up_days
  #any fraction of the day counts as survival, take ceiling
  true_TTD_days_rounded <- ceiling(true_TTD_days)

  censored_TTD_days <- ifelse(true_TTD_days <= follow_up_days,
                              true_TTD_days_rounded,
                              follow_up_days + 1)

  #Create a data frames
  data_true <- tibble(
    time = true_TTD_days_rounded,
    status = rep(1, n_patients), 
    type = "True TTD"
  )

  data_censored <- tibble(
    time = censored_TTD_days,
    status = as.integer(!alive), 
    type = "Censored TTD"
  )

  #Combine the data into a single data frame
  combined_data <- bind_rows(data_true, data_censored)
}

sim_result <- simulate_TTD(n_patients = 175, death_rate = 0.3, follow_up = 1)

sim_result$time_years <- sim_result$time / 365

km_fit <- survfit(Surv(time_years, status) ~ type, data = sim_result)

# Plot using survminer
e_s_prob <- ggsurvplot(km_fit,
           data = sim_result,
           palette = c("blue", "red"),
           linetype = c("solid", "dashed"),
           xlab = "Time (years)",
           ylab = "Survival Probability",
           legend.title = "Data Type",
           legend.labs = c("Censored TTD", "True TTD"),
           break.x.by = 1)  # Adjust x-axis breaks for years

ggsave("estimated_survival_prob_func.png", plot = e_s_prob$plot, width = 5, height = 4, dpi = 300)

# Simulate a full study



simulate_study <- function(
  n.active = 175,
  n.control = 175,
  mu.baseline = 200,
  sd.baseline = 70,
  mu.cfb.control = 0,
  mu.cfb.active = 25,
  sd.cfb = 70,
  annual.rate.death.control = 0.3,
  death.rrr.active = 0.2,
  followup = 1,
  measurement.time.cfb = followup
) {
  annual.rate.death.active <- annual.rate.death.control * (1 - death.rrr.active)

  ttd_data.active <- simulate_TTD(n.active, annual.rate.death.active, followup)
  ttd_data.control <- simulate_TTD(n.control, annual.rate.death.control, followup)

  baseline.active <- floor(rnorm(n.active, mean = mu.baseline, sd = sd.baseline))
  baseline.control <- floor(rnorm(n.control, mean = mu.baseline, sd = sd.baseline))

  ttd.active <- ttd_data.active$time[ttd_data.active$type == "True TTD"]
  ttd.control <- ttd_data.control$time[ttd_data.control$type == "True TTD"]

  died.active <- ifelse(ttd.active <= (followup*365), 1, 0)
  died.control <- ifelse(ttd.control <= (followup*365), 1, 0)

  survivors.active <- which(died.active == 0)
  survivors.control <- which(died.control == 0)

  cfb.active <- rep(NA, n.active)
  cfb.control <- rep(NA, n.control)

  cfb.active[survivors.active] <- rnorm(length(survivors.active), mean = mu.cfb.active, sd = sd.cfb)
  cfb.control[survivors.control] <- rnorm(length(survivors.control), mean = mu.cfb.control, sd = sd.cfb)
  
  data.active <- tibble(
    group = rep("active", n.active),
    ttd = ttd.active,
    alive = died.active == 0,
    baseline = baseline.active,
    cfb = cfb.active
  )

  data.control <- tibble(
    group = rep("control", n.control),
    ttd = ttd.control,
    alive = died.control == 0,
    baseline = baseline.control,
    cfb = cfb.control
  )

  study_data <- bind_rows(data.active, data.control)

  return(study_data)
}


study_data <- simulate_study()

survivors_data <- study_data[study_data$alive, ]
cfb_plot <- ggplot(survivors_data, aes(x = group, y = cfb, fill = group)) +
  geom_boxplot() +
  labs(title = "Distribution of 6MWD (Survivors)", x = "Arm", y = "6MWD 1 year CFB (meters)") +
  theme_minimal()

ggsave("cfb_boxplot.png", plot = cfb_plot, width = 5, height = 4, dpi = 300)


annual.rate.death.control <- 0.3
death.rrr.active <- 0.2
n.active <- 175
n.control <- 175
followup <- 1

annual.rate.death.active <- annual.rate.death.control * (1 - death.rrr.active)

simulate_TTD <- function(n, annual_rate, followup) {
  lambda <- -log(1 - annual_rate)
  rexp(n, rate = lambda)
}
annual.rate.death.active <- annual.rate.death.control * (1 - death.rrr.active)

ttd.active <- simulate_TTD(n.active, annual.rate.death.active, followup)
ttd.control <- simulate_TTD(n.control, annual.rate.death.control, followup)

ttd_data <- data.frame(
  group = rep(c("control", "active"), each = n.active, times = 2),
  ttd = c(ttd.active, ttd.control),
  died = c(ifelse(ttd.active <= followup, 1, 0), ifelse(ttd.control <= followup, 1, 0))
)

surv_obj <- Surv(time = ttd_data$ttd, event = ttd_data$died)

surv_fit <- survfit(surv_obj ~ group, data = ttd_data)

survival_plot <- ggsurvplot(surv_fit,
                           data = ttd_data,
                           palette = c("blue", "red"),
                           xlab = "Time (years)",
                           ylab = "Survival Probability",
                           legend.title = "Group",
                           legend.labs = c("Control", "Active"),
                           xlim = c(0, followup),
                           break.x.by = 1)

ggsave("survival_plot.png", plot = survival_plot$plot, width = 5, height = 4, dpi = 300)

# ANOVA on change from baseline

anova_model <- aov(cfb ~ group, data = survivors_data)

summary(anova_model)

#control: 0.3 
p1 <- 0.3
#active: 0.3*(1-0.2)=0.24
p2 <- 0.24
#cohen effect size 
h <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
power_result <- pwr.2p2n.test(h =h, n1 = 175, n2 = 175,power=NULL, sig.level = 0.05)

print(power_result)

# Power of a z test

z.test.pvalue <- function(n, effect) {
  x <- mean(rnorm(n=n, mean=effect, sd=1))
  2 * pnorm(abs(x), mean=0, sd=1/sqrt(n), lower.tail=FALSE)
}

simulate_z.test.pvalue <- function(N.sim = 1e3, n, effect) {
  pvalue <- replicate(N.sim, z.test.pvalue(n, effect))
  tibble(n = n, effect = effect, pvalue = pvalue)
}

calculate_power <- function(data, alpha = 0.05) {
  power_results <- data %>%
    group_by(n, effect) %>%
    summarize(
      power = mean(pvalue < alpha, na.rm = TRUE)
    )
  return(power_results)
}

data <- bind_rows(
  simulate_z.test.pvalue(N.sim = 1000, n = 8, effect = 0),
  simulate_z.test.pvalue(N.sim = 1000, n = 8, effect = 0.5),
  simulate_z.test.pvalue(N.sim = 1000, n = 24, effect = 0),
  simulate_z.test.pvalue(N.sim = 1000, n = 24, effect = 0.5)
)

pvalue_hist <- ggplot(data |> mutate(effect = factor(effect)), aes(x = pvalue)) +
  facet_grid(effect ~ n,
             labeller = labeller(n = function(x) paste0("n=", x),
                                 effect = function(x) paste0("effect=", x)),
             scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  geom_histogram(aes(y = after_stat(width * density)), breaks = seq(0, 1, by = 0.05)) +
  labs(y = "Proportion of p-values in bin\n(bin width=0.05)", x = "p-value") +
  ggtitle("Histogram of p-values for a two-sided z-test")
ggsave("pvalue_histogram.png", plot = pvalue_hist, width = 6, height = 5, dpi = 300)

power_results <- calculate_power(data)

print(power_results)

alpha <- 0.05
z_critical <- qnorm(1 - alpha/2) 

calculate_power_analytical <- function(n, effect, alpha = 0.05) {
  z_critical <- qnorm(1 - alpha/2)
  mean_z <- effect * sqrt(n)
  power <- 1 - pnorm(z_critical, mean = mean_z, sd = 1) + pnorm(-z_critical, mean = mean_z, sd = 1)
  return(power)
}

analytical_power <- data.frame(
  n = c(8, 8, 24, 24),
  effect = c(0, 0.5, 0, 0.5),
  power = c(
    alpha, 
    calculate_power_analytical(8, 0.5, alpha),
    alpha,
    calculate_power_analytical(24, 0.5, alpha)
  )
)

print(analytical_power)


calculate_ci <- function(p_hat, n_sim = 1000) {
  se <- sqrt(p_hat * (1 - p_hat) / n_sim)
  me <- 1.96 * se
  lower <- max(0, p_hat - me)  
  upper <- min(1, p_hat + me)  
  return(data.frame(lower = lower, upper = upper))
}

type1_error_8 <- 0.051 
type1_error_24 <- 0.051 
power_8 <- 0.288        
power_24 <- 0.68        

ci_type1_error_8 <- calculate_ci(type1_error_8)
ci_type1_error_24 <- calculate_ci(type1_error_24)
ci_power_8 <- calculate_ci(power_8)
ci_power_24 <- calculate_ci(power_24)

print("95% CI for Type I error (n=8):")
print(ci_type1_error_8)
print("95% CI for Type I error (n=24):")
print(ci_type1_error_24)
print("95% CI for Power (n=8):")
print(ci_power_8)
print("95% CI for Power (n=24):")
print(ci_power_24)

# Rank analysis of deaths and survivors

rank_prognosis <- function(data) {
  dead_patients <- data[!is.na(data$ttd), ]
  dead_ranks <- rank(dead_patients$ttd, ties.method = "average")

  surviving_patients <- data[is.na(data$ttd), ]
  surviving_ranks <- rank(surviving_patients$cfb, ties.method = "average")


  ranks <- numeric(nrow(data))
  ranks[!is.na(data$ttd)] <- dead_ranks
  ranks[is.na(data$ttd)] <- surviving_ranks + length(dead_ranks)

  ranked_data <- data
  ranked_data$rank <- ranks

  return(ranked_data)
}

ranked_data <- rank_prognosis(study_data)

print(ranked_data)

data_for_ranking <- data.frame(
  group = study_data$group,
  ttd = ifelse(study_data$alive == 1, study_data$ttd, NA),
  cfb = ifelse(study_data$alive == 1, NA, rnorm(sum(study_data$alive == 0), mean = 200, sd = 70)) #baseline values
)

ranked_data <- rank_prognosis(data_for_ranking)

print(ranked_data[order(ranked_data$rank), ])

anova_model <- aov(rank ~ group, data = ranked_data)
    
summary(anova_model)

type_i_error <- 0
num_simulations <- 1000

for (i in 1:num_simulations) {
  df_sim <- study_data
  df_sim$group <- sample(c("control", "active"), size = nrow(df_sim), replace = TRUE)
  ranked_sim <- rank_prognosis(df_sim)
  anova_sim <- aov(rank ~ group, data = ranked_sim)
  if (summary(anova_sim)[[1]]$`Pr(>F)`[1] < 0.05) {
    type_i_error <- type_i_error + 1
  }
}

type_i_error_rate <- type_i_error / num_simulations
print(paste("Type I error rate:", type_i_error_rate))

n_sims <- 1000

alpha <- 0.05

effect_size <- 10  # for example, the active arm has ranks that are 10 units lower on average

sample_sizes <- c(20, 50, 100,200,500)

power_values <- numeric(length(sample_sizes))

for (j in 1:length(sample_sizes)) {
  n_patients <- sample_sizes[j]
  p_values <- numeric(n_sims)

  for (i in 1:n_sims) {
    control_rank <- rnorm(n_patients, mean = 200, sd = 70)
    active_rank <- rnorm(n_patients, mean = 200 - effect_size, sd = 70)

    data <- data.frame(
      group = rep(c("control", "active"), each = n_patients),
      rank = c(control_rank, active_rank)
    )

    model <- aov(rank ~ group, data = data)
    p_values[i] <- summary(model)[[1]]$`Pr(>F)`[1]
  }

  power_values[j] <- mean(p_values < alpha)
}

print(power_values)


# Power for different death rates, risk reductions and effects


#minor tweaks to fn
simulate_TTD <- function(n_patients, death_rate, follow_up) {
  ttd <- rexp(n_patients, rate = death_rate)
  type <- rep("True TTD", n_patients)
  ttd_data <- data.frame(time = ttd, type = type)
  return(ttd_data)
}

rank_prognosis <- function(data) {
  data$rank <- rank(data$ttd, na.last = "keep")
  data$rank[data$alive == 1] <- rank(data$cfb[data$alive == 1], na.last = "keep") + max(data$rank, na.rm = TRUE)
  return(data)
}

n.sims <- 500

calculate_power_varying_death_rate <- function(death_rates, n.sim = n.sims, n.active = 175, n.control = 175) {
  power_values <- c()
  for (rate in death_rates) {
    power <- 0
    for (i in 1:n.sim) {
      study_data <- simulate_study(
        n.active = n.active,
        n.control = n.control,
        annual.rate.death.control = rate,
        death.rrr.active = 0.2,
        mu.cfb.active = 25
      )
      ranked_data <- rank_prognosis(study_data)
      anova_result <- summary(aov(rank ~ group, data = ranked_data))
      p_value <- anova_result[[1]]$`Pr(>F)`[1]
      if (p_value < 0.05) {
        power <- power + 1
      }
    }
    power_rate <- power / n.sim
    power_values <- c(power_values, power_rate)
  }
  return(power_values)
}

calculate_power_varying_rrr <- function(rrrs, n.sim = n.sims, n.active = 175, n.control = 175) {
  power_values <- c()
  for (rrr in rrrs) {
    power <- 0
    for (i in 1:n.sim) {
      study_data <- simulate_study(
        n.active = n.active,
        n.control = n.control,
        annual.rate.death.control = 0.3,
        death.rrr.active = rrr,
        mu.cfb.active = 25
      )
      ranked_data <- rank_prognosis(study_data)
      anova_result <- summary(aov(rank ~ group, data = ranked_data))
      p_value <- anova_result[[1]]$`Pr(>F)`[1]
      if (p_value < 0.05) {
        power <- power + 1
      }
    }
    power_rate <- power / n.sim
    power_values <- c(power_values, power_rate)
  }
  return(power_values)
}

calculate_power_varying_cfb <- function(cfbs, n.sim = n.sims, n.active = 175, n.control = 175) {
  power_values <- c()
  for (cfb in cfbs) {
    power <- 0
    for (i in 1:n.sim) {
      study_data <- simulate_study(
        n.active = n.active,
        n.control = n.control,
        annual.rate.death.control = 0.3,
        death.rrr.active = 0.2,
        mu.cfb.active = cfb
      )
      ranked_data <- rank_prognosis(study_data)
      anova_result <- summary(aov(rank ~ group, data = ranked_data))
      p_value <- anova_result[[1]]$`Pr(>F)`[1]
      if (p_value < 0.05) {
        power <- power + 1
      }
    }
    power_rate <- power / n.sim
    power_values <- c(power_values, power_rate)
  }
  return(power_values)
}

death_rates <- seq(0.1, 0.5, by = 0.05)
power_death_rates <- calculate_power_varying_death_rate(death_rates)

rrrs <- seq(0.01, 0.4, by = 0.05)
power_rrrs <- calculate_power_varying_rrr(rrrs)

cfbs <- seq(10, 30, by = 5)
power_cfbs <- calculate_power_varying_cfb(cfbs)

png("power_vs_death_rate.png", width = 500, height = 500)
plot(death_rates, power_death_rates, type = "b", xlab = "Baseline Annual Death Rate", ylab = "Power", main = "Power vs Baseline Annual Death Rate")
invisible(dev.off())

png("power_vs_rrr.png", width = 500, height = 500)
plot(rrrs, power_rrrs, type = "b", xlab = "Drug's RRR", ylab = "Power", main = "Power vs Drug's RRR")
invisible(dev.off())

png("power_vs_cfb.png", width = 500, height = 500)
plot(cfbs, power_cfbs, type = "b", xlab = "Expected 6MWD CFB", ylab = "Power", main = "Power vs Expected 6MWD CFB")
invisible(dev.off())
