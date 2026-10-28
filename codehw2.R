# data 101 - hw 2
# dataset from hw 1: 2019.csv

# loading hypothesis testing package
library(HypothesisTesting)

# reading the data
data2019 <- read.csv("2019.csv")

# creating categorical vars to act as my groups 
data2019$GDP_Level <- ifelse(data2019$GDP.per.capita > 0.9, "HighGDP", "LowGDP")

# a categorical var for high freedom
data2019$HighFreedom <- ifelse(data2019$Freedom.to.make.life.choices > 0.4, "HighFreedom", "LowFreedom")



# q: In countries with HIGH Social Support (> 1.2)
# is the average Happiness Score higher for High GDP countries than Low GDP countries?

# subset 
S_mean <- data2019[data2019$Social.support > 1.2, ]

# observed diff
mean_HighGDP <- mean(S_mean$Score[S_mean$GDP_Level == "HighGDP"], na.rm = TRUE)
mean_LowGDP  <- mean(S_mean$Score[S_mean$GDP_Level == "LowGDP"], na.rm = TRUE)

obs_diff_mean <- mean_HighGDP - mean_LowGDP
cat("Observed Difference in Mean Score: ", obs_diff_mean, "\n")

# perm test (one-tail: mean(HighGDP) > mean(LowGDP))
cat("\n--- Running Permutation Test for Means ---\n")
permutation_test(
  S_mean,
  "GDP_Level",
  "Score",
  10000,
  "HighGDP",
  "LowGDP"
)


# proportion pattern
# q: in countries with HIGH generosity (> 0.2) 
# is the fraction of high freedom countries higher in high GDP than low GDP countries?

# subset
S_prop <- data2019[data2019$Generosity > 0.2, ]

# observed diff
# calculating proportions
prop_HighGDP <- mean(S_prop$HighFreedom[S_prop$GDP_Level == "HighGDP"] == "HighFreedom", na.rm = TRUE)
prop_LowGDP  <- mean(S_prop$HighFreedom[S_prop$GDP_Level == "LowGDP"] == "HighFreedom", na.rm = TRUE)

obs_diff_prop <- prop_HighGDP - prop_LowGDP
cat("\nObserved Difference in Proportions: ", obs_diff_prop, "\n")

# creating numeric indicator 
# TRUE -> 1, FALSE -> 0
S_prop$HighFreedomIndicator <- S_prop$HighFreedom == "HighFreedom"

# perm test 
cat("\n--- Running Permutation Test for Proportions ---\n")
permutation_test(
  S_prop,
  "GDP_Level",
  "HighFreedomIndicator",
  10000,
  "HighGDP",
  "LowGDP"
)

# PART 3: confidence intervals 


# mean score confidence interval
n_overall <- nrow(data2019)
mean_overall <- mean(data2019$Score, na.rm=TRUE)
sd_overall <- sd(data2019$Score, na.rm=TRUE)
moe_overall <- 1.96 * (sd_overall / sqrt(n_overall))
ci_overall_lower <- mean_overall - moe_overall
ci_overall_upper <- mean_overall + moe_overall

# subset mean score confidence interval, social support > 1.2
n_sub <- nrow(S_mean)
mean_sub <- mean(S_mean$Score, na.rm=TRUE)
sd_sub <- sd(S_mean$Score, na.rm=TRUE)
moe_sub <- 1.96 * (sd_sub / sqrt(n_sub))
ci_sub_lower <- mean_sub - moe_sub
ci_sub_upper <- mean_sub + moe_sub

# CI result
cat("\n--- 95% Confidence Intervals ---\n")
cat("Overall Mean Score CI: [", ci_overall_lower, ",", ci_overall_upper, "]\n")
cat("Subset Mean Score CI : [", ci_sub_lower, ",", ci_sub_upper, "]\n")

