
###################################

# Workshop R-Introduction - Lecture

# Date: 13 October 20216

###################################


rm(list = ls())



######################
# Statistical Analysis
######################


# Lecture
# Install and load packages
library(tidyverse)

# Load and inspect data
library(NHANES)
data(NHANES)



# Descriptive statistics
########################

# Categorical

# Tabulation and cross-tabulation
table(NHANES$Education, useNA = "always")
table(NHANES$Education, NHANES$Gender, useNA = "always")

# Proportion
prop.table(table(NHANES$Education))
prop.table(table(NHANES$Education, NHANES$Gender), 2)


# Numeric

# Mean
mean_age <- mean(NHANES$Age, na.rm = TRUE)

# Standard deviation
sd_age <- sd(NHANES$Age, na.rm = TRUE)

# 95% Confidence Interval
n <- length(NHANES$Age[!is.na(NHANES$Age)])
ci_lower <- mean_age - qnorm(0.975) * sd_age / sqrt(n)
ci_upper <- mean_age + qnorm(0.975) * sd_age / sqrt(n)

# Median and quartiles
median_age <- median(NHANES$Age, na.rm = TRUE)
quartiles <- quantile(NHANES$Age, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Print results
cat("Mean:", round(mean_age, 2), "\n")
cat("Standard Deviation:", round(sd_age, 2), "\n")
cat("95% CI:", round(ci_lower, 2), "-", round(ci_upper, 2), "\n")
cat("Median:", quartiles[2], "\n")
cat("1st Quartile:", quartiles[1], "\n")
cat("3rd Quartile:", quartiles[3], "\n")

# With tidyverse
age_summary <- NHANES %>%
  summarise(
    mean_age = mean(Age, na.rm = TRUE),
    sd_age = sd(Age, na.rm = TRUE),
    n = sum(!is.na(Age)),
    ci_lower = mean_age - qnorm(0.975) * sd_age / sqrt(n),
    ci_upper = mean_age + qnorm(0.975) * sd_age / sqrt(n),
    median_age = median(Age, na.rm = TRUE),
    q1 = quantile(Age, 0.25, na.rm = TRUE),
    q3 = quantile(Age, 0.75, na.rm = TRUE)
  )
print(age_summary)


#----------


# Correlation - Pearson and Spearman
####################################

# Pearson
NHANES %>%
  select(Age, BMI) %>%
  drop_na() %>%
  summarise(correlation = cor(Age, BMI, method = "pearson"))

# Spearman
NHANES %>%
  select(Age, BMI) %>%
  drop_na() %>%
  summarise(correlation = cor(Age, BMI, method = "spearman"))

# Histogram of Age
NHANES %>%
  ggplot(aes(x = Age)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Age in NHANES Dataset",
       x = "Age (years)",
       y = "Count") +
  theme_minimal()

# Histogram of BMI
NHANES %>%
  ggplot(aes(x = BMI)) +
  geom_histogram(bins = 30, fill = "green", color = "black") +
  labs(title = "Histogram of BMI in NHANES Dataset",
       x = "BMI (kg/m²)",
       y = "Count") +
  theme_minimal()

# Scatter plot
ggplot(NHANES, aes(x = Age, y = BMI)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Scatter Plot of Age vs BMI in NHANES Dataset",
       x = "Age (years)",
       y = "BMI (kg/m²)") +
  theme_minimal()


#----------


# Common statistical tests
##########################

NHANES <- NHANES[1:200,]

# Numeric

# One-sample t-test (based on average US population age)
t.test(NHANES$Age, mu = 38.5)

# Two-sample t-test
t.test(Age ~ Gender, data = NHANES)

# Paired t-test
mean(NHANES$BPSys1, na.rm = TRUE)
mean(NHANES$BPSys2, na.rm = TRUE)
t.test(NHANES$BPSys1, NHANES$BPSys2, paired = TRUE)

# ANOVA
summary(aov(Age ~ Education, data = NHANES))
NHANES %>%
  group_by(Education) %>%
  summarize(mean_age = mean(Age, na.rm = TRUE))

# Wilcoxon ranksum test
wilcox.test(Age ~ Gender, data = NHANES)

# Wilcoxon signed rank test
wilcox.test(NHANES$BPSys1, NHANES$BPSys2, paired = TRUE)

# Kruskal-Wallis test
kruskal.test(Age ~ Education, data = NHANES)


# Categorical

# Chi-square test
prop.table(table(NHANES$Education, NHANES$Gender), 2)
chisq.test(table(NHANES$Gender, NHANES$Education))

# Fisher's exact test
fisher.test(table(NHANES$Gender[1:50], NHANES$Education[1:50]))

# McNemar test for high blood pressure
bp_high1 <- NHANES$BPSys1 > 140
bp_high2 <- NHANES$BPSys2 > 140
mcnemar.test(bp_high1, bp_high2)


#----------


# Linear model
##############

# Is age associated with BMI?

# Scatter plot
ggplot(NHANES, aes(x = Age, y = BMI)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Scatter Plot of Age vs BMI in NHANES Dataset",
       x = "Age (years)",
       y = "BMI (kg/m²)") +
  theme_minimal()

# Simple linear model
model <- lm(BMI ~ Age, data = NHANES)
summary(model)
confint(model, level = 0.95)

# Normal Q-Q Plot
qqnorm(residuals(model))

# Linear model with quadratic term for whole age range
model <- lm(BMI ~ Age + I(Age^2), data = NHANES)
summary(model)
qqnorm(residuals(model))

# Linear model for age<30
model <- lm(BMI ~ Age, data = NHANES, subset = Age < 30)
summary(model)
qqnorm(residuals(model))

# Adjusted linear model for age<30
model <- lm(BMI ~ Age + Gender + HHIncomeMid, data = NHANES, subset = Age < 30)
summary(model)
qqnorm(residuals(model))


#----------


# Logistic model
################

# Dichotomize BMI
NHANES$HighBMI <- ifelse(NHANES$BMI >= 30, 1, 0)
prop.table(table(NHANES$HighBMI))

# Scatter plot
ggplot(data = NHANES, aes(x = Age, y = HighBMI)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  scale_y_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  labs(x = "Age", y = "Obesity", title = "Scatter Plot of Age vs. Obesity") +
  theme_minimal() + geom_jitter(height = 0.1, width = 0.2)

# Box plot
NHANES$BMI_category <- ifelse(NHANES$BMI >= 30, "High BMI", "Low BMI")
ggplot(NHANES, aes(x = BMI_category, y = Age, fill = BMI_category)) +
  geom_boxplot() +
  labs(title = "Age Distribution by BMI Category",
       x = "BMI Category",
       y = "Age (years)") +
  theme_minimal()

# Logistic model
model <- glm(HighBMI ~ Age, data = NHANES, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))











