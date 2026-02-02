library(stats)
library(graphics)

# Sleep health and lifestyle analysis
# I’m mostly interested in whether sleep duration is linked to stress level.
# The dataset has other variables, but I’m keeping the analysis focused so I can explain it well in the oral exam.

# Load data
data <- read.csv(
  "Sleep_health_and_lifestyle_dataset.csv",
  stringsAsFactors = FALSE
)

# Standardising column names so I don’t fight spaces/capital letters later
names(data) <- tolower(gsub("[ .]", "_", names(data)))

# Changing variables to appropriate types
data$gender <- factor(data$gender)
data$sleep_duration <- as.numeric(data$sleep_duration)
data$stress_level <- as.numeric(data$stress_level)
data$age <- as.numeric(data$age)

# Inspecting data structure
str(data)

# Exploratory summaries
table(data$gender)

hist(
  data$sleep_duration,
  breaks = 10,
  col = "lightgray",
  border = "white",
  main = "Sleep duration distribution",
  xlab = "Hours of sleep"
)

# Data cleaning
validate_range <- function(x, min_val, max_val) {
  ifelse(is.na(x) | x < min_val | x > max_val, NA, x)
}

data$sleep_duration <- validate_range(data$sleep_duration, 3, 12)
data$stress_level <- validate_range(data$stress_level, 1, 10)

# Keeping rows that are complete for the variables I use in the main questions

data_clean <- data[
  complete.cases(
    data[, c("sleep_duration", "stress_level", "gender", "age")]
  ),
]

nrow(data_clean)

#Stat Summary

summary_stats <- data.frame(
  Mean_Sleep = mean(data_clean$sleep_duration),
  Sd_Sleep   = sd(data_clean$sleep_duration),
  Mean_Stress = mean(data_clean$stress_level),
  SD_Stress   = sd(data_clean$stress_level),
  Mean_Age   = mean(data_clean$age)
)

summary_stats
# Research question 1: relationship between sleep duration and stress
cor_sleep_stress <- cor.test(
  data_clean$sleep_duration,
  data_clean$stress_level
)

cor_sleep_stress #correlation

plot(
  jitter(data_clean$sleep_duration, 0.1),
  jitter(data_clean$stress_level, 0.2),
  pch = 16,
  col = rgb(0.2, 0.4, 0.8, 0.6),
  xlab = "Sleep duration (hours)",
  ylab = "Stress level (1–10)",
  main = "Sleep duration and stress"
)

model_sleep_stress <- lm(
  stress_level ~ sleep_duration,
  data = data_clean
)
abline(model_sleep_stress, lwd = 2)

# Research question 2 (exploratory): age, sleep, and stress
# How does age relate to sleep and stress?
# Not a deep causal question—just exploring patterns.
summary(data_clean$age)

par(mfrow = c(1, 2))

hist(
  data_clean$age,
  breaks = 20,
  col = "lightgray",
  main = "Age distribution",
  xlab = "Age (years)"
)

hist(
  data_clean$stress_level,
  breaks = 10,
  col = "lightgray",
  main = "Stress level distribution",
  xlab = "Stress level (1–10)"
)

par(mfrow = c(1, 1))

cor_age_sleep <- cor.test(
  data_clean$age,
  data_clean$sleep_duration
)

cor_age_stress <- cor.test(
  data_clean$age,
  data_clean$stress_level
)

cor_age_sleep
cor_age_stress

par(mfrow = c(1, 2))

plot(
  jitter(data_clean$age, 0.5),
  jitter(data_clean$sleep_duration, 0.1),
  pch = 16,
  col = rgb(0.2, 0.5, 0.8, 0.6),
  xlab = "Age (years)",
  ylab = "Sleep duration (hours)",
  main = "Age and sleep duration"
)
abline(lm(sleep_duration ~ age, data = data_clean), lwd = 2)

plot(
  jitter(data_clean$age, 0.5),
  jitter(data_clean$stress_level, 0.2),
  pch = 16,
  col = rgb(0.9, 0.4, 0.3, 0.6),
  xlab = "Age (years)",
  ylab = "Stress level (1–10)",
  main = "Age and stress level"
)
abline(lm(stress_level ~ age, data = data_clean), lwd = 2)

par(mfrow = c(1, 1))


# Sleep disorder (binary)
# This is a quick comparison: do people with any reported sleep disorder show higher stress?
# I’m grouping everything not "None" together so the groups are bigger and easier to compare.
data_clean$sleep_disorder_bin <- ifelse(
  data_clean$sleep_disorder == "None",
  "None",
  "Sleep Disorder"
)
data_clean$sleep_disorder_bin <- as.factor(data_clean$sleep_disorder_bin)

# Group sizes
table(data_clean$sleep_disorder_bin)

# T-test: stress level by sleep disorder
t.test(
  stress_level ~ sleep_disorder_bin,
  data = data_clean
)

# Mean and SD by group
means <- tapply(
  data_clean$stress_level,
  data_clean$sleep_disorder_bin,
  mean,
  na.rm = TRUE
)

sds <- tapply(
  data_clean$stress_level,
  data_clean$sleep_disorder_bin,
  sd,
  na.rm = TRUE
)

# Bar plot with SD
bp <- barplot(
  means,
  ylim = c(0, max(means + sds) + 1),
  col = c("lightblue", "steelblue"),
  ylab = "Stress level",
  main = "Stress level by sleep disorder"
)

arrows(
  bp, means - sds,
  bp, means + sds,
  angle = 90,
  code = 3,
  length = 0.1
)
text(
  x = bp,
  y = means + sds + 0.5,
  # This combines the text "Mean =" with the number
  labels = paste("Mean =", round(means, 2)), 
  cex = 0.8 
)

