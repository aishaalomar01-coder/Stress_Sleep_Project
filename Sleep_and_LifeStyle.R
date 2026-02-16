library(stats)
library(graphics)

#Sleep health and lifestyle analysis
#I’m mostly interested in whether sleep duration is linked to stress level.
#The dataset has other variables, but I’m keeping the analysis focused so I can explain it well in the oral exam.

#Loading data
file_path <- "Sleep_health_and_lifestyle_dataset.csv"

if (!file.exists(file_path)) {
  stop("CSV file not found: ", file_path,
       "Put it in the folder of .R/.qmd files or change the file_path.")
}

data <- read.csv(file_path, stringsAsFactors = FALSE)

#Standardising column names
names(data) <- tolower(gsub("[ .]", "_", names(data)))

# Changing variables to its appropriate types
data$gender <- factor(data$gender)
data$sleep_duration <- as.numeric(data$sleep_duration)
data$stress_level <- as.numeric(data$stress_level)
data$age <- as.numeric(data$age)

#Quick check at data structure
str(data)

#Exploring the gender column
table(data$gender)

hist(
  data_clean$sleep_duration,
  breaks = 10,
  col = "lightgray",
  border = "white",
  main = "Sleep duration distribution",
  xlab = "Hours of sleep"
)

#Data cleaning
validate_range <- function(x, min_val, max_val) {
  ifelse(is.na(x) | x < min_val | x > max_val, NA, x)
}

data$sleep_duration <- validate_range(data$sleep_duration, 3, 12)
data$stress_level <- validate_range(data$stress_level, 1, 10)

#Keeping rows that are complete for the variables I use in the main questions to make it easier

data_clean <- data[
  complete.cases(
    data[, c("sleep_duration", "stress_level", "gender", "age")]
  ),
]

nrow(data_clean)

#Summary and what is observed 

summary_stats <- data.frame(
  Mean_Sleep = mean(data_clean$sleep_duration),
  Sd_Sleep   = sd(data_clean$sleep_duration),
  Mean_Stress = mean(data_clean$stress_level),
  SD_Stress   = sd(data_clean$stress_level),
  Mean_Age   = mean(data_clean$age)
)

summary_stats
#Research question 1: the relationship between sleep duration and stress
cor_sleep_stress <- cor.test(
  data_clean$sleep_duration,
  data_clean$stress_level
)

cor_sleep_stress #correlation

plot(
  data_clean$sleep_duration,
  data_clean$stress_level,
  pch = 16,
  cex = 0.7,
  col = "blue",
  xlab = "Sleep duration (hours)",
  ylab = "Stress level (1–10)",
  main = "Sleep Duration and Stress"
)

abline(lm(stress_level ~ sleep_duration, data = data_clean), lwd = 2)

#Research question 2: age, sleep, and stress (how do they interact?)
#How does age relate to sleep and stress?
#Not a deep question just exploring patterns.
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
  data_clean$age,
  data_clean$sleep_duration,
  pch = 16,
  cex = 0.7,
  col = "blue",
  xlab = "Age (years)",
  ylab = "Sleep duration (hours)",
  main = "Age and Sleep Duration Relationship"
)

abline(lm(sleep_duration ~ age, data = data_clean), lwd = 2)
 

plot(
  data_clean$age,
  data_clean$stress_level,
  pch = 16,
  cex = 0.7,
  col = "red",
  xlab = "Age (years)",
  ylab = "Stress level (1–10)",
  main = "Age and Stress Level Relationship"
)

abline(lm(stress_level ~ age, data = data_clean), lwd = 2)


par(mfrow = c(1, 1))


#Sleep disorder. This is a quick comparison!
#Do people with any reported sleep disorder show higher stress?
#I’m grouping not "None" together so the groups are bigger and easier to compare.

data_clean$sleep_disorder_bin <- ifelse(
  data_clean$sleep_disorder == "None",
  "None",
  "Sleep Disorder"
)
data_clean$sleep_disorder_bin <- as.factor(data_clean$sleep_disorder_bin)

#Group sizes
table(data_clean$sleep_disorder_bin)

#T-test: stress level by sleep disorder
t.test(
  stress_level ~ sleep_disorder_bin,
  data = data_clean
)

#Mean and SD
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

#Bar plot with Means and SD
bp <- barplot(
  means,
  ylim = c(0, max(means + sds) + 1),
  col = c("lightblue", "steelblue"),
  ylab = "Stress level",
  main = "Stress level by Sleep Disorder"
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
  labels = paste("Mean =", round(means, 2)),
  cex = 0.8
)


