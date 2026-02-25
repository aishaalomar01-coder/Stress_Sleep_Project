library(stats)
library(graphics)
library(ggplot2)

#Sleep health and lifestyle analysis
#I’m mostly interested in whether sleep duration is linked to stress level.
#The dataset has other variables, but I’m keeping the analysis focused so I can explain it well in the oral exam.

#Loading data from Kaggle Sleep Health and Lifestyle Dataset (https://www.kaggle.com/datasets/uom190346a/sleep-health-and-lifestyle-dataset)
file_path <- "Sleep_health_and_lifestyle_dataset.csv"

if (!file.exists(file_path)) {
  stop("CSV file not found: ", file_path,
       "Put it in the folder of .R/.qmd files or change the file_path.")
}

sleep_kaggle_data <- read.csv(file_path, stringsAsFactors = FALSE)

#Standarising column names
names(sleep_kaggle_data) <- tolower(gsub("[ .]", "_", names(sleep_kaggle_data)))


#check for the data structure
str(sleep_kaggle_data)

#Changing the variables I will use to its appropriate types
sleep_kaggle_data$gender <- as.factor(sleep_kaggle_data$gender)


#Exploring the gender column to see if the results are biased towards one gender
table(sleep_kaggle_data$gender)

hist(
  sleep_kaggle_data$sleep_duration,
  breaks = 10,
  col = "maroon",
  border = "white",
  main = "Sleep Duration Distribution",
  xlab = "Hours of Sleep"
)
#distribution is irregular
#sleep is at a healty level between 6 and 9 hours for all recorded nights

#Data cleaning 
#For Sleep duration, I will only take from 3 to 12 hours of sleep data more or less would be an error 
#for stress levels it only ranges from 1 to 10
validate_range <- function(x, min_val, max_val) {
  bad <- !is.na(x) & (x < min_val | x > max_val)
  x[bad] <- NA
  list(x = x, n_replaced = sum(bad))
}

after_sleep <- validate_range(sleep_kaggle_data$sleep_duration, 3, 12)
sleep_kaggle_data$sleep_duration <- after_sleep$x
message("Sleep_duration values set to NA:", after_sleep$n_replaced)

after_stress <- validate_range(sleep_kaggle_data$stress_level, 1, 10)
sleep_kaggle_data$stress_level <- after_stress$x
message("Stress_level values set to NA:", after_stress$n_replaced)

#no values fell off but just to make sure:

clean_sleep_data <- sleep_kaggle_data[
  complete.cases(sleep_kaggle_data[, c("sleep_duration", "stress_level", "gender", "age", "sleep_disorder")]),
]

nrow(clean_sleep_data)
#Summary and what is observed 

summary_sleep_stats <- data.frame(
  Mean_Sleep = mean(clean_sleep_data$sleep_duration),
  Sd_Sleep   = sd(clean_sleep_data$sleep_duration),
  Mean_Stress = mean(clean_sleep_data$stress_level),
  SD_Stress   = sd(clean_sleep_data$stress_level),
  Mean_Age   = mean(clean_sleep_data$age)
)

#Research question 1: the relationship between sleep duration and stress
pt_col <- adjustcolor("red", alpha.f = 0.35)

plot(
  clean_sleep_data$sleep_duration,
  clean_sleep_data$stress_level,
  pch = 16,
  cex = 0.7,
  col = pt_col,
  xlab = "Sleep Duration (hours)",
  ylab = "Stress level (1–10)",
  main = "Longer Sleep Relates to Lower Stress"
)

abline(lm(stress_level ~ sleep_duration, data = clean_sleep_data), col = "blue", lwd = 2)

#correlation of sleep and stress
cor_sleep_stress <- cor.test(
  clean_sleep_data$sleep_duration,
  clean_sleep_data$stress_level
)

#Research question 2: age, sleep, and stress (how do they interact?)
#How does age relate to sleep and stress?
summary(clean_sleep_data$age)
summary(clean_sleep_data$stress_level)


plot(
  clean_sleep_data$age,
  clean_sleep_data$sleep_duration,
  pch = 16,
  cex = 0.7,
  col = pt_col,
  xlab = "Age (years)",
  ylab = "Sleep duration (hours)",
  main = "Age Has a Positive Link With Sleep Duration"
)

abline(lm(sleep_duration ~ age, data = clean_sleep_data), col= "blue", lwd = 2)
 
plot(
  clean_sleep_data$age,
  clean_sleep_data$stress_level,
  pch = 16,
  cex = 0.7,
  col = pt_col,
  xlab = "Age (years)",
  ylab = "Stress level (1–10)",
  main = "Older Participants Report Lower Stress"
)

abline(lm(stress_level ~ age, data = clean_sleep_data), col= "blue", lwd = 2)


#correlation of age and sleep
cor_age_sleep <- cor.test(
  clean_sleep_data$age,
  clean_sleep_data$sleep_duration
)

#correlation of age and stress
corage_stress <- cor.test(
  clean_sleep_data$age,
  clean_sleep_data$stress_level
)

#Sleep disorder. This is a quick comparison!
#Do people with any reported sleep disorder show higher stress?
#Do people with any reported sleep disorder show less sleeping duration?
#The sleep disorder variable already existed I just created (None vs any disorder) for a simpler comparison because the subcategories have small counts, and I wanted a clearer two-group visualization.

clean_sleep_data$sleep_disorder[is.na(clean_sleep_data$sleep_disorder) | clean_sleep_data$sleep_disorder == ""] <- "None"
clean_sleep_data$sleep_disorder_bin <- factor(
  ifelse(clean_sleep_data$sleep_disorder == "None", "None", "Sleep Disorder"),
  levels = c("None", "Sleep Disorder")
)

#Group sizes
table(clean_sleep_data$sleep_disorder_bin)

# Stress by sleep disorder plot
ggplot(clean_sleep_data, aes(x = sleep_disorder_bin, y = stress_level)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.35) +
  labs(title = "Participants Reporting a Sleep Disorder Tend to Report Higher Stress", y = "Stress Level", x = "")

ggplot(clean_sleep_data, aes(x = sleep_disorder_bin, y = sleep_duration)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.35) +
  labs(title = "Participants Reporting a Sleep Disorder Tend to Report Shorter Sleep",
       y = "Sleep Duration (Hours)", x = "")

#Mean and SD
mean_dis_none <- tapply(
  clean_sleep_data$stress_level,
  clean_sleep_data$sleep_disorder_bin,
  mean,
  na.rm = TRUE
)

sd_dis_none <- tapply(
  clean_sleep_data$stress_level,
  clean_sleep_data$sleep_disorder_bin,
  sd,
  na.rm = TRUE
)

mean_sleep_dis <- tapply(
  clean_sleep_data$sleep_duration,
  clean_sleep_data$sleep_disorder_bin,
  mean,
  na.rm = TRUE
)

sd_sleep_dis <- tapply(
  clean_sleep_data$sleep_duration,
  clean_sleep_data$sleep_disorder_bin,
  sd,
  na.rm = TRUE
)

#T-test: stress level by sleep disorder + sleep duration by sleep disorder
stress_none <- clean_sleep_data$stress_level[clean_sleep_data$sleep_disorder_bin == "None"]
stress_dis  <- clean_sleep_data$stress_level[clean_sleep_data$sleep_disorder_bin == "Sleep Disorder"]
sleep_none <- clean_sleep_data$sleep_duration[clean_sleep_data$sleep_disorder_bin == "None"]
sleep_dis  <- clean_sleep_data$sleep_duration[clean_sleep_data$sleep_disorder_bin == "Sleep Disorder"]

#T-test: disorder group has higher stress 
t_stress <- t.test(stress_dis, stress_none, alternative = "greater")
#T-test: disorder group has less sleep
t_sleep <- t.test(sleep_dis, sleep_none, alternative = "less")
