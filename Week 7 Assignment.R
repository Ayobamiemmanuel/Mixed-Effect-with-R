library(lme4)
library('tidyverse')
library('broom')
library(ggplot2)
library(Matrix)
library(dplyr)

install.packages("xtable")
library(xtable)

#install.packages("reprex")
#install.packages("pak")
#pak::pak("tidyverse/reprex")

#1. Data Exploration
#a. Loading the data from R library
data("sleepstudy")

#Viewing the top 6 rows of the data to know what it is made of
head(sleepstudy)

#b. Exploring the structure of the dataset
str(sleepstudy)


#c. Visualization 
# Scatter plot
sleepstudy %>%
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point() +
  labs(title = "Reaction Time Over Days",
       x = "Days of Sleep Deprivation", y = "Reaction Time")

# Boxplot
sleepstudy %>%
  ggplot(aes(y = Reaction)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of Reaction Time", y = "Reaction Time")
 
# Density plot
# The plot shows the densityplot of reaction time by subject
sleepstudy %>%
  ggplot(aes(x = Reaction, fill = as.factor(Subject))) +
  geom_density(alpha = 0.7) +
  labs(title = "Density Plot of Reaction Time by Subject", x = "Reaction Time") +
  theme_minimal()

# The plot shows the densityplot of reaction time by days
sleepstudy %>%
  ggplot(aes(x = Reaction, fill = as.factor(Days))) +
  geom_density(alpha = 0.7) +
  labs(title = "Density Plot of Reaction Time by Days", x = "Reaction Time") +
  theme_minimal()


#2 Descriptive Statistics

# a. summary statistics for the key variables
# grouping by days

summary_by_days <- sleepstudy %>%
  group_by(Days) %>%
  summarise(
    Mean_Reaction = mean(Reaction),
    Median_Reaction = median(Reaction),
    SD_Reaction = sd(Reaction),
    Min_Reaction = min(Reaction),
    Max_Reaction = max(Reaction)) %>% 
  mutate_all(~round(., 1))
summary_by_days

write.table(summary_by_days, file = "summary_by_days.txt", sep = ",", quote = FALSE, row.names = F)

summary_by_subject <- sleepstudy %>%
  group_by(Subject) %>%
  summarise(
    Mean_Reaction = mean(Reaction),
    Median_Reaction = median(Reaction),
    SD_Reaction = sd(Reaction),
    Min_Reaction = min(Reaction),
    Max_Reaction = max(Reaction))
summary_by_subject

write.table(summary_by_subject, file = "summary_by_subject.txt", sep = ",", quote = FALSE, row.names = F)


# b. Create visualizations

# Histogram
sleepstudy %>%
  ggplot(aes(x = Reaction)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Reaction Time", x = "Reaction Time") +
  theme_minimal()

# Boxplot by Days
sleepstudy %>%
  ggplot(aes(x = as.factor(Days), y = Reaction)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of Reaction Time by Days", x = "Days of Sleep Deprivation", y = "Reaction Time") +
  theme_minimal()

# The plot shows the densityplot of reaction time by days
sleepstudy %>%
  ggplot(aes(x = Reaction, fill = as.factor(Days))) +
  geom_density(alpha = 0.7) +
  labs(title = "Density Plot of Reaction Time by Days", x = "Reaction Time") +
  theme_minimal()

#3: Fit an adequate Model(s)

# a mixed-effects model

# The dataset involves repeated measurements on the same subjects over multiple days, introducing a level of dependency in the data. 
# A mixed-effects model is well-suited for handling this type of correlated observations within subjects.

model <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
model


#4: Interpret the results
summary(model)

#For the fixed effects
# the intercept 251.41 represnts the average reaction time at day 0.
#  the reaction time increases by approximately 10.47 units per day.

#For the mixed effect

# The intercept(subject) S.D 37.12 reflects the variability in the baseline reaction times among different subjects.
# The residual S.D 30.99 is the unexplained variability in reaction times after accounting for fixed and random effects.




#5: Residual Analysis
res <- residuals(model)

#set graphical parameters to generate plot matrix
par(mfrow =c(1,3))

# Plot 1 histogram
hist(res)

# Plot 2 Q-Q Plot
qqnorm(res)
qqline(res)

#Plot 3  Residual plot

plot(fitted(model), res)

