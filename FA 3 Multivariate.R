
library(tidyverse)
library(reshape2)
library(afex)
library(ggplot2)
library(emmeans)
library(dplyr)
library(car)

data <- carDatadata <- read.csv("C:/Users/yohan gacasa/Downloads/Looksorpersonality.csv")

long_data <- data %>%
  gather(key = "Condition", value = "Rating", att_high:ug_none) %>%
  separate(Condition, into = c("Looks", "Personality"), sep = "_")

long_data$Gender <- factor(long_data$Gender)
long_data$Looks <- factor(long_data$Looks, levels = c("att", "av", "ug"), labels = c("Attractive", "Average", "Ugly"))
long_data$Personality <- factor(long_data$Personality, levels = c("high", "some", "none"), labels = c("High", "Some", "None"))

anova_results <- aov_car(
  Rating ~ Gender * Looks * Personality + Error(Gender/(Looks * Personality)), 
  data = long_data, 
  type = 3  
)

anova_results


set.seed(123)
df <- data.frame(
  Gender = rep(c("Male", "Female"), each = 18),
  Looks = rep(c("Attractive", "Average", "Ugly"), each = 6, times = 2),
  Personality = rep(c("High", "Some", "None"), times = 12),
  Rating = round(runif(36, 1, 10), 1)
)

head(df)

aov_results <- aov_car(Rating ~ Looks * Personality + Error(Gender/(Looks*Personality)), data = df)

summary(aov_results)

# Main effect of Looks
ggplot(df, aes(x = Looks, y = Rating, fill = Looks)) +
  geom_boxplot() +
  labs(title = "Main Effect of Looks", x = "Looks", y = "Rating") +
  theme_minimal()
# Main effect of Personality
ggplot(df, aes(x = Personality, y = Rating, fill = Personality)) +
  geom_boxplot() +
  labs(title = "Main Effect of Personality", x = "Personality", y = "Rating") +
  theme_minimal()

ggplot(df, aes(x = Looks, y = Rating, color = Personality, group = Personality)) +
  geom_point() +
  geom_line() +
  labs(title = "Interaction between Looks and Personality", x = "Looks", y = "Rating") +
  theme_minimal()

df <- data.frame(
  Gender = rep(c("Male", "Female"), each = 18),
  Looks = rep(c("Attractive", "Average", "Ugly"), each = 6, times = 2),
  Personality = rep(c("High", "Some", "None"), times = 12),
  Rating = round(runif(36, 1, 10), 1)
)

leveneTest(Rating ~ Looks * Personality * Gender, data = df)


