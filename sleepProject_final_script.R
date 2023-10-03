# Loading necessary libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(leaps)
library(car)

# Reading in the csv file
sleep <- read.csv("Sleep_Efficiency.csv")

# Cleaning data
colnames(sleep) <- c("id", "age", "sex", "bedtime", "wake.time", "duration", 
                     "efficiency", "percent.rem", "percent.deep",
                     "percent.light", "awakenings", "caffeine.consumed", 
                     "alcohol.consumed", "smoker","exercise.frequency")
sleep <- dplyr::select(sleep, -id, -bedtime, -wake.time, -awakenings, -percent.rem, -percent.light, -percent.deep)
sleep <- na.omit(sleep)
sleep$smoker<-ifelse(sleep$smoker=="Yes",1,0)
sleep$smoker <- as.factor(sleep$smoker)
sleep$sex <- as.factor(sleep$sex)

## Preliminary comparison of variables
boxplot(efficiency ~ sex, data = sleep,
        main = "Proportion of Sleep Efficiency v.s. Sex",
        xlab = "Sex",
        ylab = "Proportion of Sleep Efficiency")

hist(sleep$age, xlab = "Age (in years)")
range(sleep$age)

sleep$age.groups <- cut(sleep$age,
                      breaks = c(18, 36, 54, 70),
                      include.lowest = T,
                      right = F)

boxplot(efficiency ~ age.groups, data = sleep,
        main = "Proportion of Sleep Efficiency v.s. Age Groups",
        xlab = "Age Groups (in years)",
        ylab = "Proportion of Sleep Efficiency")

## Factors that may affect sleep quality

boxplot(efficiency ~ caffeine.consumed, data = sleep,
        main = "Proportion of Sleep Efficiency v.s. Caffeine Consumed in 24 Hours",
        xlab = "Caffeine Consumed 24h before sleep (mg)",
        ylab = "Proportion of Sleep Efficiency")

boxplot(efficiency ~ alcohol.consumed, data = sleep,
        main = "Proportion of Sleep Efficiency v.s. Alcohol Consumed 24h before sleep",
        xlab = "Alcohol Consumed 24h before sleep (oz)",
        ylab = "Proportion of Sleep Efficiency")

boxplot(efficiency ~ smoker, data = sleep,
        main = "Proportion of Sleep Efficiency v.s. Smoking Status",
        xlab = "Smoking Status",
        ylab = "Proportion of Sleep Efficiency")

boxplot(efficiency ~ exercise.frequency, data = sleep,
        main = "Proportion of Sleep Efficiency v.s. Exercise sessions each week",
        xlab = "Exercise sessions each week",
        ylab = "Proportion of Sleep Efficiency")

# Linear model
reg1 <- lm(efficiency~age+sex+caffeine.consumed+alcohol.consumed+smoker+exercise.frequency, data = sleep)
summary(reg1)

# Fitted values v.s. residual plot
plot(reg1$fitted.values, reg1$residuals)
abline(h=0)

# Q-Q plot of residuals
qqPlot(reg1$residuals)

# Final linear model
reg2 <- lm(efficiency~age+alcohol.consumed+smoker+exercise.frequency, data = sleep)
summary(reg2)

# Subset selection algorithm
s <- regsubsets(efficiency~age+sex+caffeine.consumed+alcohol.consumed+smoker+exercise.frequency, data=sleep, method="exhaustive")
ss <- summary(s)
ss

ss_adjr2 <- ss$adjr2
ss_cp <- ss$cp
ss_rsp <- ss$rsq

