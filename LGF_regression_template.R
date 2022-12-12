
#See: https://www.datacamp.com/tutorial/linear-regression-R
#For regression tutorial

#Load package (install with install.package("packagename") if missing)
library(readr)
library(tidyverse)
library(MASS)
library(ggplot2)

#Read in simulated data
dat <- read_csv("LGF_simdata.csv")

#split to separate data frames for 500Hz and 4kHz
dat5 <- dat[,c("ID", "low5", "lowmid5", "mid5", "midhigh5", "high5")]
dat4 <- dat[,c("ID", "low4", "lowmid4", "mid4", "midhigh4", "high4")]

#Gather to "long" format
dat5 <- gather(dat5, key = "loudness_score", value = "dB_presentation", -ID)
dat4 <- gather(dat4, key = "loudness_score", value = "dB_presentation", -ID)

#Recode loudness_score to numeric "dummy variable"
dat5$loudness_score <- recode(dat5$loudness_score, "low5" = 1, "lowmid5" = 2, "mid5" = 3, "midhigh5" = 4, "high5" = 5)
dat4$loudness_score <- recode(dat4$loudness_score, "low4" = 1, "lowmid4" = 2, "mid4" = 3, "midhigh4" = 4, "high4" = 5)

#Linear models
lm_mod5 <- lm(dat5$loudness_score ~ dat5$dB_presentation)
lm_mod4 <- lm(dat4$loudness_score ~ dat4$dB_presentation)

#Plot data and linear model fit in standard R
plot(dat5$dB_presentation, dat5$loudness_score)
abline(lm_mod5)

plot(dat4$dB_presentation, dat4$loudness_score)
abline(lm_mod4)

#A nicer plot(?) option using ggplot-package
ggplot(dat5, aes(x = dB_presentation, y = loudness_score)) + 
  geom_point(shape = 1, size = 3) +
  stat_smooth(method = "lm", col = "red", alpha = 0.35)+
  xlab("Level presented (dB HL)")+
  ylab("Loudness category")+
  theme_bw()

ggplot(dat4, aes(x = dB_presentation, y = loudness_score)) + 
  geom_point(shape = 1, size = 3) +
  stat_smooth(method = "lm", col = "red", alpha = 0.35)+
  xlab("Level presented (dB HL)")+
  ylab("Loudness category")+
  theme_bw()

#Make dummy variables factor
dat5$loudness_score <- factor(dat5$loudness_score, levels = c(1,2,3,4,5))
dat4$loudness_score <- factor(dat5$loudness_score, levels = c(1,2,3,4,5))

#Linear regression models per frequency: lm([target] ~ [predictor / features], data = [data source])
mod5 <- glm(loudness_score ~ dB_presentation, data = dat5, family = binomial)
mod4 <- glm(loudness_score ~ dB_presentation, data = dat4, family = binomial)

#Display model summary
summary(mod4)
summary(mod5)
