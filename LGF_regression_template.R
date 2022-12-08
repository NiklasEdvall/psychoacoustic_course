
#See: https://www.datacamp.com/tutorial/linear-regression-R
#For regression tutorial

#Load package (install with install.package("packagename") if missing)
library(readr)
library(tidyverse)

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

#Linear regression models per frequency: lm([target] ~ [predictor / features], data = [data source])
mod5 <- lm(loudness_score ~ dB_presentation, data = dat5)
mod4 <- lm(loudness_score ~ dB_presentation, data = dat4)

#Display model summary
summary(mod4)
summary(mod5)

#plot LGF
plot(dat5$dB_presentation, dat5$loudness_score)
plot(dat4$dB_presentation, dat4$loudness_score)



