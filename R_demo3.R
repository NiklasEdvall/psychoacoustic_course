
#Libraries
library(readr)
library(tidyverse)

#Load data
example_data <- read_csv("example_data.csv")

#Copy and rename data
dat <- example_data
rm(example_data)

#What type of variable is ID?
mean(dat$ID) #?
class(dat$ID)

#Factor variables
dat$ID <- factor(dat$ID)

dat$Sex <- factor(dat$Sex)
levels(dat$Sex) <- c("1", "2")
dat$Sex <- recode(dat$Sex, "1" = "M", "2" = "F")

dat$Tinnitus <- factor(dat$Tinnitus)
levels(dat$Tinnitus) <- c("0", "1", "2")
dat$Tinnitus <- recode(dat$Tinnitus, "0" = "No", "1" = "Occasional", "2" = "Constant")

#Check descriptives
mean(dat$Age)
summary(dat$Age)

table(dat$Sex)
table(dat$Sex, dat$Tinnitus)

#Calculate new variables
dat$TMV4LHW <- rowMeans(cbind(dat$LHW500, dat$LHW1000, dat$LHW2000, dat$LHW4000))
dat$TMV4RHW <- rowMeans(cbind(dat$RHW500, dat$RHW1000, dat$RHW2000, dat$RHW4000))

dat$TMV4LB <- rowMeans(cbind(dat$LB500, dat$LB1000, dat$LB2000, dat$LB4000))
dat$TMV4RB <- rowMeans(cbind(dat$RB500, dat$RB1000, dat$RB2000, dat$RB4000))

#Descriptives of new variable
summary(dat$TMV4LHW)
summary(dat$TMV4RHW)

summary(dat$TMV4LB)
summary(dat$TMV4RB)

#Difference between new variables?
t.test(dat$TMV4LHW, dat$TMV4LB, paired = TRUE)
t.test(dat$TMV4RHW, dat$TMV4LB, paired = TRUE)
