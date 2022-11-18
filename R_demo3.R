
#Libraries
library(readr)

#Load data
example_data <- read_csv("C:/Users/nikedv/OneDrive - KI.SE/Skrivbordet/example_data.csv")

#Copy and rename data
dat <- example_data
rm(example_data)

#What type of variable is ID?
mean(dat$ID) #?
class(dat$ID)

#Factor variables
dat$ID <- factor(dat$ID)

dat$Sex <- factor(dat$Sex)
levels(dat$Sex) <- c("M", "F")

dat$Tinnitus <- factor(dat$Tinnitus)
levels(dat$Tinnitus) <- c("No", "Occasional", "Constant")

#Check descriptives
mean(dat$Age)
summary(dat$Age)

table(dat$Sex)
table(dat$Sex, dat$Tinnitus)

#Calculate new variables
dat$TMV4LHW <- rowMeans(cbind(dat$LHW500, dat$LHW1000, dat$LHW2000, dat$LHW4000))
dat$TMV4RHW <- rowMeans(cbind(dat$RHW500, dat$RHW1000, dat$RHW2000, dat$RHW4000))

#Descriptives of new variable
summary(dat$TMV4LHW)
summary(dat$TMV4RHW)

#Difference between new variables?
t.test(dat$TMV4LHW, dat$TMV4RHW)
