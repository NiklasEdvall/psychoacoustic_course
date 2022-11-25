##### Libraries
library(ggplot2)
library(hrbrthemes)
library(tidyr)

#####
#Produce 10 random numbers
runif(10)

#Round numbers to zero decimals
round(runif(10),0)

#calculate means of ten "coin flips"
mean(round(runif(10),0))

for (i in 1:10)
{print(i)}

#####

#Create variable "coins"
coins <- c()

#Flip ten coins 1000 times, put results in "coins"
for (i in 1:1000)
{coins[i] <- mean(round(runif(10),0))}

#Plot histogram of "coins"
hist(coins, ylim = c(0,350), xlim = c(0,1), breaks = 10, xaxt='n')
axis(side=1, at=seq(0,1, 0.1), labels=seq(0,1,0.1))

## PP Dice

##### Same as above but with t-distribution overlay in plot

for (i in 1:1000)
{coins[i] <- mean(round(runif(10),1))}

hist(coins, ylim = c(0,5), xlim = c(0,1), freq=FALSE, breaks = 12, xaxt='n')
axis(side=1, at=seq(0,1, 0.1), labels=seq(0,1,0.1))

par(new = TRUE)
curve(dt(x, df=10), from=-4, to=4, xaxt='n', yaxt='n', ylab = "")

## PP t-distribution

#t-test of coins compared to known mean
t.test(coins, mu = 0.5, alternative = "two.sided")

##### What if we want to compare different coins?

#sample coins
green <- sample(coins, 250)
purple <- sample(coins, 250)

# Build dataset
data <- data.frame(
  type = c( rep("green", 250), rep("purple", 250) ),
  value = c(green, purple)
)

#plot distribution of green and purple coins
data %>%
ggplot( aes(x=value, fill=type)) +
geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
scale_fill_manual(values=c("#69b3a2", "#404080")) +
theme_ipsum() +
labs(fill="")

#t-test are green or purple coins better at showing heads?
t.test(green, purple)

#shapiro?

