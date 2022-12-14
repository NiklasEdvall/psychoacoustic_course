# setup ====
library(readr) 
library(tidyverse)
library(ggplot2)
library(ggpubr)

# Read data ====
aud.dat <- read.csv("aud_data_subject1.csv")

#Remove variable "Group"
aud.dat <- within(aud.dat, rm(Group))   

# make long
aud.dat <- gather(aud.dat, key = "ear-freq", value = "dB", -ID)

# split and rename variables
aud.dat <- aud.dat %>% 
  separate(col = "ear-freq", into = c("ear","freq"), sep = (1)) %>%
  mutate(freq = (type.convert(freq))/1000) %>% 
  mutate(freqLabels = factor(freq)) %>% 
  mutate(ear = factor(ear, levels = c("R", "L"))) %>%
  mutate(ear = recode(ear, "R" = "Right", "L" = "Left"))

# define function for SEM (standard error of mean)
std_error <- function(x) sd(x)/sqrt(length(x))

# Regular audiogram with SEM ====
plot1 <- ggplot(data = aud.dat, aes(y = dB, x = freqLabels, group = ear, color = ear, shape = ear))+

# Aud layers
stat_summary(fun = mean,
             fun.min = function(x) mean(x) - std_error(x), 
             fun.max = function(x) mean(x) + std_error(x), 
             geom = "pointrange", position=position_dodge(width=0.3), size = 1, alpha = 0.5) +
stat_summary(fun = mean, geom = "line") +

scale_y_reverse(limits = c(90,-10), breaks = seq(-10, 90, by=10))+

geom_hline(yintercept=25, linetype="dashed", color = "black", size=0.5)+
labs(x = "Frequency (kHz)", y = "Threshold (dB HL)")+
ggtitle("Subject 1") +
theme_bw()+
  theme(legend.position="none")+
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 10))

# Read data ====
aud.dat <- read.csv("aud_data_subject2.csv")

#Remove variable "Group"
aud.dat <- within(aud.dat, rm(Group))   

# make long
aud.dat <- gather(aud.dat, key = "ear-freq", value = "dB", -ID)

# split and rename variables
aud.dat <- aud.dat %>% 
  separate(col = "ear-freq", into = c("ear","freq"), sep = (1)) %>%
  mutate(freq = (type.convert(freq))/1000) %>% 
  mutate(freqLabels = factor(freq)) %>% 
  mutate(ear = factor(ear, levels = c("R", "L"))) %>%
  mutate(ear = recode(ear, "R" = "Right", "L" = "Left"))

# define function for SEM (standard error of mean)
std_error <- function(x) sd(x)/sqrt(length(x))

# Regular audiogram with SEM ====
plot2 <- ggplot(data = aud.dat, aes(y = dB, x = freqLabels, group = ear, color = ear, shape = ear))+
  
  # Aud layers
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - std_error(x), 
               fun.max = function(x) mean(x) + std_error(x), 
               geom = "pointrange", position=position_dodge(width=0.3), size = 1, alpha = 0.5) +
  stat_summary(fun = mean, geom = "line") +
  
  scale_y_reverse(limits = c(90,-10), breaks = seq(-10, 90, by=10))+
  
  geom_hline(yintercept=25, linetype="dashed", color = "black", size=0.5)+
  labs(x = "Frequency (kHz)", y = "Threshold (dB HL)")+
  ggtitle("Subject 2") +
  theme_bw()+
  theme(legend.position="none")+
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 10))

# Read data ====
aud.dat <- read.csv("aud_data_subject3.csv")

#Remove variable "Group"
aud.dat <- within(aud.dat, rm(Group))   

# make long
aud.dat <- gather(aud.dat, key = "ear-freq", value = "dB", -ID)

# split and rename variables
aud.dat <- aud.dat %>% 
  separate(col = "ear-freq", into = c("ear","freq"), sep = (1)) %>%
  mutate(freq = (type.convert(freq))/1000) %>% 
  mutate(freqLabels = factor(freq)) %>% 
  mutate(ear = factor(ear, levels = c("R", "L"))) %>%
  mutate(ear = recode(ear, "R" = "Right", "L" = "Left"))

# define function for SEM (standard error of mean)
std_error <- function(x) sd(x)/sqrt(length(x))

# Regular audiogram with SEM ====
plot3 <- ggplot(data = aud.dat, aes(y = dB, x = freqLabels, group = ear, color = ear, shape = ear))+
  
  # Aud layers
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - std_error(x), 
               fun.max = function(x) mean(x) + std_error(x), 
               geom = "pointrange", position=position_dodge(width=0.3), size = 1, alpha = 0.5) +
  stat_summary(fun = mean, geom = "line") +
  
  scale_y_reverse(limits = c(90,-10), breaks = seq(-10, 90, by=10))+
  
  geom_hline(yintercept=25, linetype="dashed", color = "black", size=0.5)+
  labs(x = "Frequency (kHz)", y = "Threshold (dB HL)")+
  ggtitle("Subject 3") +
  theme_bw()+
  theme(plot.title = element_text(size = 16), axis.text = element_text(size = 10))

#Arrange plots
ggarrange(plot1, plot2, plot3, ncol = 3, nrow = 1)


######################
## Left and Right ear split with different groups
######################

# Read data ====
aud.dat <- read.csv("aud_data.csv")

#Specify group as factor
aud.dat$Group <- factor(aud.dat$Group, levels = c("1", "2"))

#Recode group
aud.dat$Group <- recode(aud.dat$Group, "1" = "M", "2" = "F")

# Do NOT remove variable "Group" (as for above non-grouped aud)
# aud.dat <- within(aud.dat, rm(Group))

# make long
aud.dat <- gather(aud.dat, key = "ear-freq", value = "dB", -ID, -Group)

# split and rename variables
aud.dat <- aud.dat %>% 
  separate(col = "ear-freq", into = c("ear","freq"), sep = (1)) %>%
  mutate(freq = (type.convert(freq))/1000) %>% 
  mutate(freqLabels = factor(freq)) %>% 
  mutate(ear = factor(ear, levels = c("R", "L"))) %>%
  mutate(ear = recode(ear, "R" = "Right", "L" = "Left"))

# define function for SEM (standard error of mean)
std_error <- function(x) sd(x)/sqrt(length(x))

##Create plot for LEFT EAR ====
aud_L <- ggplot(data = aud.dat[aud.dat$ear == "Left",], aes(y = dB, x = freqLabels, group = Group, color = Group, shape = Group))+

#Aud layers
stat_summary(fun = mean,
             fun.min = function(x) mean(x) - std_error(x), 
             fun.max = function(x) mean(x) + std_error(x), 
             geom = "pointrange", position=position_dodge(width=0.3), size = 1, alpha = 0.5) +
stat_summary(fun = mean, geom = "line") +

scale_y_reverse(limits = c(90,-10), breaks = seq(-10, 90, by=10))+

geom_hline(yintercept=25, linetype="dashed", color = "black", size=0.5)+
labs(x = "Frequency (kHz)", y = "Threshold (dB HL)")+
ggtitle("Audiogram L") +
theme_bw()+
theme(plot.title = element_text(size = 16), axis.text = element_text(size = 12))

#Create plot for RIGHT EAR
aud_R <- ggplot(data = aud.dat[aud.dat$ear == "Right",], aes(y = dB, x = freqLabels, group = Group, color = Group, shape = Group))+
  
#Aud layers
stat_summary(fun = mean,
             fun.min = function(x) mean(x) - std_error(x), 
             fun.max = function(x) mean(x) + std_error(x), 
             geom = "pointrange", position=position_dodge(width=0.3), size = 1, alpha = 0.5) +
stat_summary(fun = mean, geom = "line") +

scale_y_reverse(limits = c(90,-10), breaks = seq(-10, 90, by=10))+

geom_hline(yintercept=25, linetype="dashed", color = "black", size=0.5)+
labs(x = "Frequency (kHz)", y = "Threshold (dB HL)")+
ggtitle("Audiogram R") +
theme_bw()+
theme(plot.title = element_text(size = 16), axis.text = element_text(size = 12))

## Display Left and Right ears for groups ====

ggarrange(aud_L, aud_R, ncol = 2, nrow = 1)
