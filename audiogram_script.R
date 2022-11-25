
library(readr)
library(tidyverse)
library(ggplot2)

aud.dat <- read.csv("aud_data.csv")

aud.dat <- gather(aud.dat, key = "ear-freq", value = "dB", -ID)

aud.dat <- aud.dat %>% 
  separate(col = "ear-freq", into = c("ear","freq"), sep = (1)) %>%
  mutate(freq = (type.convert(freq))/1000) %>% 
  mutate(freqLabels = factor(freq)) %>% 
  mutate(ear = factor(ear, levels = c("R", "L"))) %>%
  mutate(ear = recode(ear, "R" = "Right", "L" = "Left"))

std_error <- function(x) sd(x)/sqrt(length(x))

# Regular audiogram with SEM ----
ggplot(data = aud.dat, aes(y = dB, x = freqLabels, group = ear, color = ear, shape = ear))+

# Aud layers
stat_summary(fun = mean,
             fun.min = function(x) mean(x) - std_error(x), 
             fun.max = function(x) mean(x) + std_error(x), 
             geom = "pointrange", position=position_dodge(width=0.3), size = 1, alpha = 0.5) +
stat_summary(fun = mean, geom = "line") +

scale_y_reverse(limits = c(90,-10), breaks = seq(-10, 90, by=10))+

geom_hline(yintercept=25, linetype="dashed", color = "black", size=0.5)+
labs(x = "Frequency (kHz)", y = "Threshold (dB HL)")+
ggtitle("Audiogram") +
theme_bw()+
theme(plot.title = element_text(size = 8))


## Left and Right ear split with different groups ----
## LEFT EAR
aud_L <- ggplot(data = aud.dat[aud.dat$ear == "Left",], aes(y = dB, x = freqLabels))+

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
theme(plot.title = element_text(size = 8))

#RIGHT EAR
aud_R <- ggplot(data = aud.dat[aud.dat$ear == "Right",], aes(y = dB, x = freqLabels))+
  
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
theme(plot.title = element_text(size = 8))

## Display Left and Right ears for groups ----