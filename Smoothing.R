library(tidyverse)
library(dslabs)
library(ggplot2)
data("polls_2008")
qplot(day, margin, data = polls_2008)
mean(polls_2008$margin)

#################
#Bin Smoothing
#################
span = 7
fit <- with(polls_2008, ksmooth(day, margin, kernel = "box", bandwidth = span))
polls_2008 %>% mutate(smooth =fit$y) %>% ggplot(aes(day, margin))+geom_point(size=3, alpha= .5, color= "grey")+geom_line(aes(day, smooth), color= "red")
span = 7
fit <- with(polls_2008, ksmooth(day, margin, kernel = "normal", bandwidth = span))
polls_2008 %>% mutate(smooth =fit$y) %>% ggplot(aes(day, margin))+geom_point(size=3, alpha= .5, color= "grey")+geom_line(aes(day, smooth), color= "red")

