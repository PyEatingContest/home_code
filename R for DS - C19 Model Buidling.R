# Chapter 19 - Model Building #
library(tidyverse)
library(modelr)
options(na.action = na.warn)

library(lubridate)
library(nycflights13)

data(diamonds)

#diamond box plots
ggplot(data = diamonds, aes(x = cut, y = price)) + geom_boxplot()
ggplot(data = diamonds, aes(x = color, y = price)) + geom_boxplot()
ggplot(data = diamonds, aes(x = clarity, y = price)) + geom_boxplot()

ggplot(data = diamonds, aes(x = carat, y = price)) + geom_hex(bins = 50)

# Assess percentile
diamonds %>% 
    select(carat) %>% 
    mutate(perc_rank = percent_rank(carat)) %>% 
    filter(round(perc_rank,3) == 0.997) %>% 
    summarize(mean(carat))

diamonds2 <- diamonds %>% 
    filter(carat <= 2.5) %>% 
    mutate(log2price = log2(price), 
           log2carat = log2(carat))

ggplot(diamonds2, aes(x = log2carat, y = log2price)) + geom_hex(bins = 50)

mod_diamond <- lm(log2price ~ log2carat, data = diamonds2)
diamonds2 <- diamonds2 %>% add_residuals(mod_diamond, 'lresid')

mod2_diamond <- lm(log2price ~ log2carat + cut + color + clarity, data = diamonds2)
grid <- diamonds2 %>% 
    data_grid(cut, .model = mod2_diamond) %>% 
    add_predictions(mod2_diamond)

