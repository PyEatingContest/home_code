library(tidyverse)
library(modelr)
library(lubridate)

ggplot(diamonds, aes(cut, price)) + geom_boxplot()

ggplot(diamonds, aes(carat, price)) + geom_hex(bins = 25)

diamonds2 <- diamonds %>% 
    filter(carat <= 2.5) %>% 
    mutate(lprice = log2(price), lcarat = log2(carat))

ggplot(diamonds2, aes(lcarat, lprice)) + geom_hex(bins = 50)

mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)

grid <- diamonds2 %>% 
    data_grid(carat = seq_range(carat, 20)) %>% 
    mutate(lcarat = log2(carat)) %>% 
    add_predictions(mod_diamond, 'lprice') %>% 
    mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) +
    geom_hex(bins = 50) +
    geom_line(data = grid, color = 'red', size = 1)

diamonds2 <- diamonds2 %>% 
    add_residuals(mod_diamond, 'lresid')

ggplot(diamonds2, aes(lcarat, lresid)) +
    geom_hex(bins = 50)
