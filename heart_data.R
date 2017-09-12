library(glmpath)
library(tidyverse)
library(broom)
library(ggplot2)
library(corrplot)
library(GGally) #does a better version of plot matrix

data(heart.data)
data(iris)
heart <- bind_cols(as_tibble(heart.data[[1]]),as_tibble(heart.data[[2]]))

corrplot.mixed(cor(heart, use='complete.obs'), upper = 'circle')
corrplot(cor(heart, use='complete.obs'), type = "upper")
corrplot(cor(heart, use='complete.obs'), type = "upper", method = "number")

pairs(heart) #Base R paid scatterplot
heart$famhist <- as.factor(heart$famhist)
ggpairs(heart, columns = c(1:4), 
        upper = NULL, 
        lower = list(continous = 'cor'), 
        diag = list(continous = 'points'))
