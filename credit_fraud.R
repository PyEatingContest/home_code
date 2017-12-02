library(tidyverse)
library(caret)

raw <- read_csv('//Users//stevensandiford//Downloads//creditcardfraud.zip')

data <- raw
pairs(data[,c(2:10,31)])
