library(lubridate)
library(tidyverse)
library(purrr)
library(haven)

original <- read_sas('natl2002.sas7bdat')
mass <- original %>% 
    filter(statenat == 22)

good_features <- mass %>%
    summarise_each(funs(sum(is.na(.)/n()))) %>% 
    gather(key = feature, value = missing_perc) %>% 
    filter(missing_perc <= 0.2)
    
vars <- good_features$feature
mass <- mass %>% select(one_of(vars))
write_csv(mass, "mass_2002_nhs.csv")

# Coding SAS variables ----------------------------------------------------

mass_recode <- mass %>% mutate(sex_of_child = ifelse(csex == 1, "Male", "Female"),
                        gest_range = as.factor(case_when(
                            mass$gestat10 == 1 ~ "Under 20 weeks",
                            mass$gestat10 == 2 ~ "20 to 27 weeks",
                            mass$gestat10 == 3 ~ "28 to 31 weeks",
                            mass$gestat10 == 4 ~ "32 to 35 weeks",
                            mass$gestat10 == 5 ~ "36 weeks",
                            mass$gestat10 == 6 ~ "37 to 39 weeks",
                            mass$gestat10 == 7 ~ "40 weeks",
                            mass$gestat10 == 8 ~ "41 weeks",
                            mass$gestat10 == 9 ~ "42 weeks and over",
                            mass$gestat10 == 10 ~ "Not stated")
                            ),
                        vbac_stat = as.factor(case_when(
                            mass$vbac == 1 ~ "Method used",
                            mass$vbac == 2 ~ "Method not used",
                            mass$vbac == 8 ~ "Method not on certificate",
                            mass$vbac == 9 ~ "Method unknown or not stated"
                        )),
                        mother_age = as.factor(case_when(
                            mass$mage12 == 1 ~ "Under 15 years",
                            mass$mage12 == 3 ~ "15 years",
                            mass$mage12 == 4 ~ "16 years",
                            mass$mage12 == 5 ~ "17 years",
                            mass$mage12 == 6 ~ "18 years",
                            mass$mage12 == 7 ~ "19 years",
                            mass$mage12 == 8 ~ "20 - 24 years",
                            mass$mage12 == 9 ~ "25 - 29 years",
                            mass$mage12 == 10 ~ "30 - 34 years",
                            mass$mage12 == 11 ~ "35 - 39 years",
                            mass$mage12 == 12 ~ "40 - 44 years",
                            mass$mage12 == 13 ~ "45 - 49 years",
                            mass$mage12 == 14 ~ "50 - 54 years"
                        )),
                        birth_order = as.factor(case_when(
                            mass$livord9 == 1 ~ "First child",
                            mass$livord9 == 2 ~ "Second child",
                            mass$livord9 == 3 ~ "Third child",
                            mass$livord9 == 4 ~ "Fourth child",
                            mass$livord9 == 5 ~ "Fifth child",
                            mass$livord9 == 6 ~ "Sixth child",
                            mass$livord9 == 7 ~ "Seventh child",
                            mass$livord9 == 8 ~ "Eigth and over child",
                            mass$livord9 == 9 ~ "Unknown or not stated"
                        ))
                    )
mass_recode$gest_range <- relevel(mass_recode$gest_range, "Under 20 weeks")
mass_recode$birth_order <- ordered(mass_recode$birth_order, 
                                   c("First child","Second child", "Third child",
                                     "Fourth child", "Fifth child", "Sixth child",
                                     "Seventh child","Eigth and over child","Unknown or not stated"))
# Visualizations ----------------------------------------------------------

# Births by father's age
mass_recode %>% 
    filter(dfage != 99) %>% 
    ggplot(aes(x = dfage, color = sex_of_child)) +
    geom_freqpoly(alpha = .5, position = 'identity', binwidth = 1) +
    coord_cartesian(xlim = c(15,60))

# Child gender ratio based on father's age
mass_recode %>% 
    filter(dfage != 99, dfage <=65) %>% 
    group_by(dfage, sex_of_child) %>%
    summarize(n = n()) %>%
    spread(key = sex_of_child, value = n) %>%
    ungroup() %>% 
    mutate(freq = Female / sum(Female, na.rm = T))

# Birth order vs sex
mass_recode %>% 
    group_by(sex_of_child, birth_order) %>% 
    count() %>% 
    spread(key = sex_of_child, value = n) %>%
    mutate_each(funs(round(./sum(.)*100,1)), one_of('Female', 'Male')) %>%
    mutate(cum_male = cumsum(Male), cum_female = cumsum(Female))

# Births by gestation range
mass_recode %>% 
    filter(birth_order %in% c('First child', 'Second child', 'Third child')) %>% 
    group_by(gest_range, birth_order) %>% 
    count() %>%
    ggplot(aes(x = gest_range, y = n, fill = birth_order)) + 
    geom_bar(stat = 'identity', position = 'dodge')

# Births by gestation week
mass_recode %>%
    filter(birth_order == 'Second child') %>% 
    group_by(dgestat) %>% 
    count() %>%
    mutate(perc = n / sum(n, na.rm = T) * 100) %>% 
    ggplot(aes(x = dgestat, y = perc)) + 
    geom_bar(stat = 'identity') +
    coord_cartesian(xlim = c(35,42)) + 
    geom_text(aes(label = round(cumsum(perc),0)), 
              position = position_stack(vjust = 1.05))
 