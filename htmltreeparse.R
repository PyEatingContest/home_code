library(XML)
library(httr)
library(tidyverse)
library(stringr)

JM_URL <- 'http://app.mediaplex.com/reports/bl_template.get_web_query?rtid_in=1079198'
RES_URL <- 'http://app.mediaplex.com/reports/bl_template.get_web_query?rtid_in=1079224'
UN <- 'steven.sandiford@logmein.com'
PW <- '7!R^%TsxOkl5UV0i67!K'

mediaplex_report <- function(url, username, password){
  response <- GET(url, authenticate(username, password))
  html <- content(response, 'text')
  doc <- htmlParse(html, asText=TRUE)
  plain.text <- xpathSApply(doc, "//table/tbody/tr/td", xmlValue, trim=T)
  df <- as_tibble(matrix(plain.text,ncol = 9,byrow = T))
  colnames(df) <- c('Campaign ID', 'Campaign Name', 'Site ID', 'Site Name', 'Placement ID', 'Placement Name',
                    'Date', 'Impressions', 'Clicks')
  vars <- c("Impressions", "Clicks") # vector of column names 
  df %>% 
    mutate_at(vars, funs(parse_number))
  return(df)
}

jm_test <- mediaplex_report(JM_URL, UN, PW)
jm_test <- jm_test %>% mutate(Source3 = str_replace_all(`Placement ID`, '-',''))

library(lubridate)
baa_url <- 'http://registration.baa.org/2016/cfMedley/Public/iframe_leaderboard.cfm?mode=results&GenderID=1'
response <- GET(baa_url)
html <- content(response, 'text')
doc <- htmlParse(html, asText=TRUE)
plain.text <- xpathSApply(doc, "//table/tbody/tr/td", xmlValue, trim=T)
df <- as_tibble(matrix(plain.text,ncol = 9,byrow = T))
colnames(df) <- c('Rank_2015', 'Name', 'Home', 'Age_Group', '5k', '10k',
                  'Half', 'Cum_Time', 'Percentile')
df <- df %>% 
    mutate(time = hms(Cum_Time)) %>% 
    separate(`5k`, c('5K_time', '5K_rank'), sep = "\\s") %>% 
    separate(`10k`, c('10K_time', '10K_rank'), sep = "\\s") %>% 
    separate(`Half`, c('Half_time', 'Half_rank'), sep = "\\s") %>% 
    mutate_at(vars(`5K_rank`, `10K_rank`, Half_rank), 
              funs(str_replace_all(., '[()]', '')))

df %>% filter(Rank_2015 <= 100) %>% ggplot(aes(x = ))
