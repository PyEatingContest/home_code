library(httr)
library(ggplot2)
library(lubridate)
library(jsonlite)
library(tidyverse)

client_id <- 'edada00d8a004722a6eb6199647595e4'
client_secret <- 'd9dc9e6c1cba44f68f0b8d3d6e47b5df'
api_domain <- 'https://api.runkeeper.com'
auth_url <- 'https://runkeeper.com/apps/authorize'
acc_token_url <- 'https://runkeeper.com/apps/token'
deauth_url <- 'https://runkeeper.com/apps/de-authorize'

runkeeper_endpoints = oauth_endpoint(
    base_url = NULL,
    authorize = auth_url,
    access = acc_token_url
)

myapp <- oauth_app("runkeeper", 
    key = client_id,
    secret = client_secret)

runkeeper_token <- oauth2.0_token(runkeeper_endpoints, myapp)

rktoken <- config(token = runkeeper_token)

req <- GET("https://api.runkeeper.com/fitnessActivities?pageSize=300",runkeeper_token, 
           add_headers(Accept = 'application/vnd.com.runkeeper.FitnessActivityFeed+json'))
runs <- fromJSON(content(req, "text", encoding = "ISO-8859-1"))[["items"]]
runs <- mutate(runs, miles = round(3.28084 * total_distance / 5280,3), secs = round(duration), total_calories = round(total_calories))
runs <- mutate(runs, pace = paste((secs / miles) %/% 60, round((secs/ miles) %% 60)))
runs <- mutate(runs, date = round_date(dmy_hms(substr(start_time, 6, length(start_time))),'day'))
runs <- runs %>% filter(date >= mdy('07/01/2016'))
runs$mile_break <- cut(runs$miles,breaks=c(0,4,6,9,11,15))
progress.plot <- ggplot(runs, aes(x=date,y=secs/60/miles)) +
    ggtitle("Run Progress") +
    geom_line(aes(color=mile_break))
progress.plot

# Some additional views of mileage vs. pace
month_progress <- runs %>% 
    filter(miles > 3) %>% 
    mutate(date_month = round_date(date, 'month')) %>% 
    group_by(date_month, mile_break) %>%
    summarize(miles = sum(miles), hours = sum(secs)/60/60, pace = sum(secs)/60/sum(miles))

ggplot(data = month_progress, aes(x = date_month, color = mile_break)) + 
    geom_line(aes(y = miles)) +
    geom_line(aes(y = pace*8)) +
    facet_grid(~ mile_break) +
    scale_y_continuous(sec.axis = sec_axis(~./8, name = "Pace"))

week_progress <- runs %>% 
    mutate(date_week = round_date(date, 'week')) %>% 
    group_by(date_week) %>%
    summarize(miles = sum(miles), hours = sum(secs)/60/60, pace = sum(secs)/60/sum(miles))

ggplot(data = week_progress, aes(x = date_week)) + 
    geom_line(aes(y = miles)) +
    geom_line(aes(y = pace * 4)) +
    coord_cartesian(ylim = c(0,40)) +
    scale_y_continuous(sec.axis = sec_axis(~./4, name = "Pace"))

##View of Monthly Activity Analysis
View(
    runs %>% 
        group_by(month = month(date), year = year(date)) %>%
        summarize(
            mileage = sum(miles),
            tot_min = round(sum(secs)/60,2),
            tot_runs = n(),
            avg_dist = sum(miles)/n(),
            avg_pace = paste((sum(secs) / sum(miles)) %/% 60, round((sum(secs)/ sum(miles)) %% 60))
        ) %>% 
        arrange(year, month)
)
