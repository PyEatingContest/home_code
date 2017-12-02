library(httr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(jsonlite)
library(data.table)
library(geosphere)

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

req <- GET("https://api.runkeeper.com/fitnessActivities?pageSize=250",runkeeper_token, 
           add_headers(Accept = 'application/vnd.com.runkeeper.FitnessActivityFeed+json'))
runs <- fromJSON(content(req, "text", encoding = "ISO-8859-1"))[["items"]]

##runs <- rbindlist(lapply(runs, as.list))
runs <- mutate(runs, miles = 3.28084 * total_distance / 5280, secs = round(duration))
runs <- mutate(runs, pace = paste((secs / miles) %/% 60, round((secs/ miles) %% 60)))

#runs <- mutate(runs, pace = ms(paste((secs / miles) %/% 60, round((secs/ miles) %% 60))))
runs <- mutate(runs, date = round_date(dmy_hms(substr(start_time, 6, length(start_time))),'day'))
runs <- data.table(runs)
runs <- runs[date > mdy('06/01/2016')]

#runs$m <- with(runs, miles %/% 1)
runs$m <- with(runs, miles)
runs$mileage <- cut(runs$m,breaks=c(0,3,3.2,6,6.2,13,13.2,15))

p <- ggplot(runs, aes(x=date,y=secs/60/miles))+geom_line(aes(color=mileage))
p

##Monthly Pace Analysis
View(runs[date > mdy('06/30/2016') & order(date)
          ,.(pace = paste((sum(secs) / sum(miles)) %/% 60, round((sum(secs)/sum(miles)) %% 60))
          , mileage = sum(miles)
          , totsecs = sum(secs)
          , totruns = .N
          , avgDist = sum(miles)/.N)
          ,by=.(month(date), year(date))])

calc.distance <- function(activityid){
    response <- GET(paste0("https://api.runkeeper.com/fitnessActivities/",activityid),runkeeper_token, 
                    add_headers(Accept = 'application/vnd.com.runkeeper.FitnessActivity+json'))
    temp.path <- fromJSON(content(response, "text", encoding = "ISO-8859-1"))[["path"]]
    if (is.null(temp.path)){next}
    key.cols <- temp.path[,c("latitude","longitude","altitude")]
    temp.holder <- 0
    temp.distance <- 0
    for (i in 1:(nrow(key.cols)-1)) {
        altitude.change <- abs(key.cols[i,3]-key.cols[i+1,3])
        direct.distance <- distm(key.cols[i,1:2],key.cols[i+1,1:2], fun=distHaversine)[[1,1]]
            if(altitude.change!=0){
                temp.distance <- sqrt(direct.distance^2+altitude.change^2)
            } else {
                temp.distance <- direct.distance
            }
        temp.holder <- temp.holder + temp.distance
    }
    temp.holder <- 3.28084 * temp.holder / 5280 
    temp.holder
}

req2 <- GET("https://api.runkeeper.com/fitnessActivities/908951482",runkeeper_token, 
            add_headers(Accept = 'application/vnd.com.runkeeper.FitnessActivity+json'))
path <- fromJSON(content(req2, "text", encoding = "ISO-8859-1"))[["path"]]

lla <- path[,c("latitude","longitude","altitude")]
##Tries to calculate length of run by using GPS coordinates and altitude
holder <- 0
distance <- 0
for (i in 1:(nrow(lla)-1)) {
    altitude.change <- abs(lla[i,3]-lla[i+1,3])
    direct.distance <- distm(lla[i,1:2],lla[i+1,1:2], fun=distHaversine)[[1,1]]
#     if(altitude.change!=0){
#         distance <- sqrt(direct.distance^2+altitude.change^2)
#     } else {
#         distance <- direct.distance
#     }
    holder <- holder + direct.distance
}
holder <- 3.28084 * holder / 5280 
holder
