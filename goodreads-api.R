library(httr)
library(tidyverse)
library(stringr)
library(xml2)
library(viridis)
library(knitr)
library(lubridate)

API_KEY <- "qcMYtbuLdmoEgYcvnUA"
secret <- 'PbKW0i4TKnkl3bGOIQeYjaVuMw7Dei2st9H4up4F7Yc'

GR_ID <- 2220015

URL <- "https://www.goodreads.com/review/list?"
Friend_URL <- 'https://www.goodreads.com/friend/user.xml?'

callback_URL <- '/oauth/authorize?oauth_callback=http://steve_reading_callback.com/goodreads_oauth_callback'
myapp <- oauth_app("Steve's Reading Graph", 
                   key = API_KEY,
                   secret = secret)

goodreads_endpoints = oauth_endpoint(
    base_url = NULL,
    authorize = 'http://www.goodreads.com/oauth/authorize?',
    access = callback_URL
)
goodreads_token <- oauth2.0_token(goodreads_endpoints, myapp)

get_friends <- function(GR_ID) {
    friends <- GET(Friend_URL, query = list(key = API_KEY, id = GR_ID))
    friend_contents <- content(friends, as = "parsed")
    return(friend_contents)
}
friends <- get_friends(GR_ID) %>% as_list()

get_friend_df <- function(friend_list) {
    first_name <- friend_list %>% 
        xml_find_all("//title") %>% 
        xml_text()
    
    last_name <- friend_list %>% 
        xml_find_all("//rating") %>% 
        xml_text()
    
    df <- tibble(
        first_name, last_name)
    
    return(df)   
}

get_shelf <- function(GR_ID) {
    shelf <- GET(URL, query = list(
        v = 2, key = API_KEY, id = GR_ID, shelf = "read", per_page = 500))
    shelf_contents <- content(shelf, as = "parsed")
    return(shelf_contents)
}

shelf <- get_shelf(GR_ID)

get_df <- function(shelf) {
    
    title <- shelf %>% 
        xml_find_all("//title") %>% 
        xml_text()
    
    rating <- shelf %>% 
        xml_find_all("//rating") %>% 
        xml_text()
    
    added <- shelf %>% 
        xml_find_all("//date_added") %>% 
        xml_text()
    
    started <- shelf %>% 
        xml_find_all("//started_at") %>% 
        xml_text()
    
    read <- shelf %>% 
        xml_find_all("//read_at") %>% 
        xml_text()
    
    df <- tibble(
        title, rating, added, started, read)
    
    return(df)
}

df <- get_df(shelf)

get_books <- function(df) {
    df %>% 
        separate(added, c("DOW", "Month", "Day", "Time", "Offset", "Year"), sep = " ") %>% 
        mutate(date = str_c(Year, "-", Month, "-", Day)) %>% 
        mutate(date = ymd(date)) %>% 
        select(title, rating, date)
}

books <- get_books(df)

books %>% 
    group_by(qtr = quarter(date), yr = as.factor(year(date)) ) %>% 
    count() %>% 
    ggplot(aes(x = qtr , y = n, group = yr, color = yr)) + 
    geom_line()
