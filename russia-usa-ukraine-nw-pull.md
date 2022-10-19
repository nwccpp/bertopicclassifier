---
title: "russia-usa-ukraine-classifier-newswhip-pull"
author: "adl"
date: "2022-10-19"
output: github_document
---

```{r setup, }
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 
knitr::opts_chunk$set(eval = FALSE)
```
## Supplemental Code for: How foreign propaganda claims penetrates American domestic partisan news: The case of Ukraine                             
### Authors: 
#### Erik Nisbet, Ayse D. Lokmanoglu, Olga Kamenchuk, Rod Abhari, Esteban Villa Turek, Chloe Mortenson, Sapna Suresh, & Sam Barnett Jenkin

The code includes all the steps of newswhip article pull for the Russian and USA corpi. 

The data set accompanying the code: <>

For questions, or more information on the code please contact: 
Ayse D. Lokmanoglu\
ayse [dot] lokmanoglu [at] northwestern [dot] edu

### Russian State Sponsored News
State sponsored media list
- rt.com/
- sputniknews.com/
- globalresearch.ca
- en.news-front.info/
- strategic-culture.org/
- oneworld.press/

Load the packages
```{r}
library(httr)
library(jsonlite)
library(devtools)
library(urltools)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
```

1. Create a function to get popular articles from Newswhip API with search words
- Create an object for maximum number of articles for each day
```{r}
num_articles <- 5000
```
-Create a variable with API key
```{r, echo=FALSE}
api_key <- 'XXXXXXX'
```
- Create API endpoint
```{r}
api_endpoint <- paste0('https://api.newswhip.com/v1/articles?key=', api_key)
```
- Write the function
```{r}
get_newswhip_articles <- function(api_key, limit, start_time, end_time) {
  api_endpoint <- paste0('https://api.newswhip.com/v1/articles?key=', api_key)          
  data <- paste0('{\"filters\": [\"language: en AND publisher: rt.com AND ((ukraine) OR (ukrainian) OR (donetsk) OR (luhansk) OR (lugansk) OR (donbass) OR (crimea) OR (zelensky))\"], 
                           \"size\": ', limit, ', 
                           \"from\": ', start_time, ',
                           \"to\": ', end_time, ',
                           \"sort_by\":\"fb_tw_and_li\",
                           \"search_full_text\": true,
                           \"find_related\": false}')
  r <- httr::POST(api_endpoint, body = data)
  httr::stop_for_status(r)         
  jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"), flatten = TRUE)$articles          
}
```
- Set the dates for the search
```{r}
days<-as.character(as.Date(as.Date("2021-05-22"):as.Date("2021-05-31"), origin="1970-01-01"))
```
- Write and run the loop
```{r}
mylist <- list()
for (i in days) {
  print("now running days:")
  print (i)
  start_time <- as.numeric(as.POSIXct(paste(i, "00:00:00 EST", sep=" "))) * 1000
  end_time <- as.numeric(as.POSIXct(paste(as.Date(paste(i))+1,  "00:00:00 EST", sep=" "))) * 1000 - 1
  data_temp <- get_newswhip_articles(api_key = api_key, limit = num_articles, start_time = start_time, end_time = end_time)
  data_temp$date_time <- as.POSIXct(round(data_temp$publication_timestamp/1000), origin="1970-01-01")
  data_temp$date <- as.Date(as.POSIXct(round(data_temp$publication_timestamp/1000), origin="1970-01-01"))
  data_temp$relatedStories <- NULL
  data_temp$topics <- NULL
  data_temp$authors <- NULL
  data_temp$entities <- NULL
  data_temp$videos <- NULL
  try(data_temp<- data_temp %>% dplyr::select(delta_time, 
                                       recent_fb_counts, 
                                       recent_tw_counts, 
                                       predicted_interactions, 
                                       predicted_timestamp, 
                                       uuid, 
                                       publication_timestamp, 
                                       link, 
                                       headline, 
                                       excerpt, 
                                       keywords, 
                                       image_link, 
                                       has_video, 
                                       nw_score, 
                                       max_nw_score, 
                                       fb_data.total_engagement_count, 
                                       fb_data.likes, 
                                       fb_data.comments, 
                                       fb_data.shares, 
                                       fb_data.total_count_delta, 
                                       fb_data.delta_period, 
                                       fb_data.delta_period_unit, 
                                       tw_data.tw_count, 
                                       tw_data.total_count_delta, 
                                       tw_data.delta_period, 
                                       tw_data.delta_period_unit, 
                                       li_data.li_count, 
                                       li_data.total_count_delta, 
                                       li_data.delta_period, 
                                       li_data.delta_period_unit, 
                                       pi_data.pi_count, 
                                       pi_data.delta_period, 
                                       pi_data.delta_period_unit, 
                                       source.publisher, 
                                       source.domain, 
                                       source.link, 
                                       source.country, 
                                       source.country_code, 
                                       source.language, 
                                       date_time, 
                                       date))
  mylist[[i]] <- data_temp
  }
```
- Bind the list to a dataframe and save for backup
```{r}
data_temp_rt <- do.call("rbind",mylist)%>%data.frame()
```
*** Repeat with each state-sponsored media saving it as data_temp_MEDIA ***
2. Combine all the state-sponsored media data-frames
```{r}
mydata_russia<-rbind(data_temp_rt,
                     data_temp_sputnik,
                     data_temp_globalresearch,
                     data_temp_newsfront,
                     data_temp_strategicculture,
                     data_temp_oneworld)
```
3. Clean duplicate UUID and links, sum engagement and add index
```{r}
mydata_russia2<- mydata_russia %>%
  distinct(link,.keep_all = TRUE)%>%
  distinct(uuid,.keep_all = TRUE) %>%
  tibble::rowid_to_column("index") %>%
  rowwise() %>%
  mutate(engagement = sum(fb_data.total_engagement_count, 
                          tw_data.tw_count,
                          li_data.li_count,
                          pi_data.pi_count)) 
mydata_russia2$dateMONTH <- format(as.Date(mydata_russia2$date), "%Y-%m")

save(mydata_russia2, file="Russia_State_Sponsored_Links_DATE.Rda")
```

### US News
Load the packages
```{r}
library(httr)
library(jsonlite)
library(devtools)
library(urltools)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
```

1. Create a function to get popular articles from Newswhip API with search words
- Create an object for maximum number of articles for each day
```{r}
num_articles <- 5000
```
-Create a variable with API key
```{r, echo=FALSE}
api_key <- 'XXXXXXX'
```
- Create API endpoint
```{r}
api_endpoint <- paste0('https://api.newswhip.com/v1/articles?key=', api_key)
```
- Write the function
```{r}
get_newswhip_articles <- function(api_key, limit, start_time, end_time) {
  api_endpoint <- paste0('https://api.newswhip.com/v1/articles?key=', api_key)          
  data <- paste0('{\"filters\": [\"language: en AND country_code:us AND ((ukraine) OR (ukrainian) OR (donetsk) OR (luhansk) OR (lugansk) OR (donbass) OR (crimea) OR (zelensky))\"], 
                           \"size\": ', limit, ', 
                           \"from\": ', start_time, ',
                           \"to\": ', end_time, ',
                           \"sort_by\":\"fb_tw_and_li\",
                           \"search_full_text\": true,
                           \"find_related\": false}')
  r <- httr::POST(api_endpoint, body = data)
  httr::stop_for_status(r)         
  jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"), flatten = TRUE)$articles          
}
```
- Set the dates for the search
```{r}
days<-as.character(as.Date(as.Date("2021-05-22"):as.Date("2021-05-31"), origin="1970-01-01"))
```
- Write and run the loop
```{r}
mylist <- list()
for (i in days) {
  print("now running days:")
  print (i)
  start_time <- as.numeric(as.POSIXct(paste(i, "00:00:00 EST", sep=" "))) * 1000
  end_time <- as.numeric(as.POSIXct(paste(as.Date(paste(i))+1,  "00:00:00 EST", sep=" "))) * 1000 - 1
  data_temp <- get_newswhip_articles(api_key = api_key, limit = num_articles, start_time = start_time, end_time = end_time)
  data_temp$date_time <- as.POSIXct(round(data_temp$publication_timestamp/1000), origin="1970-01-01")
  data_temp$date <- as.Date(as.POSIXct(round(data_temp$publication_timestamp/1000), origin="1970-01-01"))
  data_temp$relatedStories <- NULL
  data_temp$topics <- NULL
  data_temp$authors <- NULL
  data_temp$entities <- NULL
  data_temp$videos <- NULL
  try(data_temp<- data_temp %>% dplyr::select(delta_time, 
                                       recent_fb_counts, 
                                       recent_tw_counts, 
                                       predicted_interactions, 
                                       predicted_timestamp, 
                                       uuid, 
                                       publication_timestamp, 
                                       link, 
                                       headline, 
                                       excerpt, 
                                       keywords, 
                                       image_link, 
                                       has_video, 
                                       nw_score, 
                                       max_nw_score, 
                                       fb_data.total_engagement_count, 
                                       fb_data.likes, 
                                       fb_data.comments, 
                                       fb_data.shares, 
                                       fb_data.total_count_delta, 
                                       fb_data.delta_period, 
                                       fb_data.delta_period_unit, 
                                       tw_data.tw_count, 
                                       tw_data.total_count_delta, 
                                       tw_data.delta_period, 
                                       tw_data.delta_period_unit, 
                                       li_data.li_count, 
                                       li_data.total_count_delta, 
                                       li_data.delta_period, 
                                       li_data.delta_period_unit, 
                                       pi_data.pi_count, 
                                       pi_data.delta_period, 
                                       pi_data.delta_period_unit, 
                                       source.publisher, 
                                       source.domain, 
                                       source.link, 
                                       source.country, 
                                       source.country_code, 
                                       source.language, 
                                       date_time, 
                                       date))
  mylist[[i]] <- data_temp
  }
```
- Bind the list to a dataframe and save for backup
```{r}
data_temp_usa <- do.call("rbind",mylist)%>%data.frame()
```
2. Clean duplicate UUID and links, sum engagement and add index
```{r}
mydata_usa<- data_temp_usa %>%
  distinct(link,.keep_all = TRUE)%>%
  distinct(uuid,.keep_all = TRUE) %>%
  tibble::rowid_to_column("index") %>%
  rowwise() %>%
  mutate(engagement = sum(fb_data.total_engagement_count, 
                          tw_data.tw_count,
                          li_data.li_count,
                          pi_data.pi_count)) 
mydata_usa$dateMONTH <- format(as.Date(mydata_usa$date), "%Y-%m")

save(mydata_usa, file="USA_News_Ukraine_Links_DATE.Rda")
```


