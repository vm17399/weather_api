# creates a data frame dtt and a file .csv of the .json data

#---- libraries ----

library(jsonlite)
library(dplyr)
library(tidyverse)
library(lubridate)

#---- set up url ----

url1 <- as.character("https://raw.githubusercontent.com/data-hydenv/data/master/extra/weather/data/2021_12_")

num1 <- as.character("11")

url2 <- as.character("_raw_dump.json")

url3 <- as.character("https://raw.githubusercontent.com/data-hydenv/data/master/extra/weather/data/2022_1_")

urlt <- paste(url1, num1, url2, sep= "")
urlt

# for january

num2 <- as.character("1")

# ---- empty data frame

dtt <- data.frame(dttm =as.Date(character()), th = as.double(double())) 

##---- while machine ----

while (as.integer(num1) < 43) {
  
  #---- json importer  ----  
  
  js <- fromJSON(urlt, flatten =TRUE)
  
  jdf <- js$historic$hourly %>% as.data.frame()
  
  dfor <- jdf %>% select(dt, temp)%>% 
    mutate(th = temp) %>% 
    mutate(dttm = as.POSIXct(dt, origin ="1970-01-01")) %>% 
    select(dttm, th)
  
  #---- create data frame ----
  
  dtt <- union(dtt, dfor)
  
  dtt_check <- dtt %>% mutate(year = year(dttm), month = month(dttm), day = day(dttm))
  
  #---- names and url update ----
  
  ifelse(num1 < 31, #condition yes = december
         
         {
           
           num1 <- ifelse(num1 == 26, as.integer(num1) + 2, as.integer(num1) + 1)
           
           urlt <- paste(url1, as.character(num1), url2, sep= "")}, # yes
         
         { # no = january
           
           num1 <- as.integer(num1) + 1
           
           urlt <- paste(url3, num2, url2, sep= "")
           
           num2 <- as.integer(num2) + 1} # no
         
  )}

dtt <- dtt %>% filter(dttm > "2021-12-12 23:00:00", dttm < "2022-01-10") %>% 
  mutate(th = th -273.15)

#write.csv(dtt, file = '~/Freiburg/UniFreiburg/Data Management/weather_api/api_data_weather.csv', row.names = FALSE, quote = FALSE)
