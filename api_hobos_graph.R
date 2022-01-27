# ---- API + HOBOs graph ----

# libraries

library(RPostgres)
library(tidyverse)
library(lubridate)

# connect to db

con <- dbConnect(RPostgres::Postgres(), 
                  "hydenv", host = "localhost",
                  port = 5432, 
                  user = "hydenv", 
                  password = "hydenv")

# Importing tables from Postgres

hobos_data <- dbReadTable(con, "data") %>% # quality checked data
  filter(tstamp >= "2021-12-13 00:00:00") %>%
  rename(dttm = tstamp, th = value)

save(hobos_data, file = "~/Freiburg/UniFreiburg/Data Management/SQL/hobo_21_22_data.rda")

hobos_md <- dbReadTable(con, "metadata") %>% 
  filter(id >= 37, id <= 67)

hobos_id <- hobos_md %>% select(id, device_id)

dtl <- as.data.frame(read_csv("https://raw.githubusercontent.com/vm17399/weather_api/main/api_data_weather.csv"))

# joining df

h_data <- merge(hobos_data, hobos_id, by.x = c("meta_id"), by.y = c("id")) %>% 
  select(-variable_id) %>% select(-meta_id, -quality_flag_id)

dth <- dtl %>%  mutate(device_id = "api")

hobo_api <- union(h_data, dth) 

hobo_api <- merge(hobo_api, dth, by = "dttm") %>% select(-device_id.y)

names(hobo_api) <- c("dttm", "th", "device_id", "temp")

# plotting

cols <- c("OpenWeather" = "red", "HOBOs" = "grey")

g0 <- ggplot(hobo_api, aes(dttm)) +
  geom_line(aes(y = th), color = "grey") +
  geom_line(aes(y = temp, color = "OpenWeather"), size = 0.7) +
  labs(x = 'Date', y = 'Temperature', color = 'Legend') + 
  scale_color_manual(values = cols) +
  theme_minimal(14) +
  theme(legend.position = c(0.3, 0.85),
        legend.background = element_rect(fill = "white", color = "grey"),
        legend.key.size = unit(3, "line"))
  
