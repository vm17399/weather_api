# ---- API + HOBOs join ----

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

# Importing tables from Postgres and api

hobos_tot <- dbReadTable(con, "data") %>% # quality checked data tot
  rename(dttm = tstamp, th = value, id = meta_id)

hobos_id_tot <- dbReadTable(con, "metadata") %>% 
  select(id, device_id)

dtt <- read.csv("~/Freiburg/UniFreiburg/Data Management/Hobo project/1_data_processed/api_data_weather.csv")

# merging with id metadata frame

hobos_full <- merge(hobos_tot, hobos_id_tot, by = "id") 

# split the df in two years

hobos_2021 <- hobos_full %>% filter(dttm < "2021-5-1")

hobos_2122 <- hobos_full %>% filter(dttm > "2021-12-13 00:00:00",
                                    dttm < "2021-12-26 01:00:00" | 
                                    dttm > "2021-12-27 00:00:00")

cyc <- data.frame(name = c("hobos_2021","hobos_2122"))

i <- 1

while (i < nrow(cyc) + 1) {

  # ---- HOBO and API model comparison ----
  
  nam <- cyc[[i, 1]]

  curr <- eval(parse(text = nam))

  # get the names
  
  h_names <- data.frame(device_id = (curr$device_id), id = curr$id) %>% distinct_all()
  
  # empty data frame and counter
  
  rel <- data.frame(device_id = NA, pear = NA, cov = NA, meta_id = NA)
  
  dth <- dtt
  
  count <- 1
  
  while (count < (nrow(h_names) + 1)) {
    
    filtr <- h_names[[count, 1]]
    
    # filter by device
    
    dev <- curr %>% filter(device_id == filtr) 
    
    if(i == 1) {
      
      dth <- dtt %>% slice(1:nrow(dev))
    } else {
      
      dth <- dtt %>% slice(1:nrow(dev))
    }
    
    cova <- cov(x = dev$th, y = dth$th)
    
    pears <- cor.test(x = dev$th, y = dth$th, model ="pearson")
    
    rel <- union(rel, data.frame(pear = pears$estimate, device_id =  h_names[[count, 1]], cov = cova, meta_id =  h_names[[count, 2]])) 
    
    count <- count + 1
    
  }
  
  rel <- as.data.frame(rel) %>%  drop_na()
  
  assign(paste0("corr_", cyc[[i, 1]]), rel)
  
  i <- i + 1
  
}

write.csv(corr_hobos_2021, file = "~/Freiburg/UniFreiburg/Data Management/Hobo project/1_data_processed/hobos_2021.csv", row.names = FALSE, quote = FALSE)

write.csv(corr_hobos_2122, file = "~/Freiburg/UniFreiburg/Data Management/Hobo project/1_data_processed/hobos_2122.csv", row.names = FALSE, quote = FALSE)
