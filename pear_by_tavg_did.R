# get data frames

dtt <- as.data.frame(read_csv("https://raw.githubusercontent.com/vm17399/weather_api/main/api_data_weather.csv"))

pointer <- read_csv("https://raw.githubusercontent.com/vm17399/weather_api/main/pointer22.csv")

#pointer22 <- point %>% filter(term_id == 13) %>% select(id, did) %>% mutate(did = as.integer(did))

hobos_2022 <- read_csv("https://raw.githubusercontent.com/vm17399/weather_api/main/hobos_2022.csv")

hobos_did <- full_join(hobos_2022, pointer, by = "id")

hobos_did_avg <- hobos_did %>% group_by(did, dttm) %>%
  summarise(tavg = mean(th), did, dttm) %>% 
  distinct_all() %>% filter(is.na(tavg) == FALSE)

write.csv(pointer22, file = "~/Freiburg/UniFreiburg/Data Management/weather_api/pointer22.csv", row.names = FALSE, quote = FALSE)



#hobos_did_pear <- hobos_did_avg %>% group_by(did) %>% 
 # summarise(pears = cor.test(x = dev$th, y = dth$th, model ="pearson"))

didder <- data.frame(did = as.integer(pointer$did)) %>% 
  distinct_all() %>% 
  filter(did != "integer(0)")

real <- data.frame(pear = NA, did = NA)

dtg <- dtt

j <- 1

while (j < nrow(didder)) {
 
  hda <- hobos_did_avg %>% filter(did == didder[[j,1]]) %>% slice(c(1:648))
  
  pearson <- cor.test(x = hda$tavg, y = dtg$th, model ="pearson")
  
  real <- union(real, data.frame(pear = pearson$estimate, 
                                 did =  didder[[j, 1]]))
  j <- j + 1
}
