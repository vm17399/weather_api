# get data frames

dtt <- as.data.frame(read_csv("https://raw.githubusercontent.com/vm17399/weather_api/main/api_data_weather.csv"))

pointer <- point %>% select(id, did) %>% mutate(did = as.integer(did)) %>% 
  filter(is.na(did) == FALSE)

didnt <- dist_coord %>% select(name, did)

hobos_did <- full_join(hobos_full, pointer21, by = "id")

hobos_did_avg <- hobos_did %>% group_by(did, dttm) %>%
  summarise(tavg = mean(th), did, dttm) %>% 
  distinct_all() %>% filter(is.na(tavg) == FALSE)

#write.csv(didnt, file = "~/Freiburg/UniFreiburg/Data Management/weather_api/pointer21.csv", row.names = FALSE, quote = FALSE)

#hobos_did_pear <- hobos_did_avg %>% group_by(did) %>% 
# summarise(pears = cor.test(x = dev$th, y = dth$th, model ="pearson"))

didder <- data.frame(did = as.integer(pointer21$did)) %>% 
  distinct_all() %>% 
  filter(did != "integer(0)")

real <- data.frame(pear = NA, did = NA)

dtg <- dtt

j <- 1

while (j < nrow(didder) + 1) {
  
  hda <- hobos_did_avg %>% filter(did == didder[[j,1]])
  
  dtg <- dtt %>% slice(1:nrow(hda))
  
  pearson <- cor.test(x = hda$tavg, y = dtg$th, model ="pearson")
  
  real <- union(real, data.frame(pear = pearson$estimate, 
                                 did =  didder[[j, 1]]))
  j <- j + 1
}

peareal2122 <- merge(real, didnt, by = "did")

#write.csv(peareal21, file = "~/Freiburg/UniFreiburg/Data Management/weather_api/pearson_per_distr_21.csv", row.names = FALSE, quote = FALSE)


