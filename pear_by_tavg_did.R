# get data frames

dtt <- as.data.frame(read_csv("https://raw.githubusercontent.com/vm17399/weather_api/main/api_data_weather.csv"))

pointer <- read_csv("https://raw.githubusercontent.com/vm17399/weather_api/main/pointer.csv")

hobos_did <- full_join(hobos_full, pointer, by = "id")

hobos_did_avg <- hobos_did %>% group_by(did, dttm) %>%
  summarise(tavg = mean(th), did, dttm) %>% 
  distinct_all()

write.csv(pointer, file = "~/Freiburg/UniFreiburg/Data Management/weather_api/pointer.csv", row.names = FALSE, quote = FALSE)

#hobos_did_pear <- hobos_did_avg %>% group_by(did) %>% 
 # summarise(pears = cor.test(x = dev$th, y = dth$th, model ="pearson"))

didder <- data.frame(did = as.integer(point$did)) %>% distinct_all() %>% filter(did != "integer(0)")

real <- data.frame(pear = NA, did = NA)

j <- 1

while (j < nrow(didder)) {
  
  hda <- hobos_did_avg %>% filter(did == didder[[j,1]]) 
  
  if(i == 1) {
    
    dtg <- dtt %>% slice(1:nrow(hda))
  } else {
    
    dtg <- dtt %>% slice(1:nrow(hda))
  }
  
  pearson <- cor.test(x = hda$th, y = dtg$th, model ="pearson")
  
  real <- union(real, data.frame(pear = pearson$estimate, 
                                 did =  didder[[j, 1]]))
  j <- j + 1
}
