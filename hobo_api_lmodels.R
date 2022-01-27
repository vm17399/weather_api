# importing

hobos_2021 <- read_csv("https://raw.githubusercontent.com/vm17399/weather_api/main/hobos_2021.csv")

hobos_2022 <- read_csv("https://raw.githubusercontent.com/vm17399/weather_api/main/hobos_2022.csv")

pointer <- read_csv("https://raw.githubusercontent.com/vm17399/weather_api/main/pointer22.csv")

dtt <- as.data.frame(read_csv("https://raw.githubusercontent.com/vm17399/weather_api/main/api_data_weather.csv"))

# unified df

hobos_full <- union(hobos_2021, hobos_2022)

hobos_didt <- full_join(hobos_full, pointer, by = "id")

model_data <- hobos_didt %>% group_by(did, dttm) %>%
  summarise(tavg = mean(th), did, dttm) %>% 
  distinct_all() %>% filter(is.na(tavg) == FALSE)

pointer_n <- merge(pointer, dist_coord, by ="did") %>% select(did, name, id.x)

pn22 <- pointer_n %>% filter(id.x >= 36) %>% distinct(did, name)

pn21 <- pointer_n %>% filter(id.x < 36)

c <- 1

while (c < nrow(pn22)) {
  
  pn21 <- pn21 %>% filter(did != pn22[[c,1]]) 
  
  c <- c + 1
  
}

pn21 <- pn21 %>% distinct(did, name)

pntot <- union(pn21, pn22)

k <- 1

while (k < nrow(pntot) + 1) {
  
md <- model_data %>% filter(did == pntot[[k, 1]]) 

if (nrow(md) > 800) {md <- md %>% filter(dttm > "2021-01-01")} else {md <- md}

dtk <- dtt %>% slice(1:nrow(md))

model <- lm(md$tavg ~ dtk$th)

assign(paste0("lm_", pntot[[k,2]]), model)

k <- k + 1

}
