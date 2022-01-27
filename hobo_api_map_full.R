# ---- mapping and HOBO locations ----

# libraries

library(RPostgres)
library(tidyverse)
library(sf)

# connect to db

con <- dbConnect(RPostgres::Postgres(), 
                 "hydenv", host = "localhost",
                 port = 5432, 
                 user = "hydenv", 
                 password = "hydenv")

# read .csv and maps, metadata

h21 <- read.csv("https://raw.githubusercontent.com/vm17399/weather_api/main/cor_hobos_2021.csv")

h22<- read.csv("https://raw.githubusercontent.com/vm17399/weather_api/main/cor_hobos_2122.csv")

hwt <- union(h21, h22) %>% rename(id = meta_id)

districts <- dbReadTable(con, "osm_nodes")

hmd <- dbReadTable(con, "metadata") %>% select(-description)

# we have two pq_geometries being in hmd and in districts, we must convert them

distr <- st_as_sfc(districts$geom)

distr1 <- sf::st_as_sf(distr) %>% st_set_crs("WGS84")

distr1 <- distr1 %>% mutate(id = districts$id)

hmd <- hmd %>%  mutate(coord = st_as_sf(sf::st_as_sfc(hmd$location))) 

hmd$coord %>% st_set_crs("WGS84")

hmdt <- merge(hmd, hwt, by = "id")%>% 
  mutate(term = ifelse(term_id == 11, "WT21", "WT22")) 

hmd22 <- hmdt %>% filter(term == "WT22")

# filter for Wt22

#%>% 
# filter(term == "WT22")

g1 <- ggplot() +
  geom_sf(data = distr1, colour = "white", fill = "grey70") +
  geom_sf(data = hmdt$coord, aes(fill = hmdt$pear, shape = hmdt$term), size = 3, alpha= 0.8) + 
  theme_minimal(14) +
  scale_shape_manual(values = c(24, 21), name = "Term") +
  scale_fill_viridis_b(option = "plasma",
                       name = "Pearson correlation",
                       n.breaks = 5) +
  ggtitle("HOBOs in Freiburg") +
  theme(legend.key.size = unit(0.3, "line"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "white", colour = "grey"))

# ---- hobos by district ----

dist_coord <- districts %>% mutate(coord = st_as_sf(sf::st_as_sfc(geom))) %>% 
  mutate(did = c(1:28))

dist_coord$coord %>% st_set_crs("WGS84")

dist_coord$coord %>% st_within(hmdt$coord[[1]])

points <- hmdt$coord %>% st_within(dist_coord$coord)

with <- st_within(hmdt$coord, dist_coord$coord)

point <- hmdt %>%  
  mutate(did = with) 

hobo_dist <- point %>% select(-location, -device_id.y, -sensor_id, -term_id)

hobo_dist$did <- hobo_dist$did %>% as.integer()

hobo_districts <- inner_join(hobo_dist, dist_coord, by = "did") 

hobo_but <- hobo_districts %>% 
  select(id.x, device_id.x, pear, name) %>% 
  rename(id = id.x, device_id = device_id.x)

distribution <- hobo_but %>% select(name) %>% group_by(name) %>% count()

pearper <- hobo_but %>% group_by(name) %>% 
  summarise(p_avg = mean(pear),
            name) %>% distinct_all()

pearper22 <- hobo_but %>% filter(id > 36) %>% 
  group_by(name) %>% 
  summarise(p_avg = mean(pear),
            name) %>% distinct_all()



# ---- pearson average mapped tot

colorz <- merge(pearper, districts, by = "name")

colorz <- merge(colorz, distr1, by = "id") 

g2 <- ggplot(colorz$x) +
  geom_sf(data = distr1, colour = "white", fill = "grey70") +
  geom_sf(aes(fill = colorz$p_avg)) +
  geom_sf(data = hmdt$coord, aes(shape = hmdt$term), size = 3, alpha= 0.8) +
  scale_fill_viridis_b(option = "plasma",
                       name = "P. average",
                       n.breaks = 5) + 
  ggtitle("Pearson correlation per district for 2021 and 2022") +
  theme(legend.key.size = unit(0.55, "line"),
        legend.position = c(0.1,0.18),
        legend.background = element_rect(fill = "white", colour = "grey")) 

# ---- pearson 22 ----

colorz1 <- merge(pearper22, districts, by = "name") %>% slice(1:11)

colorz1 <- left_join(colorz1, distr1, by = "id") 

g3 <- ggplot(colorz1$x) +
  geom_sf(data = distr1, colour = "white", fill = "grey70") +
  geom_sf(aes(fill = colorz1$p_avg)) +
  geom_sf(data = hmd22$coord, size = 3, alpha= 0.8) +
  scale_fill_viridis_c(option = "plasma",
                       name = "P. average",
                       n.breaks = 4) + 
  ggtitle("Pearson correlation per district 2022") +
  theme(legend.key.size = unit(0.55, "line"),
        legend.position = c(0.1,0.18),
        legend.background = element_rect(fill = "white", colour = "grey")) 

#---- by tavg 21 ----

# get data frames

dtt <- as.data.frame(read_csv("https://raw.githubusercontent.com/vm17399/weather_api/main/api_data_weather.csv"))

hobos_2021 <- read_csv("https://raw.githubusercontent.com/vm17399/weather_api/main/hobos_2021.csv")

pointer21 <- point %>% filter(term_id == 11) %>% select(id, did) %>% 
  mutate(did = as.integer(did)) %>% 
  filter(is.na(did) == FALSE)

didnt <- dist_coord %>% select(name, did)

hobos_did <- full_join(hobos_2021, pointer21, by = "id")

hobos_did_avg21 <- hobos_did %>% group_by(did, dttm) %>%
  summarise(tavg = mean(th), did, dttm) %>% 
  distinct_all() %>% filter(is.na(tavg) == FALSE)

didder <- data.frame(did = as.integer(pointer21$did)) %>% 
  distinct_all() %>% 
  filter(did != "integer(0)")

real <- data.frame(pear = NA, did = NA)

dtg <- dtt

j <- 1

while (j < nrow(didder) + 1) {
  
  hda <- hobos_did_avg21 %>% filter(did == didder[[j,1]])
  
  dtg <- dtt %>% slice(1:nrow(hda))
  
  pearson <- cor.test(x = hda$tavg, y = dtg$th, model ="pearson")
  
  real <- union(real, data.frame(pear = pearson$estimate, 
                                 did =  didder[[j, 1]]))
  j <- j + 1
}

peareal21 <- merge(real, didnt, by = "did")

#write.csv(peareal21, file = "~/Freiburg/UniFreiburg/Data Management/weather_api/pearson_per_distr_21.csv", row.names = FALSE, quote = FALSE)


#---- by tavg22 ----

# get data frames

pointer <- read_csv("https://raw.githubusercontent.com/vm17399/weather_api/main/pointer22.csv")

pointer22 <- point %>% filter(term_id == 13) %>% select(id, did) %>% mutate(did = as.integer(did)) %>% 
  filter(is.na(did) == FALSE) %>% distinct(did)

hobos_2022 <- read_csv("https://raw.githubusercontent.com/vm17399/weather_api/main/hobos_2022.csv")

#didnt <- read_csv("https://raw.githubusercontent.com/vm17399/weather_api/main/didnt.csv")

hobos_did <- full_join(hobos_2022, pointer, by = "id")

hobos_did_avg <- hobos_did %>% group_by(did, dttm) %>%
  summarise(tavg = mean(th), did, dttm) %>% 
  distinct_all() %>% filter(is.na(tavg) == FALSE)

hobos_did_avg22 <- hobos_did %>% group_by(did, dttm) %>%
  summarise(tavg = mean(th), did, dttm) %>% 
  distinct_all() %>% filter(is.na(tavg) == FALSE)

didnt <- dist_coord %>% select(name, did)

write.csv(didnt, file = "~/Freiburg/UniFreiburg/Data Management/weather_api/didnt.csv", row.names = FALSE, quote = FALSE)

#hobos_did_pear <- hobos_did_avg %>% group_by(did) %>% 
# summarise(pears = cor.test(x = dev$th, y = dth$th, model ="pearson"))

didder <- data.frame(did = as.integer(pointer$did)) %>% 
  distinct_all() %>% 
  filter(did != "integer(0)")

real <- data.frame(pear = NA, did = NA)

dtg <- dtt

j <- 1

while (j < nrow(didder) + 1) {
  
  hda <- hobos_did_avg22 %>% filter(did == didder[[j,1]]) %>% slice(c(1:648))
  
  pearson <- cor.test(x = hda$tavg, y = dtg$th, model ="pearson")
  
  real <- union(real, data.frame(pear = pearson$estimate, 
                                 did =  didder[[j, 1]]))
  j <- j + 1
}

peareal <- merge(real, didnt, by = "did")

#write.csv(peareal, file = "~/Freiburg/UniFreiburg/Data Management/weather_api/pearson_.csv", row.names = FALSE, quote = FALSE)

# ---- peareal 22 ----

colorz2 <- merge(peareal, districts, by = "name") %>% slice(1:11)

colorz2 <- left_join(colorz2, distr1, by = "id") 

g4 <- ggplot(colorz2$x) +
  geom_sf(data = distr1, colour = "white", fill = "grey70") +
  geom_sf(aes(fill = colorz2$pear)) +
  geom_sf(data = hmd22$coord, size = 3, alpha= 0.8) +
  scale_shape_manual(values = c(21)) +
  scale_fill_viridis_c(option = "plasma",
                       name = "P. average",
                       n.breaks = 3) + 
  ggtitle("Pearson correlation per district 2022") +
  theme(legend.key.size = unit(0.55, "line"),
        legend.position = c(0.1,0.18),
        legend.background = element_rect(fill = "white", colour = "grey")) 

# ---- peartot ----

peartot <- union(peareal, peareal21)

colorz3 <- merge(peartot, districts, by = "name")

colorz3 <- left_join(colorz3, distr1, by = "id") 

g5 <- ggplot(colorz3$x) +
  geom_sf(data = distr1, colour = "white", fill = "grey70") +
  geom_sf(aes(fill = colorz3$pear)) +
  geom_sf(data = hmdt$coord, aes(shape = hmdt$term), fill = "black",size = 3, alpha= 0.8) +
  scale_shape_manual(values = c(24, 21), name = "Term") +
  scale_fill_viridis_c(name = "P. average",
                       n.breaks = 5) + 
  ggtitle("Pearson correlation per district for 2021 and 2022") +
  theme(legend.key.size = unit(0.55, "line"),
        legend.position = c(0.1,0.18),
        legend.background = element_rect(fill = "white", colour = "grey")) 
