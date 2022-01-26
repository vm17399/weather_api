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

# filter for Wt22

#%>% 
 # filter(term == "WT22")

g1 <- ggplot() +
  geom_sf(data = distr1, colour = "white", fill = "grey70") +
  geom_sf(data = hmdt$coord, aes(fill = hmdt$pear, shape = hmdt$term), size = 4, alpha= 0.8) + 
  theme_minimal(14) +
  scale_shape_manual(values = c(21, 24), name = "Term") +
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

# ---- pearson average mapped

colorz <- merge(pearper, districts, by = "name")

colorz <- merge(colorz, distr1, by = "id") 

g2 <- ggplot(colorz$x) +
  geom_sf(data = distr1, colour = "white", fill = "grey70") +
  geom_sf(aes(fill = colorz$p_avg)) +
  geom_sf(data = hmdt$coord, aes(fill = hmdt$pear), size = 3, alpha= 0.8) +
  scale_fill_viridis_b(option = "plasma",
                       name = "P. average",
                       n.breaks = 5) + 
  ggtitle("Pearson correlation per district for 2021 and 2022") +
  theme(legend.key.size = unit(0.3, "line"),
        legend.position = c(0.1,0.18),
        legend.background = element_rect(fill = "white", colour = "grey")) 
