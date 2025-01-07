# abrindo o diretorio de trabalho

setwd ("C:/Users/Iohara Quirino/OneDrive/Área de Trabalho/figuras_goal1") #PC pessoal
setwd("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/results") #PC trabalho

# Load your data

data_mackerel <- read.csv("data_mackerel.csv")

install.packages(c("ggplot2", "sf", "ggspatial", "rnaturalearth", "rnaturalearthdata"))

install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("sf")

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggplot2)
library(ggspatial)
library(dplyr)

world <- ne_countries(scale = "medium", returnclass = "sf") #mapa mundo

europe <- world[world$continent == "Europe", ] #mapa europa

uk_neighbors <- world[world$name %in% c("United Kingdom", "Ireland", "France", 
                                        "Belgium", "Netherlands", "Germany"), ] #apenas UK e vizinhos


data_sf <- st_as_sf(data_novmackerel, coords = c("lon", "lat"), crs = 4326)

bbox <- st_bbox(data_sf)

# Filtrar os dados para o intervalo de anos desejado
data_filtered <- data_sf %>% 
  filter(year >= 2019 & year <= 2023)


ggplot(data = uk_neighbors) +
  geom_sf() +  # Mapa base
  geom_sf(data = data_filtered, aes(color = as.factor(year)), size = 0.5) +  # Pontos com cores por ano
  scale_color_viridis_d() +  # Escala de cores automáticas
  theme_minimal() +  # Tema limpo
  ggtitle("Mackerel Towing Tracks 2019-2023") +
  labs(color = "Year") +
  annotation_scale(location = "bl") +  # Escala
  annotation_north_arrow(location = "bl", style = north_arrow_fancy_orienteering()) +  # Setas de norte
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]))  # Ajuste automático dos limites com a bounding box





#### Adding analysis of the map per vessel to understand possibles outliers #### 

world <- ne_countries(scale = "medium", returnclass = "sf") #mapa mundo

europe <- world[world$continent == "Europe", ] #mapa europa

uk_neighbors <- world[world$name %in% c("United Kingdom", "Ireland", "France", 
                                        "Belgium", "Netherlands", "Germany"), ] #apenas UK e vizinhos


data_sf <- st_as_sf(data_mackerel, coords = c("lon", "lat"), crs = 4326)

bbox <- st_bbox(data_sf)

# Filtrar os dados para o intervalo de anos desejado
data_filtered <- data_sf %>% 
  filter(year >= 2019 & year <= 2023) 

#OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOU

#Para ano especifico, use:
data_filtered <- data_sf %>% 
  filter(year == 2022)


# Filtrar os dados para os vessels desejados
data_filtered <- data_filtered %>%
  filter(VE_ID %in% c(2, 8, 9, 11)) # mais de um vessel

#OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOU

# Filtrar os dados para apenas UM vessel
data_filtered <- data_filtered %>%
  filter(VE_ID == 22) 


ggplot(data = uk_neighbors) +
  geom_sf() +  # Base map
  geom_sf(data = data_filtered, aes(color = as.factor(year)), size = 0.5) +  # Points colored by year
  scale_color_viridis_d() +  # Color scale
  theme_minimal() +  # Clean them
  ggtitle("Mackerel Towing Tracks 2022 by Vessel 22") +
  labs(color = "Year") +
  annotation_scale(location = "bl") +  # Scale bar
  annotation_north_arrow(location = "bl", style = north_arrow_fancy_orienteering()) +  # North arrow
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4])) +  # Zoom to data
  facet_wrap(~ VE_ID)  # Facet by vessel


















