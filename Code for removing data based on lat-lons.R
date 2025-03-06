
# Cleaning #

# Map with all data

# abrindo o diretorio de trabalho
setwd ("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/results/Towing_data/data_per_vessels") #PC trabalho


# Load your data

towing_v10 <- read.csv("mackerel_towing_data10.csv")

# Necessary packages 

library(maps)
library(mapdata)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggrepel)
library(viridis)


# Mapa do ano todo ou vessel #


world <- ne_countries(scale = "medium", returnclass = "sf") # Mapa do mundo; filtrar países relevantes

europe <- world[world$continent == "Europe", ] # Mapa da Europa

uk_neighbors <- world[world$name %in% c("United Kingdom", "Ireland", "France", 
                                        "Belgium", "Netherlands", "Germany", "Norway"), ] # Inclui Noruega


data_sf <- st_as_sf(mackerel_towing_data_2004, coords = c("lon", "lat"), crs = 4326) # Converter os dados de haul para sf


bbox_data <- st_bbox(data_sf) # Calcular a bounding box dos dados de haul para ajustar os limites do mapa

# Define your desired coastal area (adjust these numbers as needed)
norway_coast_bbox <- c(xmin = 4, xmax = 6, ymin = 58, ymax = 63)

# Create a union bounding box that covers both the data and your desired coastal region
final_bbox <- c(
  xmin = min(bbox_data["xmin"], norway_coast_bbox["xmin"]),
  xmax = max(bbox_data["xmax"], norway_coast_bbox["xmax"]),
  ymin = min(bbox_data["ymin"], norway_coast_bbox["ymin"]),
  ymax = max(bbox_data["ymax"], norway_coast_bbox["ymax"])
)


plot(st_geometry(data_sf), col = "blue", pch = 20, main = "Distribution - 2004") # CHECKING DATA DISTRIBUTION


# Mapa Plotter já com o shapefile do ICES


# Set working directory for the ICES shapefile (use the correct one for your machine)
setwd("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/chapter2/ICES_areas")  # PC trabalho


ICES_Areas<- st_read("ICES_Areas_20160601_cut_dense_3857.shp")

head(ICES_Areas)
summary(ICES_Areas)
st_crs(ICES_Areas)  # Check coordinate system
ICES_Areas <- st_make_valid(ICES_Areas) 


# Transformar data_sf em um objeto sf usando as colunas de longitude e latitude
data_sf <- st_as_sf(mackerel_towing_data_2004, coords = c("lon", "lat"), crs = 4326) 

data_filtered <- data_sf


#Mapa

ggplot() +
  geom_sf(data = uk_neighbors, fill = "aliceblue", color = "darkblue", linewidth = 0.3) +
  geom_sf(data = ICES_Areas, color = "darkblue", fill = NA, linewidth = 0.2, linetype = "dashed") +
  geom_sf(data = data_filtered, aes(color = as.factor(month)), size = 0.5, alpha = 0.8) +
  scale_color_manual(values = c("red", "orange", "purple", "yellow","magenta", "green", "lightsalmon3", "indianred3", "plum4"), name = "Month") +
  annotation_scale(location = "bl", style = "ticks", text_cex = 0.8) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +
  ggtitle("Towings - 2004") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightblue", color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(size = 10)
  ) +
  coord_sf(xlim = c(final_bbox["xmin"], final_bbox["xmax"]),
           ylim = c(final_bbox["ymin"], final_bbox["ymax"]))



#Subset data based on lat lon (to clean)

#por ano apenas
mackerel_1 <- subset(mackerel_towing_data_2004, mackerel_towing_data_2004$lon>2.5 & mackerel_towing_data_2004$lon<7 & mackerel_towing_data_2004$lat<61 & mackerel_towing_data_2004$lat>58)

#por vessel + ano especifico
v10_2017 <- towing_v10 %>%
  filter(year == 2017 & lon > 0 & lon < 5 & lat > 59 & lat < 62)

head(v10_2017)

v10_2017$month #check the months

# Converter o subset `mackerel_2` para sf
data_sf <- st_as_sf(v10_2017, coords = c("lon", "lat"), crs = 4326)

# Calcular a bounding box para que o mapa foque apenas no subset
bbox_filtered <- st_bbox(data_sf)

# Criar o mapa com o subset
ggplot() +
  geom_sf(data = uk_neighbors, fill = "aliceblue", color = "darkblue", linewidth = 0.3) +
  geom_sf(data = ICES_Areas, color = "darkblue", fill = NA, linewidth = 0.2, linetype = "dashed") +
  geom_sf(data = data_sf, aes(color = as.factor(month)), size = 1.5, alpha = 0.5) +  # Apenas os dados do subset
  scale_color_manual(values = c("yellow2", "black", "red"), name = "Month") +
  annotation_scale(location = "bl", style = "ticks", text_cex = 0.8) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +
  ggtitle("Towings Vessel 10 - 2017 - Subset 1 by Lon and Lat") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightblue", color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(size = 10)
  ) +
  coord_sf(xlim = c(bbox_filtered["xmin"], bbox_filtered["xmax"]),
           ylim = c(bbox_filtered["ymin"], bbox_filtered["ymax"]))  # Ajuste automático do zoom para o subset


write.csv(v10_2017, "v10_2017_subset1.csv", row.names = FALSE)


#Entirely map without subsets


world <- ne_countries(scale = "medium", returnclass = "sf") # Mapa do mundo; filtrar países relevantes

europe <- world[world$continent == "Europe", ] # Mapa da Europa

uk_neighbors <- world[world$name %in% c("United Kingdom", "Ireland", "France", 
                                        "Belgium", "Netherlands", "Germany", "Norway"), ] # Inclui Noruega


towing_v10_2017 <- towing_v10 %>%
  filter(year == 2017) # 


data_sf_all <- st_as_sf(towing_v10_2017, coords = c("lon", "lat"), crs = 4326) # Converter os dados de haul para sf


bbox_data <- st_bbox(data_sf_all) # Calcular a bounding box dos dados de haul para ajustar os limites do mapa

# Define your desired coastal area (adjust these numbers as needed)
norway_coast_bbox <- c(xmin = 4, xmax = 6, ymin = 58, ymax = 63)

# Create a union bounding box that covers both the data and your desired coastal region
final_bbox <- c(
  xmin = min(bbox_data["xmin"], norway_coast_bbox["xmin"]),
  xmax = max(bbox_data["xmax"], norway_coast_bbox["xmax"]),
  ymin = min(bbox_data["ymin"], norway_coast_bbox["ymin"]),
  ymax = max(bbox_data["ymax"], norway_coast_bbox["ymax"])
)


mackerel_3 <- mackerel_towing_data_2004 %>%
  filter(! ((lon > 2.5 & lon < 7 & lat < 61 & lat > 58) |
           lon < 1 & lon > -2 & lat < 59.5 & lat > 58) ) # To filter more than 1 subset without filtering years


v10_2017 <- towing_v10_2017 %>%
  filter(!(lon > 0 & lon < 5 & lat > 59 & lat < 62)) # To filter just a subset filtering years



# Converter o subset `mackerel_2` para sf
data_sf <- st_as_sf(v10_2017, coords = c("lon", "lat"), crs = 4326)

# Criar o mapa com o subset
ggplot() +
  geom_sf(data = uk_neighbors, fill = "aliceblue", color = "darkblue", linewidth = 0.3) +
  geom_sf(data = ICES_Areas, color = "darkblue", fill = NA, linewidth = 0.2, linetype = "dashed") +
  geom_sf(data = data_sf, aes(color = as.factor(month)), size = 1.5, alpha = 0.5) +  # Apenas os dados do subset
  scale_color_manual(values = c("red", "orange", "purple", "yellow","magenta", "green", "lightsalmon3", "indianred3", "plum4"), name = "Month") +
  annotation_scale(location = "bl", style = "ticks", text_cex = 0.8) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +
  ggtitle("Towings vessel 10 - 2017 - Without subset 1") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightblue", color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(size = 10)
  ) +
  coord_sf(xlim = c(final_bbox["xmin"], final_bbox["xmax"]),
           ylim = c(final_bbox["ymin"], final_bbox["ymax"]))


write.csv(v10_2017, "v_10_withoutSubsets.csv", row.names = FALSE) #Para salvar



















