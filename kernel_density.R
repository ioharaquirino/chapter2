

# abrindo o diretorio de trabalho para Plotter Data
setwd ("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/results")

# Load Plotter Data 
mackerel_towing_2019 <- read.csv("mackerel_towing_data2019.csv")


# abrindo o diretorio de trabalho Self-sampling data
setwd ("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/chapter2/self_sampling_data") # Plotter Data diretorio

# Load Self-sampling data 
mackerel_hauls_2019 <- read.csv("full_2019_selfsmpl.csv")



# 📌 Carregar pacotes
library(ggplot2)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(MASS)


# 📌 Converter para sf (sistema de coordenadas geográficas)
haul_sf <- st_as_sf(mackerel_hauls_2019, coords = c("londd", "latdd"), crs = 4326)
towing_sf <- st_as_sf(mackerel_towing_2019, coords = c("lon", "lat"), crs = 4326)

# 📌 Carregar mapa base (países relevantes)
world <- ne_countries(scale = "medium", returnclass = "sf")
uk_neighbors <- world[world$name %in% c("United Kingdom", "Ireland", "France", 
                                        "Belgium", "Netherlands", "Germany", "Norway"), ]

# 📌 Carregar shapefile do ICES
setwd("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/chapter2/ICES_areas")
ICES_Areas <- st_read("ICES_Areas_20160601_cut_dense_3857.shp")
ICES_Areas <- st_make_valid(ICES_Areas)  # Corrige possíveis problemas geométricos

# 📌 Criar bounding box para ajustar limites do mapa
bbox_data <- st_bbox(towing_sf)
final_bbox <- c(xmin = bbox_data["xmin"], xmax = bbox_data["xmax"], 
                ymin = bbox_data["ymin"], ymax = bbox_data["ymax"])

# 📌 Criar Kernel Density Estimation (KDE) para hauls e towings
haul_kde <- kde2d(mackerel_hauls_2019$londd, mackerel_hauls_2019$latdd, n = 200)
plotter_kde <- kde2d(mackerel_towing_2019$lon, mackerel_towing_2019$lat, n = 200)

# 📌 Transformar KDE em dataframe para plotagem
haul_kde_df <- expand.grid(x = haul_kde$x, y = haul_kde$y)
haul_kde_df$value <- as.vector(haul_kde$z)

plotter_kde_df <- expand.grid(x = plotter_kde$x, y = plotter_kde$y)
plotter_kde_df$value <- as.vector(plotter_kde$z)

# 📌 Criar o mapa com sobreposição de densidade e pontos individuais
ggplot() +
  # 🔹 Adiciona mapa base com países
  geom_sf(data = uk_neighbors, fill = "aliceblue", color = "darkblue", linewidth = 0.01) +
  
  # 🔹 Adiciona shapefile do ICES
  geom_sf(data = ICES_Areas, color = "darkblue", fill = NA, linewidth = 0.01, linetype = "dashed") +
  
  # 🔹 Adiciona densidade KDE do Self-sampling (hauls) em azul usando geom_raster()
  geom_raster(data = haul_kde_df, aes(x = x, y = y, fill = value), alpha = 0.5) +
  scale_fill_gradient(low = "lightblue", high = "blue", name = "Hauls Density") +
  
  # 🔹 Adiciona densidade KDE do Plotter Data (towings) em vermelho
  geom_raster(data = plotter_kde_df, aes(x = x, y = y, fill = value), alpha = 0.5) +
  scale_fill_gradient(low = "pink", high = "red", name = "Towings Density") +
  
  # 🔹 Adiciona os pontos individuais dos hauls (vermelho) e towings (roxo)
  geom_sf(data = towing_sf, color = "purple3", shape = 17, size = 1.5, alpha = 0.05) +
  geom_sf(data = haul_sf, color = "red", shape = 16, size = 1, alpha = 0.9) +
  
  # 🔹 Título e legendas
  ggtitle("2023 - Overlapping Densidade: Haul (azul) vs Towing (vermelho)") +
  
  # 🔹 Adiciona escala e seta norte
  annotation_scale(location = "bl", style = "ticks", text_cex = 0.8) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +
  
  # 🔹 Tema minimalista
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightblue", color = NA),
    panel.grid.major = element_line(color = "white", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(size = 10)
  ) +
  
  # 🔹 Ajusta limites do mapa
  coord_sf(xlim = c(final_bbox["xmin"], final_bbox["xmax"]),
           ylim = c(final_bbox["ymin"], final_bbox["ymax"]))




