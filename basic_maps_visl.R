
# First version of map code #

# abrindo o diretorio de trabalho

setwd ("C:/Users/Iohara Quirino/OneDrive/Área de Trabalho/PhD/chapters/chapter_2") # Personal laptop
setwd("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/results") # Work laptop

# Load my data

mackerel_plotter_data <- read.csv("mackerel_data.csv")

# Installing some packages if needed #

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

# Base map #

world <- ne_countries(scale = "medium", returnclass = "sf") # Load world map

europe <- world[world$continent == "Europe", ] # Load Europe map

uk_neighbors <- world[world$name %in% c("United Kingdom", "Ireland", "France", 
                                        "Belgium", "Netherlands", "Germany"), ]


data_sf <- st_as_sf(data_novmackerel, coords = c("lon", "lat"), crs = 4326)

bbox <- st_bbox(data_sf)


# Filtering for specic years

data_filtered <- data_sf %>% 
  filter(year >= 2019 & year <= 2023)

# Specific year, use:

data_filtered <- data_sf %>% 
  filter(year == 2022)

# Map #

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


# Adding analysis of the map per vessel to understand possibles outliers #

data_filtered <- data_filtered %>%
  filter(VE_ID %in% c(2, 8, 9, 11)) # more than 1 vessel

#Or

# Use just 1 vessel

data_filtered <- data_filtered %>%
  filter(VE_ID == 22) 

# Map with the facet by vessel

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


# Novas figuras - Adicionando Noruega, retângulos do ICES e comparando diferentes tipos de dados (self-sampling data vs plotter data)

# Variáveis utilizadas para criação de mapas: Latitude, longitude e mês.


# Mapas do self-sampling #

# abrindo o diretorio de trabalho
setwd ("C:/Users/Iohara Quirino/OneDrive/Área de Trabalho/PhD/chapters/chapter_2") #PC pessoal
setwd ("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/chapter2") #PC trabalho


# Load your data

mack_selfs_2023_autumn <- read.csv("INTERNAL_QC_MACautumn23_HAUL_21.05.csv")


# Necessary packages 

library(ggplot2)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)


world <- ne_countries(scale = "medium", returnclass = "sf") # World map; filtering relevant countries

europe <- world[world$continent == "Europe", ] # Europe map

uk_neighbors <- world[world$name %in% c("United Kingdom", "Ireland", "France", 
                                        "Belgium", "Netherlands", "Germany", "Norway"), ] # Including Norway


data_sf <- st_as_sf(mack_selfs_2023_autumn, coords = c("londd", "latdd"), crs = 4326) # Converting data to sf
# sf (simple features) to deal with geographic information, part of me the main tool to manipulate spacial data in R


bbox <- st_bbox(data_sf) # Calculate the bounding box to adjust the map limits

data_filtered <- data_sf # If I need to filter the data


# Creating map

library(ggrepel)  # To label countries
library(viridis) 

# Adicionando o shapefile do ICES

library(sf)
setwd ("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/chapter2/ICES_areas") #PC trabalho
setwd ("C:/Users/Iohara Quirino/OneDrive/Área de Trabalho/PhD/chapters/chapter_2") #PC pessoal

# Load the ICES shapefile (update with the correct path)
ICES_Areas <- st_read("ICES_Areas_20160601_cut_dense_3857.shp")

# Checking the ICES dataset
head(ICES_Areas)  # Check attributes
summary(ICES_Areas)
st_crs(ICES_Areas)  # Check the coordinate system
ICES_Areas <- st_make_valid(ICES_Areas) 


#Agora meu código com a adição do ICES

ggplot() +
  # Base map layer with customized fill and border
  geom_sf(data = uk_neighbors, fill = "aliceblue", color = "darkblue", linewidth = 0.3) +
  
  # ICES Statistical Areas overlay
  geom_sf(data = ICES_Areas, color = "black", fill = NA, linewidth = 0.2, linetype = "dashed") +
  
  # Haul points colored by month
  geom_sf(data = data_filtered, aes(color = as.factor(mnth)), size = 1.5, alpha = 0.8) +  
  
  # Color scale for months
  scale_color_viridis_d(option = "C", name = "Month") +
  
  # Scale and north arrow (moved before theme)
  annotation_scale(location = "bl", style = "ticks", text_cex = 0.8) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +
  
  # Title
  ggtitle("Fishing Hauls - Winter 2023") +
  
  # Theme and aesthetics
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
  
  # Adjusting zoom limits
  coord_sf(xlim = c(-7, 7), ylim = c(54, 63))





### Modificando agora todo o código os meses pelos VESSELS, following the same logic ###

# Abrindo diretorio
setwd ("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/chapter2/self_sampling_data") #PC trabalho

# Load your data
mack_selfs_2023_winter <- read.csv("INTERNAL_QC_MACwinter23_HAUL_24.05.csv")

# Necessary packages 
library(ggplot2)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggrepel)  # To label countries
library(viridis)


world <- ne_countries(scale = "medium", returnclass = "sf")  # World map
europe <- world[world$continent == "Europe", ]              # Europe map


uk_neighbors <- world[world$name %in% c("United Kingdom", "Ireland", "France", 
                                        "Belgium", "Netherlands", "Germany", "Norway"), ] 

# Convert data to an sf object (Simple Features) to work with spatial data
data_sf <- st_as_sf(mack_selfs_2023_winter, coords = c("londd", "latdd"), crs = 4326)

# Bounding box (useful for auto-adjusting zoom if needed)
bbox <- st_bbox(data_sf)

# Filtering data (if needed) - currently, it includes all vessels
data_filtered <- data_sf  

# Load the ICES shapefile (update with the correct path)
setwd("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/chapter2/ICES_areas")  # Laptop location
ICES_Areas <- st_read("ICES_Areas_20160601_cut_dense_3857.shp")

# Checking the ICES dataset
head(ICES_Areas)  # Check attributes
summary(ICES_Areas)
st_crs(ICES_Areas)  # Check the coordinate system

# Creating the map with vessels as the color category
ggplot() +
  # Base map layer with customized fill and border
  geom_sf(data = uk_neighbors, fill = "aliceblue", color = "darkblue", size = 0.3) +
  
  # ICES Statistical Areas overlay
  geom_sf(data = ICES_Areas, color = "black", fill = NA, size = 0.2, linetype = "dashed") +
  
  # Haul points colored by vessel
  geom_sf(data = data_filtered, aes(color = as.factor(vessel)), size = 1, alpha = 0.8) +  # Changed from `month` to `vessel`
  
  # Refined color palette for vessels
  scale_color_viridis_d(option = "C", name = "vessel") +  # Legend now refers to vessels
  
  # Clean, professional theme
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightblue", color = NA),  # Light blue map background
    panel.grid.major = element_line(color = "white", size = 0.2),     # Soft grid lines
    panel.grid.minor = element_blank(),                              # Remove minor grid lines
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5), # Bold, centered title
    plot.subtitle = element_text(size = 12, hjust = 0.5),             # Subtitle
    legend.position = "right",                                        # Legend on the right
    legend.title = element_text(face = "bold"),                       # Bold legend title
    legend.background = element_rect(fill = "white", color = NA),     # White background for legend
    axis.title = element_blank(),                                    # Remove axis labels
    axis.text = element_text(size = 10)                              # Readable axis text
  ) +
  
  # Informative title
  ggtitle("Fishing Hauls by Vessel - Winter 2023") +
  
  # Scale bar and north arrow
  annotation_scale(location = "bl", style = "ticks", text_cex = 0.8) +  # Scale in bottom-left
  annotation_north_arrow(
    location = "tl",  # Top-left corner
    which_north = "true",  # True north direction
    pad_x = unit(0.5, "cm"),  # Padding adjustments
    pad_y = unit(0.5, "cm"),
    style = north_arrow_fancy_orienteering()  # Classic north arrow style
  ) +
  
  # Adjusting zoom limits
  coord_sf(xlim = c(-7, 7), ylim = c(54, 63))  # Adjusted zoom for UK and surroundings



#.
#.
#.


# MAPS PLOTTER DATA #


# abrindo o diretorio de trabalho
setwd ("C:/Users/Iohara Quirino/OneDrive/Área de Trabalho/PhD/chapters/chapter_2") #PC pessoal
setwd ("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/results") #PC trabalho


# Load your data

mackerel_towing_data_2024 <- read.csv("mackerel_towing_data2024.csv")


# First, I want to create different dataset per year

unique (mackerel_towing_data$year) # Check unique values in variable

data_by_year <- split(mackerel_towing_data, mackerel_towing_data$year) # Splitting by year

for(year in names (data_by_year)) {
  separated_years <- paste0("mackerel_towing_data", year, ".csv")
  write.csv (data_by_year[[year]], file = separated_years, row.names = FALSE)
}


# Necessary packages 

library(ggplot2)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggrepel)  # To label countries
library(viridis)


# Map

world <- ne_countries(scale = "medium", returnclass = "sf") # Mapa do mundo; filtrar países relevantes

europe <- world[world$continent == "Europe", ] # Mapa da Europa

uk_neighbors <- world[world$name %in% c("United Kingdom", "Ireland", "France", 
                                        "Belgium", "Netherlands", "Germany", "Norway"), ] # Inclui Noruega


data_sf <- st_as_sf(mackerel_towing_data_2024, coords = c("lon", "lat"), crs = 4326) # Converter os dados de haul para sf


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


plot(st_geometry(data_sf), col = "blue", pch = 20, main = "Distribution - 2024") # CHECKING DATA DISTRIBUTION


# Mapa Plotter já com o shapefile do ICES


# Set working directory for the ICES shapefile (use the correct one for your machine)
setwd("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/chapter2/ICES_areas")  # PC trabalho
setwd("C:/Users/Iohara Quirino/OneDrive/Área de Trabalho/PhD/chapters/chapter_2")  # PC pessoal


ICES_Areas<- st_read("ICES_Areas_20160601_cut_dense_3857.shp")

head(ICES_Areas)
summary(ICES_Areas)
st_crs(ICES_Areas)  # Check coordinate system
ICES_Areas <- st_make_valid(ICES_Areas) 


# Transformar data_sf em um objeto sf usando as colunas de longitude e latitude
data_sf <- st_as_sf(mackerel_towing_data_2024, coords = c("lon", "lat"), crs = 4326) 

data_filtered <- data_sf


#Mapa

# Now, the map with ICES added
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
  ggtitle("Towings - 2024") +
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


#.
#.
#.

### Modificando agora todo o código os meses pelos VESSELS Plotter data ###

# Load your data
mackerel_towing_data <- read.csv("mackerel_data.csv")

# Necessary packages
library(ggplot2)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

# Mapa do mundo e países relevantes
world <- ne_countries(scale = "medium", returnclass = "sf")  # Mapa do mundo
europe <- world[world$continent == "Europe", ]              # Mapa da Europa

# Inclui Reino Unido e países vizinhos (incluindo Noruega)
uk_neighbors <- world[world$name %in% c("United Kingdom", "Ireland", "France", 
                                        "Belgium", "Netherlands", "Germany", "Norway"), ] 

# Converter os dados para sf (Simple Features)
data_sf <- st_as_sf(mackerel_towing_data, coords = c("lon", "lat"), crs = 4326)

# Filtrar os dados por ano e embarcação, se necessário
data_filtered <- data_sf %>%
  filter(year %in% c(2021) & VE_ID =="22")  # Filtrar apenas o ano de interesse (2004)

# Carregar o shapefile do ICES
ICES_Areas <- st_read("ICES_Areas_20160601_cut_dense_3857.shp")

# Mapa
ggplot() +
  # Camada do mapa base com preenchimento e contornos personalizados
  geom_sf(data = uk_neighbors, fill = "aliceblue", color = "darkblue", size = 0.3) +
  
  # Camada do ICES Statistical Areas com contornos
  geom_sf(data = ICES_Areas, color = "darkblue", fill = NA, linewidth = 0.2, linetype = "dashed") +
  
  # Camada com os pontos de towings por embarcação
  geom_sf(data = data_filtered, aes(color = as.factor(VE_ID)), size = 1, alpha = 0.8) +  # `vessel` no lugar de `month`
  
  # Paleta de cores refinada para as embarcações
  scale_color_viridis_d(option = "A", name = "Vessel") +
  
  # Tema mais estilizado
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightblue", color = NA),  # Fundo do mapa em azul claro
    panel.grid.major = element_line(color = "white", linewidth = 0.2),  # Linhas de grade suaves
    panel.grid.minor = element_blank(),                                # Remove linhas menores
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Título centralizado e em negrito
    plot.subtitle = element_text(size = 12, hjust = 0.5),              # Subtítulo menor
    legend.position = "right",                                         # Legenda à direita
    legend.title = element_text(face = "bold"),                        # Título da legenda em negrito
    legend.background = element_rect(fill = "white", color = NA),      # Fundo branco para a legenda
    axis.title = element_blank(),                                     # Remove os rótulos dos eixos
    axis.text = element_text(size = 10)                               # Texto dos eixos mais legível
  ) +
  
  # Títulos informativos
  ggtitle("Towings by Vessel - 2021") +
  
  # Escala e seta norte mais refinadas
  annotation_scale(location = "bl", style = "ticks", text_cex = 0.8) +  # Escala no canto inferior esquerdo
  
  # Seta do norte clássica no canto superior esquerdo
  annotation_north_arrow(
    location = "tl",  # Top left (esquerda superior)
    which_north = "true",  # Norte verdadeiro
    pad_x = unit(0.5, "cm"),  # Espaçamento horizontal
    pad_y = unit(0.5, "cm"),  # Espaçamento vertical
    style = north_arrow_fancy_orienteering()  # Estilo clássico
  ) +
  
  # Ajuste dos limites para um visual refinado
  coord_sf(xlim = c(-7, 7), ylim = c(54, 63))  # Zoom ajustado para o Reino Unido e arredores

#
#
#

# Using facet_grid to compare different graphs of my data #

# abrindo o diretorio de trabalho
setwd ("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/results") #PC trabalho


# Load your data

mackerel_towing_data <- read.csv("MAC_towing_tracks.csv")


library(ggplot2)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

# 🔹 Criar o mapa base novamente
world <- ne_countries(scale = "medium", returnclass = "sf")  # Mapa do mundo
europe <- world[world$continent == "Europe", ]  # Mapa da Europa

# 🔹 Criar o objeto uk_neighbors corretamente
uk_neighbors <- world %>% 
  filter(name %in% c("United Kingdom", "Ireland", "France", 
                     "Belgium", "Netherlands", "Germany", "Norway"))

# 🔹 Garantir que data_filtered seja um `sf`
if (!inherits(mackerel_towing_data, "sf")) {
  data_filtered <- st_as_sf(mackerel_towing_data, coords = c("lon", "lat"), crs = 4326)
}

# 🔹 Criar a bounding box dos dados
bbox_data <- st_bbox(data_filtered)

# 🔹 Definir uma região costeira adicional (Noruega) para expandir a visualização
norway_coast_bbox <- c(xmin = 4, xmax = 6, ymin = 58, ymax = 63)

# 🔹 Criar um bounding box final que une os dois
final_bbox <- c(
  xmin = min(bbox_data["xmin"], norway_coast_bbox["xmin"]),
  xmax = max(bbox_data["xmax"], norway_coast_bbox["xmax"]),
  ymin = min(bbox_data["ymin"], norway_coast_bbox["ymin"]),
  ymax = max(bbox_data["ymax"], norway_coast_bbox["ymax"])
)

# Set working directory for the ICES shapefile (use the correct one for your machine)
setwd("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/chapter2/ICES_areas")  # PC trabalho


ICES_Areas<- st_read("ICES_Areas_20160601_cut_dense_3857.shp")

head(ICES_Areas)
summary(ICES_Areas)
st_crs(ICES_Areas)  # Check coordinate system
ICES_Areas <- st_make_valid(ICES_Areas) 


# 🔹 Verificar e ajustar o nome correto da coluna do vessel
print(colnames(data_filtered))  # Verifica os nomes das colunas para evitar erros

vessel_column <- "VE_ID"  # Substitua pelo nome correto, se diferente

# 🔹 Lista de todos os vessels únicos no dataset
vessels <- unique(mackerel_towing_data[[vessel_column]])

# 🔹 Loop para criar um gráfico por vessel
for (v in vessels) {
  
  # Filtrar os dados apenas para o vessel atual e manter a geometria
  vessel_data <- mackerel_towing_data %>% filter(.data[[vessel_column]] == v)
  
  # Garantir que a geometria foi mantida
  if (!inherits(vessel_data, "sf")) {
    vessel_data <- st_as_sf(vessel_data, coords = c("lon", "lat"), crs = 4326)
  }
  
  # Criar o mapa para o vessel atual
  p <- ggplot() +
    geom_sf(data = uk_neighbors, fill = "aliceblue", color = "darkblue", linewidth = 0.3) +
    geom_sf(data = ICES_Areas, color = "darkblue", fill = NA, linewidth = 0.2, linetype = "dashed") +
    geom_sf(data = vessel_data, aes(color = as.factor(month)), size = 0.5, alpha = 0.8) +
    scale_color_manual(values = c("red", "orange", "purple", "yellow", "magenta", "green", "lightsalmon3", "indianred3", "plum4"), name = "Month") +
    annotation_scale(location = "bl", style = "ticks", text_cex = 0.8) +
    annotation_north_arrow(
      location = "tl",
      which_north = "true",
      pad_x = unit(0.5, "cm"),
      pad_y = unit(0.5, "cm"),
      style = north_arrow_fancy_orienteering()
    ) +
    ggtitle(paste("Towings | Vessel:", v)) +
    
    # Criar a matriz de gráficos para o vessel: mês (linha) x ano (coluna)
    facet_grid(rows = vars(month), cols = vars(year)) +
    
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
      axis.text = element_text(size = 6),
      strip.text = element_text(size = 7)
    ) +
    coord_sf(xlim = c(final_bbox["xmin"], final_bbox["xmax"]),
             ylim = c(final_bbox["ymin"], final_bbox["ymax"]))
  
  # Exibir o gráfico para cada vessel
  print(p)
  
  # Opcional: salvar cada gráfico como imagem
  ggsave(filename = paste0("map_vessel_", v, ".png"), plot = p, width = 10, height = 8, dpi = 300)
}






# Extra 

plot(st_geometry(data_sf), col = "blue", pch = 20, main = "Checking Data Distribution 2023 autumn") # CHECKING DATA DISTRIBUTION

unique (data$column) # Check unique values in variable







