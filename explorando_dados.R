# Explorando dados com mais detalhes por ano #

# Plotting with differents days appearing on subset #

# abrindo o diretorio de trabalho
setwd ("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/results/Figures/year 2004") #PC trabalho

# Load your data

mack_subset1 <- read.csv("mack_subset1_date_time.csv")

library(ggplot2)
library(sf)
library(dplyr)

# Convert to sf
data_sf <- st_as_sf(mack_subset1, coords = c("lon", "lat"), crs = 4326)

# Convert datetime to date format (if needed)
data_sf <- data_sf %>%
  mutate(date = as.Date(date))  # Adjust "datetime" to the actual column name in your dataset

# Calculate bounding box for zooming
bbox_filtered <- st_bbox(data_sf)

# Create the map with different colors for each day
ggplot() +
  geom_sf(data = uk_neighbors, fill = "aliceblue", color = "darkblue", linewidth = 0.3) +
  geom_sf(data = ICES_Areas, color = "darkblue", fill = NA, linewidth = 0.2, linetype = "dashed") +
  geom_sf(data = data_sf, aes(color = as.factor(date)), size = 1.5, alpha = 0.8) +  # Coloring by date
  scale_color_viridis_d(name = "Date") +  # Automatic color scale for better contrast
  annotation_scale(location = "bl", style = "ticks", text_cex = 0.8) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +
  ggtitle("Towings 2004 - Subset 1 day by colour") +
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
           ylim = c(bbox_filtered["ymin"], bbox_filtered["ymax"]))  # Automatic zoom


# Check days in different tracks


# Transformar datetime na classe POSIXct

class(mack_subset2$datetime)

library(lubridate)

mack_subset2$datetime <- ymd_hms(mack_subset2$datetime)  # If format is "YYYY-MM-DD HH:MM:SS"

class(mack_subset2$datetime) #to check

library(dplyr)

# Separate datetime into date and time columns
data <- mack_subset2 %>%
  mutate(date = as.Date(datetime),  # Extracts date
         time = format(datetime, "%H:%M:%S"))  # Extracts time as character

# View result
print(data)

write.csv(data, "mack_subset2_date_time.csv", row.names = FALSE)

#
#
#

# Working with heading #

# abrindo o diretorio de trabalho
setwd ("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/results/Figures/Towing_figures/year 2004") #PC trabalho

# Load your data

mackerel_1 <- read.csv("mack_subset1_heading_diff.csv")


# Ver estatísticas descritivas dos dados de steaming
summary(mackerel_2[, c("speed_kn", "heading")])

# Comparar com os dados gerais (mackerel_towing_data_2004)
summary(mackerel_towing_data_2004[, c("speed_kn", "heading")])

library(ggplot2)


# Histograma de heading (steaming vs. geral)
ggplot() +
  geom_histogram(data = mackerel_towing_2004, aes(x = heading), fill = "blue", alpha = 0.5, bins = 30) +
  ggtitle("Heading - 2004 - All points") +
  theme_minimal()


ggplot() +
  geom_histogram(data = mackerel_2, aes(x = heading_diff), fill = "red", alpha = 0.5, bins = 30) +
  ggtitle("heading_diff - 2004 - subset 2") +
  theme_minimal()


# Creating heading_diff column

library(dplyr)

# Ordenar os dados por vessel, dia e tempo (se houver variável de tempo)
mackerel_1 <- mackerel_1 %>%
  arrange(VE_ID, date, time) %>%  # Ajuste "date" e "time" para os nomes reais das colunas
  group_by(VE_ID, date) %>%  # Agrupar por vessel e dia
  mutate(heading_diff = abs(heading - lag(heading))) %>%  # Diferença absoluta entre dois pontos consecutivos
  ungroup()

# Exibir os primeiros valores calculados
head(mackerel_1[, c("VE_ID", "date", "heading", "heading_diff")])

write.csv(mackerel_1, "mack_subset1_heading_diff.csv", row.names = FALSE)


# Heading_diff summary for specific days #

library(dplyr)

# Define the specific date you want to analyze
specific_date <- "17/10/2004"  # Change to the date you need

# Filter data for that specific date and summarize heading_diff
summary(mackerel_1 %>% filter(date == specific_date) %>% select(heading_diff))

# Summary specific day but cutting outlier #

summary(mackerel_2 %>% filter(date == "02/10/2004" & heading_diff < 10) %>% select(heading_diff))


#
#
#

# Working with time_diff #

#Working now with subset 1 from year 2004

unique(mackerel_1$timediff_m)  # Show unique values in time_diff

#summary statistics of time_diff per day

time_diff_summary <- mackerel_1 %>%
  group_by(date) %>%  # Use existing day column
  summarise(
    mean_time_diff = mean(timediff_m, na.rm = TRUE),
    median_time_diff = median(timediff_m, na.rm = TRUE),
    min_time_diff = min(timediff_m, na.rm = TRUE),
    max_time_diff = max(timediff_m, na.rm = TRUE),
    sd_time_diff = sd(timediff_m, na.rm = TRUE),
    count = n()  # Count of records per day
  ) %>%
  ungroup()

# Print the summary
print(time_diff_summary)

#Save summary

write.csv(time_diff_summary, "time_diff_summary_subset1.csv", row.names = FALSE)

# To see if steaming days have more consistent time intervals

library(ggplot2)

ggplot(time_diff_summary, aes(x = date, y = mean_time_diff)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  ggtitle("Daily Mean Time Difference") +
  xlab("Date") +
  ylab("Mean Time Difference (minutes)") +
  theme_minimal()




mackerel_1 %>%
  mutate(steaming_flag = (heading_diff < 20)) %>%  # Example rule from before
  group_by(steaming_flag) %>%
  summarise(mean_distance_diff = mean(distance_m, na.rm = TRUE),
            sd_distance_diff = sd(distance_m, na.rm = TRUE),
            min_distance_diff = min(distance_m, na.rm = TRUE),
            max_distance_diff = max(distance_m, na.rm = TRUE),
            count = n())









# Histograma de speed (steaming vs. geral)


ggplot() +
  geom_histogram(data = mackerel_towing_data_2004, aes(x = speed_kn), fill = "blue", alpha = 1, bins = 30) +
  ggtitle("Speed - All") +
  theme_minimal()


ggplot() +
  geom_histogram(data = mackerel_2, aes(x = speed_kn), fill = "red", alpha = 1, bins = 30) +
  ggtitle("Speed - Steamming") +
  theme_minimal()






