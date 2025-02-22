
# Using Self-sampling data to classify different fishing trips in Plotter Data#

# abrindo o diretorio de trabalho para Self-sampling data

setwd ("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/chapter2/self_sampling_data") #PC trabalho


# Load your data

mackerel_towing_data_2024 <- read.csv("mackerel_towing_data2024.csv")





# Pacotes necessários
library(dplyr)
library(lubridate) # usado para trabalhar com datas e horários de forma mais intuitiva e eficiente. Ele facilita a conversão, manipulação e extração de informações de colunas do tipo data/hora

# Garantir que a coluna de data esteja no formato correto

self_sampling_df <- self_sampling_df %>%
  mutate(date_time = as.POSIXct(date_time, format="%Y-%m-%d %H:%M:%S", tz="UTC")) %>%
  arrange(vessel_id, trip_id, date_time)  # Ordenar por barco, trip e tempo

# Obter a última data de cada fishing trip
trip_end_times <- self_sampling_df %>%
  group_by(vessel_id, trip_id) %>%
  summarise(trip_end = max(date_time), .groups = "drop")

# Obter a primeira data da próxima fishing trip do mesmo barco
trip_end_times <- trip_end_times %>%
  arrange(vessel_id, trip_end) %>%
  group_by(vessel_id) %>%
  mutate(next_trip_start = lead(trip_end),
         time_between_trips = difftime(next_trip_start, trip_end, units = "hours")) %>%
  ungroup()

# Calcular a média do tempo entre trips
mean_trip_interval <- mean(trip_end_times$time_between_trips, na.rm = TRUE)

# Exibir o valor médio do tempo entre trips
print(mean_trip_interval)

