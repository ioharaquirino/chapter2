# Exploring per vessel #

# Count unique years per vessel

# abrindo o diretorio de trabalho
setwd ("C:/Users/SH01IQ/OneDrive - UHI/Desktop/iohara's phd/PhD_chapters/Chapter 2 - fishing areas changes/results/Towing_data") #PC trabalho


# Load my data

mackerel_towing <- read.csv("MAC_towing_tracks.csv")

library(dplyr)

vessel_years_months <- mackerel_towing %>%
  group_by(VE_ID, year) %>%  # Group by vessel and year
  summarise(
    month_count = n_distinct(month),  # Count unique months per year
    months = paste(sort(unique(month)), collapse = ", ")  # List months per year
  ) %>%
  ungroup() %>%
  group_by(VE_ID) %>%
  summarise(
    year_count = n_distinct(year),  # Count unique years per vessel
    years = paste(sort(unique(year)), collapse = ", "),  # List all years for each vessel
    details = paste(paste0(year, " (", months, ")"), collapse = " | ")  # Show months per year
  ) %>%
  ungroup()

# View the result
print(vessel_years_months)

write.csv(vessel_years_months, "vessel_years_months_summary.csv", row.names = FALSE)

