
# Load packages -----------------------------------------------------------

library(tidyverse)
library(rnaturalearth)
library(ggspatial)
library(ggarrow)



# Read data ---------------------------------------------------------------

orcas <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-15/orcas.csv")


mapdata <- 
  rnaturalearth::ne_countries(country = c("United States of America", "Canada"), scale = 10)




# Plot data ---------------------------------------------------------------

ggplot() +
  geom_arrow_curve(data = orcas |> filter(begin_longitude != end_longitude,
                                    begin_latitude != end_latitude), 
             aes(x = begin_longitude, xend = end_longitude, 
                 y = begin_latitude, yend = end_latitude), 
             alpha = .3, color = "grey20") +
  geom_sf(data = mapdata, aes(fill = brk_name)) +
  annotate("text", x = max(drop_na(orcas)$end_longitude)+1, y = 49.1, label = "CANADA", color = "#660000") +
  annotate("text", x = max(drop_na(orcas)$end_longitude)+1.15, y = 48.9, label = "USA", color = "#223366") +
  annotate("text", x = -126.1, y = 47.9, label = "Orca Encounters", 
           family = "PT Sans Narrow Bold", size = 6, hjust = 0) +
  annotate("text", x = -126.1, y = 47.6, label = "Locations of start and end points\nof orca encounters in the Salish Sea\nof the Pacific Northwest between\nCanada and the US (2017—2024)", 
           family = "PT Sans Narrow", size = 3.5, hjust = 0, color = "grey40", lineheight = .92) +
  annotate("text", x = -126.1, y = 47.3, label = "Data: Center for Whale Research (CWR)\nvia TidyTuesday", 
           family = "PT Sans Narrow", size = 2.5, hjust = 0, color = "grey60", lineheight = .9) +
  coord_sf(xlim = c(min(drop_na(orcas)$end_longitude)-2, max(drop_na(orcas)$end_longitude)+1.3),
           ylim = c(min(drop_na(orcas)$end_latitude)-1, max(drop_na(orcas)$end_latitude)+1)) +
  annotation_north_arrow(height = unit(1, "cm"), width = unit(.75, "cm"), location = "tr") +
  annotation_scale(text_family = "PT Sans Narrow Bold", location = "bl", text_col = "grey10", bar_cols = c("#FAFAFA", "grey10")) +
  scale_fill_manual(values = c("red4", "dodgerblue4")) +
  theme_void() +
  theme(legend.position = "none")



# Save plot ---------------------------------------------------------------

ggsave("orcas.png", height = 5, width = 5.75, units = "in", dpi = 600, bg = "white")



