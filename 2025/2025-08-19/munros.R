
# Load packages -----------------------------------------------------------

library(tidyverse)
library(rnaturalearth)
library(ggtext)
library(ggridges)



# Read data ---------------------------------------------------------------

# Read data
scottish_munros <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-19/scottish_munros.csv")

# Get map data of Scotland
scotland <- 
  ne_countries(geounit = "scotland", 
               type = "map_units", 
               scale = "large")



# Process and plot data ---------------------------------------------------


# Round coordinates to nearest kilometer and plot as ridges
scottish_munros |> 
  mutate(ybin = round(ycoord, -3),
         xbin = round(xcoord, -3)) |> 
  count(xbin, ybin) |> 
  ggplot() +
  geom_sf(data = t, fill = "lightyellow2", color = "transparent") +
  geom_density_ridges(aes(x = xbin, y = ybin, group = ybin), 
                          fill = "transparent", color = "grey30", 
                          linewidth = .2, lty = 1,
                          rel_min_height = .1, scale = 5)  +
  coord_sf(crs = 27700, xlim = c(70000, 445000)) +
  annotate("richtext", x = 45000, y = 1170000, label = "Scottish Munros", 
           label.color = NA, fill = "transparent", family = "PT Sans Narrow Bold", 
           hjust = 0, color = "grey20", size = rel(14)) +
  annotate("richtext", x = 55000, y = 1135000, label = "Density of Munros and Munro tops<br>— mountains/summits >3,000 ft —<br>across the country of Scotland", 
           label.color = NA, fill = "transparent", family = "PT Sans Narrow", 
           hjust = 0, vjust = 1, color = "grey35", size = rel(6)) +
  annotate("richtext", x = 60000, y = 1037000, label = "Coordinates rounded to nearest kilometer", 
           label.color = NA, fill = "transparent", family = "PT Sans Narrow", 
           hjust = 0, vjust = 1, color = "grey45", size = rel(3)) +
  annotate("richtext",  x = 62000, y = 1015000,
           label = "<b>Data:</b> Database of British and Irish Hills v18.2 (hills-database.co.uk) via TidyTuesday<br><b>Packages:</b> {ggridges, ggtext, rnaturalearth, tidyverse}<br><b>Visualization:</b> C. Börstell", 
           label.color = NA, fill = "transparent", family = "PT Sans Narrow", 
           hjust = 0, vjust = 1, color = "grey65", size = rel(1.8)) +
  theme_void()

# Save plot
ggsave("munros.png", width = 5, height = 7, units = "in", bg = "#FAFBF0", dpi = 600)

# Alt-text: Light yellow-hue map of Scotland with smoothed, dark gray ridgelines showing the density of Munros and Munro tops by location, mainly in the Highlands. Title reads "Scottish Munros" with the description "Density of Munros and Munro tops — mountains/summits >3,000 ft — across the country of Scotland. Coordinates rounded to nearest kilometer. Data: Database of British and Irish Hills v18.2 (hills-database.co.uk) via TidyTuesday; Packages: {ggridges, ggtext, rnaturalearth, tidyverse}; Visualization: C. Börstell 
 
