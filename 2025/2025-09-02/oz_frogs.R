# Load packages
library(tidyverse)
library(rnaturalearth)
library(patchwork)

# Read data
frogID_data <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-02/frogID_data.csv")

# Get Australia map
oz_map <- 
  ne_countries(scale = "large") |> 
  filter(geounit == "Australia")

# Calculate frog diversity index (entropy)
oz_frog_diversity <- 
  frogID_data |> 
  mutate(x = round(decimalLongitude),
         y = round(decimalLatitude)) |> 
  count(x, y, scientificName) |> 
  mutate(prop = n / sum(n), .by = c(x, y)) |> 
  summarize(entropy = -sum(prop * log(prop)), .by = c(x, y)) |> 
  mutate(entropy_bin = cut_interval(entropy, length = .5))

# Create a histogram legend of frog diversity
# Heavily inspired by Andrew Heiss: https://www.andrewheiss.com/blog/2025/02/19/ggplot-histogram-legend/
frog_legend <- 
  oz_frog_diversity |> 
  ggplot() +
  geom_histogram(aes(x = entropy, fill = after_stat(factor(x))), 
                 binwidth = .5, boundary = 0, color = "white", linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 3, by = .5), labels = seq(0, 3, by = .5)) +
  scale_fill_brewer(palette = "YlGn") +
  guides(fill = "none") +
  labs(title = "Frog diversity index") +
  theme_void() +
  theme(axis.text.x = element_text(family = "PT Sans Bold", 
                                   size = rel(.8), 
                                   color = "grey30"),
        plot.title = element_text(family = "PT Sans Bold", 
                                  size = rel(1.2), 
                                  hjust = .5, 
                                  vjust = -37, 
                                  color = "grey20"))

# Plot frog diversity over a map
frog_map <- 
  ggplot() +
  geom_tile(data = oz_frog_diversity, aes(x, y, fill = entropy_bin)) +
  geom_sf(data = oz_map, fill = "transparent") +
  scale_fill_brewer(palette = "YlGn") +
  guides(fill = "none") +
  theme_void()

# Add frog bar legend to map and add text annotations
frog_map + 
  inset_element(frog_legend, left = -.55, bottom = 0.25, right = 0.35, top = 0.55) +
  plot_annotation(title = "Australian Frogs", 
                  subtitle = "Diversity of frog species by location",
                  caption = 'Data: Rowley JJL, & Callaghan CT (2020) "The FrogID dataset" | Packages: {tidyverse, rnaturalearth, patchwork} | Visualization: C. Börstell',
                  theme = theme(plot.title = element_text(size = rel(3.5), 
                                                          family = "Patrick Hand",
                                                          hjust = -1.7,
                                                          vjust = -5,
                                                          color = "grey15"),
                                plot.subtitle = element_text(size = rel(1.4), 
                                                             family = "Patrick Hand",
                                                             hjust = -1.25,
                                                             vjust = -16,
                                                             color = "grey25"),
                                plot.caption = element_text(family = "PT Sans Narrow",
                                                            color = "grey50",
                                                            size = rel(.7),
                                                            hjust = 3.2,
                                                            vjust = 40),
                                plot.margin = margin(-12, -50, -20, 20, "mm")))

# Save plot
ggsave("oz_frogs.png", width = 7, height = 5, units = "in", dpi = 600, bg = "white")

