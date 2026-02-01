
# Load packages -----------------------------------------------------------

library(tidyverse)
library(scales)
library(cbthemes)
library(marquee)



# Read data ---------------------------------------------------------------

# Language families
families <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-23/families.csv")

# Languages
languages <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-23/languages.csv") |> 
  
  # Join with language family data
  left_join(families, join_by(family_id == id)) |> 
  
  # Make language isolates their own families
  mutate(family = case_when(
    is.na(family) ~ name,
    .default = family
  ))



# Plot data ---------------------------------------------------------------

# Bin latitudes and calculate Shannon diversity index entropy
# on language families per latitude bin
languages |> 
  filter(!is.na(latitude)) |> 
  mutate(lat_bin = cut_interval(abs(latitude), length = 1, labels = F)) |> 
  count(lat_bin, family) |> 
  mutate(prop = n / sum(n), .by = lat_bin) |> 
  summarize(entropy = -sum(prop * log(prop)), .by = lat_bin) |> 
  
  # Plot with {cbthemes} and customization
  ggplot() +
  stat_smooth(aes(x = lat_bin * 111, y = entropy), 
              se = F, color = "darkolivegreen", lineend = "round", linetype = 3) +
  geom_point(aes(x = lat_bin * 111, y = entropy, fill = entropy), 
             shape = 21, size = 3, color = "white") +
  annotate(GeomMarquee, x = I(.1), y = I(.25), 
           label = 
           "The amount of **linguistic diversity** in the world
           — as measured by language family entropy —
           decreases as one moves further away from the equator.", 
           width = .53, lineheight = .8, color = cb_pal("cb_seq_green")[9],
           family = "PT Sans Narrow", size = 3.5, hjust = 0) +
  scale_x_continuous(breaks = breaks_pretty(), 
                     labels = label_number(big.mark = " ", suffix = " km")) +
  scale_y_continuous(breaks = breaks_pretty()) +
  scale_fill_continuous(palette = cb_pal("cb_seq_green")[3:9]) +
  labs(title = "Linguistic diversity in the world",
       subtitle = "Language family entropy by binned latitude",
       x = "Distance from the equator",
       y = "Entropy",
       caption = "Data: Glottolog 5.2.1\nVisualization: C. Börstell\nPackages: {tidyverse, scales, marquee, cbthemes}") +
  guides(fill = "none",
         x = guide_axis(cap = "both"),
         y = guide_axis(cap = "both")) +
  theme_cb_base(base_family = "PT Sans Narrow", paper = cb_offwhite) +
  theme(axis.line = element_line(lineend = "square"),
        axis.ticks = element_line(lineend = "square"),
        plot.caption = element_text(size = rel(.5)))

# Save plot
ggsave("glottolog.jpg", width = 4.5, height = 4.5, dpi = 600)

