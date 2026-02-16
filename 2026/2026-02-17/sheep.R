
# Load packages -----------------------------------------------------------

library(tidyverse)
library(cbthemes) # Custom themes borstell/cbthemes
library(marquee)
library(patchwork)
library(rnaturalearth)
library(rvest)



# Theme -------------------------------------------------------------------

# https://fonts.google.com/specimen/Unkempt
font_base <- "Unkempt"
font_bold <- "Unkempt Bold"



# Read data ---------------------------------------------------------------

# Data from StatsNZ via TidyTuesday
dataset <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-02-17/dataset.csv")

# Data from Statista via Wikipedia
nz_pop <- 
  rvest::read_html("https://en.wikipedia.org/wiki/Demographics_of_New_Zealand") |> 
  html_table() |> 
  _[[4]] |> 
  select(year = 1,
         population = 2) |> 
  mutate(population = parse_number(population))



# Process data ------------------------------------------------------------

# Join population and sheep data
nz_data <- 
  nz_pop |> 
  left_join(dataset |> filter(measure == "Total Sheep") |> select(year = 1, sheep = value)) |> 
  filter_out(when_any(is.na(population), is.na(sheep)))

# Get and plot a simple map of NZ
nz_map <- 
  rnaturalearth::ne_countries("medium") |> 
  filter(sovereignt == "New Zealand") |> 
  ggplot() +
  geom_sf(color = "snow", fill = "palegreen", linewidth = .5) +
  coord_sf(xlim = c(166, 179),
           ylim = c(-47, -34.5)) +
  theme_void()

# Count mean ratio by 5-year increments
nz_data_by_5 <- 
  nz_data |> 
  mutate(year = year - year %% 5) |> 
  summarize(population = mean(population, na.rm = T),
            sheep = mean(sheep, na.rm = T), .by = year) |> 
  mutate(n = round(sheep / population)) 



# Plot data ---------------------------------------------------------------


nz_data_by_5 |> 
  uncount(n, .remove = F) |> 
  mutate(y = row_number(), .by = year) |> 
  ggplot() +
  geom_area(aes(x = year, y = n), 
            position = "identity", fill = "palegreen", 
            color = "snow", outline.type = "full", linewidth = 10, lineend = "round") +
  geom_area(aes(x = year, y = n), 
            position = "identity", fill = "palegreen", 
            color = "palegreen", outline.type = "full", linewidth = 9, lineend = "round") +
  geom_point(aes(x = year, y = y), shape = "🐑", size = 3) +
  annotate("point", x = seq(1935, 2020, by = 5), y = -.25, shape = "👤", size = 3) +
  annotate("segment", x = 1996, xend = 1986, y = 22, 
           color = "white", arrow = arrow(length = unit(3, "mm"), type = "closed"),
           lineend = "round" , linejoin = "round", linewidth = 1) +
  annotate(GeomMarquee, x = 1996, y = 21.8, label = str_glue("**1980—85**  
                                                           ratio {max(nz_data_by_5$n)}:1"),
           lineheight = .8, size = 3, family = font_base, 
           fill = "snow", style = classic_style(border_radius = 5, padding = trbl(2))) +
  
  annotate("segment", x = 2020, y = 10, yend = 7, 
           color = "white", arrow = arrow(length = unit(3, "mm"), type = "closed"),
           lineend = "round" , linejoin = "round", linewidth = 1) +
  annotate(GeomMarquee, x = 2019, y = 10, label = str_glue("**2020—24**  
                                                           \u2002ratio {min(nz_data_by_5$n)}:1"),
           lineheight = .8, size = 3, family = font_base, 
           fill = "snow", style = classic_style(border_radius = 5, padding = trbl(2))) +
  
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  scale_y_continuous(breaks = seq(5, 20, by = 5),
                     labels = paste0(seq(5, 20, by = 5), ":1")) +
  labs(x = NULL, y = NULL, 
       title = "Sheep to people ratio in Aotearoa/NZ",
       subtitle = "Data from 1935—2024 by 5-year averages",
       caption = "**Packages:** {tidyverse, marquee, patchwork, rnaturalearth, rvest} | **Data:** StatsNZ & Statista via TidyTuesday & Wikipedia | **Visualization:** C. Börstell") +
  theme_cb_base(base_family = font_base, paper = "paleturquoise") +
  theme(axis.text.x = element_text(vjust = 3),
        plot.caption = element_marquee(size = rel(.3), 
                                       hjust = .92,
                                       margin = margin(t = 0, b = -5, unit = "pt"))) +
  patchwork::inset_element(nz_map, .7, .5, 1, 1, align_to = "full")

# Save plot
ggsave("sheep.png", width = 5.8, height = 4.6, units = "in", dpi = 600)  



