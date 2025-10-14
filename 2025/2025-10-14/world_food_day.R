
# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggtext)
library(patchwork)
library(rnaturalearth)
library(scales)



# Themes and colors -------------------------------------------------------

font <- "Lato"

options(
  ggplot2.discrete.fill = c("Eastern Europe" = "coral2", 
                            "Northern Europe" = "skyblue3", 
                            "Southern Europe" = "goldenrod2",
                            "Western Europe" = "palegreen3")
)



# Read data ---------------------------------------------------------------

# Read FAO data
food_security <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-14/food_security.csv")

# European countries by area: https://unstats.un.org/unsd/methodology/m49/
european_areas <- 
  tibble(
    country = c(
      c("Belarus", "Bulgaria", "Czechia", "Hungary", "Poland", 
        "Moldova", "Romania", "Russia", "Slovakia", "Ukraine"),
      c("Åland Islands", "Denmark", "Estonia", "Faroe Islands", "Finland", 
        "Guernsey", "Iceland", "Ireland", "Isle of Man", "Jersey", 
        "Latvia", "Lithuania", "Norway", "Svalbard and Jan Mayen Islands", "Sweden", 
        "United Kingdom"),
      c("Albania", "Andorra", "Bosnia and Herzegovina", "Croatia", "Gibraltar", 
        "Greece", "Holy See", "Italy", "Malta", "Montenegro",
        "North Macedonia", "Portugal", "San Marino", "Republic of Serbia", "Slovenia",
        "Spain"),
      c("Austria", "Belgium", "France", "Germany", "Liechtenstein",
        "Luxembourg", "Monaco", "Netherlands", "Switzerland")),
    area = c(
      rep("Eastern Europe", 10),
      rep("Northern Europe", 16),
      rep("Southern Europe", 16),
      rep("Western Europe", 9)
    )
  )

# Get map of region and join with countries/areas
europe <- 
  ne_countries(scale = "medium", continent = c("Europe", "Asia", "Africa")) |> 
  left_join(european_areas, by = join_by(sovereignt == country))



# Process data ------------------------------------------------------------

# Filter to specific item and areas (in Europe)
food_insecurity <- 
  food_security |> 
  filter(Item == "Prevalence of moderate or severe food insecurity in the total population (percent) (annual value)") |> 
  filter(str_detect(Area, " Europe"), str_detect(Area, "America", negate = T))

# Filter to specific item and the European area as a whole
euro_avg <- 
  food_security |> 
  filter(Item == "Prevalence of moderate or severe food insecurity in the total population (percent) (annual value)") |> 
  filter(Area == "Europe")



# Plot data ---------------------------------------------------------------

# Map over areas and get area countries filled
area_maps <- 
  map(unique(food_insecurity$Area), 
      \(x)
      ggplot() +
      geom_sf(data = filter(europe, area == x), aes(fill = area), show.legend = F, alpha = .9, color = "grey80") +
      geom_sf(data = filter(europe, area != x | is.na(area)), fill = "grey98", color = "grey80", show.legend = F, alpha = .9) +
      coord_sf(xlim = c(-21.5, 43), ylim = c(35, 72)) +
      theme_void() +
      theme(plot.margin = margin(0, -20, 0, -10, "mm"))
      )

# Map over areas and plot food insecurity by area
area_plots <- 
  map(unique(food_insecurity$Area), \(x)
  ggplot(filter(food_insecurity, Area == x)) +
  geom_ribbon(aes(Year_Start, ymin = CI_Lower, ymax = CI_Upper, fill = Area), 
              alpha = .4, show.legend = F) +
  geom_line(aes(Year_Start, Value)) +
  geom_line(data = mutate(euro_avg, Area = x), aes(Year_Start, Value), linetype = "dotted") +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     limits = c(0, 12.5)) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  facet_wrap(~Area) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 16, base_family = font) +
  theme(axis.text.x = element_text(size = rel(1.2), vjust = 2),
        strip.text = element_text(face = "bold", color = "grey20")))

# Patch plots together with {patchwork}
(area_plots[[2]] + area_maps[[2]]) /
  (area_plots[[1]] + area_maps[[1]]) /
  (area_plots[[4]] + area_maps[[4]]) /
  (area_plots[[3]] + area_maps[[3]]) +
  plot_annotation(title = "Prevalence of moderate or severe food insecurity in the total population of European regions", 
                  subtitle = "<br>*Annual percentages (lines) and confidence intervals (ribbons)* — *Dotted lines show European average*<span style='font-size:6.75pt;color:grey50'><br><br>**Data:** *The Food and Agriculture Organization of the United Nations* (FAO) | **Packages:** {tidyverse, ggtext, patchwork, rnaturalearth, scales} | **Visualization:** C. Börstell</span>",
                  theme = theme(plot.title = element_textbox(face = "bold", size = rel(2), width = 1),
                                plot.subtitle = element_markdown(),
                                plot.margin = margin(8, 5, 5, 5, "mm")))

# Save plot
ggsave("world_food_day.png", width = 8, height = 10, dpi = 600)

