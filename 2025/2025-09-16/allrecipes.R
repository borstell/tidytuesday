
# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggarrow)
library(ggrepel)
library(ggtext)



# Themes and colors -------------------------------------------------------

color_pal <- c("darkred", "orangered3")
title_col <- "brown4"
bg_col <- "floralwhite"

base_font <- "Play"
bold_font <- "Play Bold"



# Read and process data ---------------------------------------------------

# Read data
cuisines <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-16/cuisines.csv")

# Tokenize all words in the ingredient column
all_ingredients <- 
  cuisines |> 
  mutate(ingredient = str_split(ingredients, "\\s+")) |> 
  unnest(ingredient) |> 
  mutate(ingredient = str_to_lower(str_remove_all(ingredient, "[^[:alpha:]]"))) |> 
  mutate(region = case_when(
    str_detect(country, " and New Zealander") ~ str_replace(country, " and New Zealander", "/NZ"),
    str_detect(country, "and") ~ str_replace(country, " and ", "/"),
    country == "Southern Recipes" ~ "Southern",
    country %in% c("Swedish", "Danish", "Norwegian", "Finnish") ~ "Scandinavian",
    country %in% c("German", "Austrian", "Swiss") ~ "Austrian/German/Swiss",
    .default = country
  ))



# Plot data ---------------------------------------------------------------

# Get all mentions of chicken|egg|butter|oil from ingredients
all_ingredients |> 
  filter(str_detect(ingredient, "^butter$|^oil$|^chicken$|^eggs?$")) |> 
  mutate(ingredient = case_when(
    ingredient == "eggs" ~ "egg",
    .default = ingredient)) |> 
  
  # Count by cuisine and z-score the proportions
  count(region, ingredient) |> 
  pivot_wider(names_from = ingredient, values_from = n, values_fill = 0) |> 
  mutate(x = egg - chicken,
         y = butter - oil) |>
  mutate(x = (x - mean(x)) / sd(x),
         y = (y - mean(y)) / sd(y)) |> 
  select(region, x, y) |> 
  
  # Estimate position from center based on mean coordinates
  mutate(size = map2_vec(x, y, \(x, y) 1 + mean(c(abs(x), abs(y))))) |> 
  
  # Plot
  ggplot() +
  
  # Arrows as grid lines
  annotate_arrow(x = c(-.1, -2.5), y = 0, linewidth = c(0, 3), color = color_pal[1]) +
  annotate_arrow(x = c(.1, 3.5), y = 0, linewidth = c(0, 3), color = color_pal[2]) +
  annotate_arrow(x = 0, y = c(.1, 3.25), linewidth = c(0, 3), color = color_pal[2]) +
  annotate_arrow(x = 0, y = c(-.1, -2.5), linewidth = c(0, 3), color = color_pal[1]) +
  
  # Shadow points for data
  geom_point(aes(x, y, size = log(size)+1), shape = 21, fill = bg_col, color = "transparent") +
  
  # Actual data points
  geom_point(aes(x, y, color = x, size = log(size)), shape = 21, alpha = .4) +
  
  # Repel text labels of cuisines
  geom_text_repel(aes(x, y, label = region, size = log(size)), 
                  color = "grey20", bg.color = bg_col, family = bold_font, min.segment.length = .99) +
  
  # Text labels at the ends of the grid arrows
  annotate("richtext", x = -2.8, y = 0, label = "Chicken<br><span style='font-size:30pt'>🍗</span>", family = bold_font, size = 8, color = color_pal[1], label.size = 0, fill = "transparent") +
  annotate("richtext", x = 3.7, y = 0, label = "Egg<br><span style='font-size:25pt'>🥚</span>", family = bold_font, size = 8, color = color_pal[2], label.size = 0, fill = "transparent") +
  annotate("richtext", x = 0, y = 3.5, label = "Butter<br><span style='font-size:40pt'>🧈</span>", family = bold_font, size = 8, color = color_pal[2], label.size = 0, fill = "transparent") +
  annotate("richtext", x = 0, y = -2.85, label = "Oil<br><span style='font-size:25pt'>🫒</span>", family = bold_font, size = 8, color = color_pal[1], label.size = 0, fill = "transparent") +
  
  # Description of data and plot
  annotate("richtext", x = -3.55, y = 3.2, label = "Based on 2,218 recipes categorized by<br>the **cuisine** (country, region, culture),<br>this graph shows the proportional<br>frequency of the ingredients<br>**chicken** vs. **egg** (x-axis) &<br>**butter** vs. **oil** (y-axis)<br>mentioned across<br>recipes of each<br>cuisine", 
           family = base_font, size = 5, hjust = 0, label.size = 0, fill = "transparent") +
  
  # Caption
  annotate("richtext", x = 1.8, y = -2.8, label = "**Visualization:** C. Börstell<br>**Data:** allrecipes.com via {tastyR} & TidyTuesday<br>**Packages:** {ggarrow, ggrepel, ggtext, tidyverse}", 
           family = base_font, size = 3, color = "grey40", hjust = 0, label.size = 0, fill = "transparent") +
  
  # Annotations for each quadrant
  annotate("richtext", x = -2.5, y = 2, label = "[Chicken & Butter]", 
           family = base_font, size = 5, color = "grey70", hjust = 0,
           label.size = 0, fill = "transparent") +
  annotate("richtext", x = -2.5, y = -1.55, label = "[Chicken & Oil]", 
           family = base_font, size = 5, color = "grey70", hjust = 0,
           label.size = 0, fill = "transparent") +
  annotate("richtext", x = 2.5, y = -1.5, label = "[Egg & Oil]", 
           family = base_font, size = 5, color = "grey70", hjust = 1,
           label.size = 0, fill = "transparent") +
  annotate("richtext", x = 2.5, y = 2, label = "[Egg & Butter]", 
           family = base_font, size = 5, color = "grey70", hjust = 1,
           label.size = 0, fill = "transparent") +
  
  # Annotate Scandinavian outlier with text...
  annotate("richtext", x = 2, y = 3.2, label = "Scandinavian cuisine<br>is a clear outlier", 
           family = bold_font, size = 4, color = "grey50", label.size = 0, hjust = 0, fill = "transparent") +
  
  # ... and arrow
  annotate("curve", x = 3.1, xend = 3.8, y = 3.1, yend = 3.75, 
           curvature = .45, arrow = arrow(length = unit(2, "mm"), type = "closed"),
           color = "grey50") +
  
  # Customization
  scale_color_gradient(low = color_pal[1], high = color_pal[2]) +
  scale_size_continuous(range = c(1.9, 5)) +
  guides(color = "none", size = "none") +
  labs(title = "The chicken 🐓  or the egg 🥚?") +
  coord_cartesian(xlim = c(-3.5, 4)) +
  theme_void() +
  theme(plot.background = element_rect(color = "transparent", fill = bg_col),
        plot.margin = margin(10, 3, 5, 1, "mm"),
        plot.title = element_markdown(family = bold_font, 
                                      size = rel(4.5), 
                                      hjust = .5,
                                      color = title_col))

# Save plot
ggsave("allrecipes.png", width = 10, height = 10, units = "in", dpi = 600)

