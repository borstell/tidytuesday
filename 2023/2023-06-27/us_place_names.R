# Libraries ---------------------------------------------------------------

library(tidyverse)
library(maps)
library(janitor)
library(stringi)
library(patchwork)


# Read data ---------------------------------------------------------------

# Get Sweden place names
swe <- # read data from https://sv.wikipedia.org/wiki/Lista_över_Sveriges_tätorter %>% 
  janitor::clean_names() %>% 
  mutate(feature_name = stringi::stri_trans_general(tatort, "Latin-ASCII")) %>% 
  select(feature_name, tatort)

# Get Norway place names
nor <- # download and read data from https://www.ssb.no/befolkning/folketall/statistikk/tettsteders-befolkning-og-areal %>% 
  janitor::clean_names() %>% 
  drop_na(tettsted) %>% 
  mutate(tettsted = str_sub(tettsted, start=6)) %>% 
  mutate(feature_name = stringi::stri_trans_general(tettsted, "Latin-ASCII")) %>% 
  select(feature_name, tettsted)

# Get US place names
us_place_names <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_names.csv") %>% 
  left_join(swe) %>% 
  left_join(nor)

# Filter out non-continental US locations
swe_nor <- us_place_names %>% 
  filter(!is.na(tatort) | !is.na(tettsted)) %>% 
  filter(!state_name %in% c("United States Virgin Islands",
                            "Puerto Rico")) %>% 
  mutate(country = if_else(is.na(tatort), "Norway", "Sweden"))

# Load US map
us_map <- map(database = "usa", plot = F)



# Plotting ----------------------------------------------------------------

# Plot a faceted density map
swe_nor_map <- swe_nor %>% 
  ggplot() +
  geom_polygon(data=us_map, aes(long, lat), 
               fill="lightblue", 
               alpha=1) +
  geom_point(aes(prim_long_dec, prim_lat_dec), 
             alpha=.5, 
             color="dodgerblue", 
             size=.2) +
  geom_density2d_filled(aes(prim_long_dec, prim_lat_dec), 
                        alpha=.5,
                        show.legend = F) +
  scale_fill_manual(values=c("transparent", "grey97", "grey92", "lightpink", "lightpink2", "lightpink3", "lightpink4")) +
  coord_quickmap() +
  theme_void(base_size=15, base_family = "Futura-Bold") +
  theme(strip.text = element_text(color="pink3", size=rel(1), hjust=1)) +
  facet_wrap(~country, ncol=1)

# Plot a horizontal bar chart of top-6 place names
swe_nor_bars <- swe_nor %>% 
  count(feature_name, tatort, tettsted, country, sort = T) %>% 
  group_by(country) %>% 
  slice_max(n = 6, order_by = n) %>% 
  mutate(label = if_else(tatort=="Brändön" & !is.na(tatort), 
                         "Brandon \n(Brändön)", 
                         feature_name)) %>% 
  ggplot() +
  geom_col(aes(x=fct_reorder(label,n), y=n),
           color="pink4", fill="lightpink") +
  geom_text(aes(x=fct_reorder(label,n), y=n-2, 
                label=str_pad(n,width = 2, pad = " ")),
            color="pink3",
            family="Futura-Bold") +
  coord_flip() +
  theme_void(base_size=15, base_family = "Futura-Bold") +
  theme(strip.text = element_blank(),
        axis.text.y = element_text(size=10, 
                                   hjust=1, 
                                   margin = margin(r=-2, unit="mm"),
                                   color="pink3")) +
  facet_wrap(~country, scales = "free_y", ncol=1)

# Combine plots
swe_nor_map + swe_nor_bars +
  patchwork::plot_annotation(title="Top-6 Norwegian & Swedish place names in the US", 
                             caption = "Data: US Board of Geographic Names, Wikipedia & ssb.no | Packages: {janitor,maps,patchwork,stringi,tidyverse} | Visualization: @c_borstell",
                             theme = theme(plot.title = element_text(family = "Futura-Bold",
                                                                     color="pink4", 
                                                                     hjust=.5, 
                                                                     size=rel(1.5)),
                                           plot.caption = element_text(family = "Futura-Medium",
                                                                       color="lightpink", 
                                                                       hjust=.5, 
                                                                       size=rel(.4))))

# Save plot
ggsave(width = 8, height = 5, units = "in", dpi = 600)
