
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(scales)
library(sf)
library(rnaturalearth)
library(ggtext)
library(patchwork)
library(cbthemes)



# Custom functions --------------------------------------------------------

# Get distance between two points in kilometers
get_distance <- function(lat.x, lng.x, lat.y, lng.y) {
  as.numeric(st_length(st_sfc(st_linestring(rbind(c(lng.x, lat.x), c(lng.y, lat.y))), crs = 4326))) / 1000
}

# Make a line between two SF points.
# - based on https://gis.stackexchange.com/a/325493
make_line <- function(x1, y1, x2, y2) {
  st_linestring(matrix(c(x1, x2, y1, y2), 2, 2))
}



# Read data ---------------------------------------------------------------

# Get map of world
world <- 
  countries110 |> 
  filter(SOVEREIGNT != "Antarctica") |> 
  st_transform("+proj=robin")

# Get twin cities
cities <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-12/cities.csv")

# Get twin city links
links <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-12/links.csv")
  


# Process data ------------------------------------------------------------

# Calculate distances between twin cities
distances <- 
  links |> 
  left_join(cities |> select(id, name, lat, lng), by = join_by(source == id)) |> 
  left_join(cities |> select(id, name, lat, lng), by = join_by(target == id)) |> 
  mutate(distance = pmap_dbl(list(lat.x, lng.x, lat.y, lng.y), \(y1, x1, y2, x2) get_distance(y1, x1, y2, x2), .progress = T))



# Plot data ---------------------------------------------------------------

# Store max distance between twinned cities
max_dist <- 
  filter(distances, distance == max(distance))

# Plot map of long distances
dist_map <- 
  distances |> 
  filter(distance > 18000) |> 
  mutate(geometry = pmap(list(lng.x, lat.x, lng.y, lat.y), make_line)) |> 
  st_as_sf(crs = "WGS84") |> 
  ggplot() +
  geom_sf(data = world, fill = cb_eggwhite, color = NA) +
  geom_sf(alpha = .3, linewidth = .3, color = cb_khaki) +
  geom_sf(data = \(x) filter(x, distance == max(distance)), 
          color = cb_khaki, linewidth = 6, lineend = "round") +
  annotate("text", x = I(.578), y = I(.42), label = str_glue("{max_dist$name.x} {paste0(rep('—', 8), collapse = '')} {format(round(max_dist$distance), big.mark = ',')} km {paste0(rep('—', 8), collapse = '')} {max_dist$name.y}"), 
           angle = 18, hjust = .5, color = cb_eggwhite, family = "PT Sans Narrow") +
  coord_sf(crs = "+proj=robin") +
  theme_void()

# Plot histogram of distances
dist_hist <- 
  distances |> 
  ggplot() +
  annotate("segment", x = 0, xend = c(20, 20, 3, 3, 3, 3), y = seq(500, 3000, by = 500), 
           linetype = 1, alpha = .2, linewidth = rep(c(.1, .3), 3)) +
  geom_histogram(aes(x = distance/1000, fill = factor(floor(distance/3000))), 
                 binwidth = 1, color = "white", boundary = 0, show.legend = F) +
  annotate("segment", x = 17.9, xend = 20, y = 200,
           color = "grey40", linewidth = 1, lineend = "square") +
  annotate("segment", x = c(17.9, 20), xend = c(17.9, 20), y = c(120, 120), yend = c(200, 200),
           color = "grey40", linewidth = 1) +
  annotate("curve", x = 19, xend = 17, y = 200, yend = 1100, curvature = .2,
           color = "grey40", linewidth = 1) +
  annotate("rect", xmin = 3, xmax = 20, ymin = 1100, ymax = 4000, 
           fill = NA, color = "grey40", linewidth = 1) +
  scale_x_continuous(breaks = breaks_pretty(), labels = label_number(suffix = c("", "", rep("k", 3), ",000"))) +
  scale_y_continuous(breaks = breaks_pretty(5), labels = label_comma(), limits = c(0, 4000)) + 
  scale_fill_manual(values = cbthemes::cb_pal("cb_seq_khaki")) +
  labs(x = "km", y = NULL,
       title = "Distances between Twinned Cities around the World",
       subtitle = str_glue("Based on {format(nrow(distances), big.mark = ',')} twinned cities (or, sister cities) listed on Wikipedia, the majority of them — {format(nrow(filter(distances, distance < 2000)), big.mark = ',')} ({round(nrow(filter(distances, distance < 2000)) / nrow(distances) * 100, 1)}%) — are less than 2,000 km apart. A mere {format(nrow(filter(distances, distance > 18000)), big.mark = ',')} ({round(nrow(filter(distances, distance > 18000)) / nrow(distances) * 100, 1)}%) of them are more than 18,000 km apart, most of them connecting South America with East Asia, or in a few cases connecting Europe with New Zealand.<br><span> </span><span> </span><span> </span><span> </span><span> </span>The greatest distance between two twinned cities is that of **{max_dist$name.x}** & **{max_dist$name.y}** — {format(round(max_dist$distance), big.mark = ',')} km apart."),
       caption = "**Data:** Wikipedia via TidyTuesday | **Packages:** {tidyverse, scales, sf, rnaturalearth, ggtext, patchwork} | **Visualization:** C. Börstell") +
  guides(fill = "none",
         x = guide_axis(cap = "both"),
         y = guide_axis(cap = "both")) +
  theme_cb_base(base_size = 18, base_family = "PT Sans Narrow", paper = cb_offwhite) +
  theme(axis.line = element_line(lineend = "square"),
        axis.ticks = element_line(lineend = "square"),
        axis.title.x = element_text(size = rel(.8), 
                                    hjust = .99, 
                                    vjust = 4),
        plot.title = element_text(size = rel(1.65)),
        plot.subtitle = element_textbox_simple(width = .98, 
                                               lineheight = 1.1,
                                               size = rel(.8)),
        plot.caption = element_markdown(hjust = .55, 
                                        margin = margin(t = -3, b = 0, unit = "mm"),
                                        size = rel(.5)))

# Patch plots together with inset
dist_hist + inset_element(dist_map, 
                          left = .1, 
                          right = 1,
                          bottom = .05,
                          top = 1.2, 
                          align_to = "panel")



# Save plot ---------------------------------------------------------------

ggsave("twin_cities.png", 
       width = 8, height = 7, units = "in", dpi = 600)
