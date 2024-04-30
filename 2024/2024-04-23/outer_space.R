
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggforce)


# Read and summarize data -------------------------------------------------

outer_space_objects <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-23/outer_space_objects.csv") |> 
  summarize(n = sum(num_objects), .by = Entity) |> 
  arrange(desc(n)) |> 
  filter(Entity!="World") |> 
  mutate(perc = round(n/sum(n), 2)*100)


# Get map data ------------------------------------------------------------

globe_data <- ggplot2::map_data("world")

lake_data <- ggplot2::map_data("lakes")


# Plot map ----------------------------------------------------------------

globe <- 
  ggplotGrob(
  ggplot() +
  geom_polygon(data=globe_data, aes(long, lat, group=group), color="green4", fill="darkgreen", alpha=.8, linewidth=.3) +
  geom_polygon(data=lake_data, aes(long, lat, group=group), color="transparent", fill="transparent") +
  coord_map(projection = "orthographic") +
  theme_void() +
  theme(plot.background = element_rect(color="transparent", fill="transparent"))
  )

globe_in_space <- 
  ggplotGrob(
  ggplot() +
  geom_circle(aes(x0=1, y0=1, r=1), fill="lightblue1", color="lightblue1", linewidth=15) +
  annotation_custom(globe) +
  coord_equal() +
  theme_void()
  )


# Plot map in space -------------------------------------------------------

ggplot() +
  annotation_custom(globe_in_space, ymin = 1, ymax = 6.2, xmin = .7, xmax = 1.8) +
  
  # Bullet backgrounds
  geom_point(aes(x=2.2, y=7.5-seq(1, 5, by=1)), size=30, color="white", show.legend = F) +
  
  # Japan
  geom_point(aes(x=1.6, y=4.65), color="white", size=10) +
  geom_curve(aes(x=1.6, xend=2.2, y=4.65, yend=2.5), color="white", linewidth=6, curvature = -.1) +
  geom_curve(aes(x=1.6, xend=2.2, y=4.65, yend=2.5), color="skyblue2", linewidth=2, curvature = -.1) +
  
  # UK
  geom_point(aes(x=1.2, y=2.4), color="white", size=10) +
  geom_curve(aes(x=1.2, xend=2.2, y=2.4, yend=3.5), color="white", linewidth=6, curvature = .2) +
  geom_curve(aes(x=1.2, xend=2.2, y=2.4, yend=3.5), color="skyblue2", linewidth=2, curvature = .2) +
  
  # China 
  geom_point(aes(x=1.65, y=3.6), color="white", size=10) +
  geom_curve(aes(x=1.65, xend=2.25, y=3.6, yend=4.5), color="white", linewidth=6, curvature = -.3) +
  geom_curve(aes(x=1.65, xend=2.25, y=3.6, yend=4.5), color="skyblue2", linewidth=2, curvature = -.3) +
  
  # Russia
  geom_point(aes(x=1.5, y=3.4), color="white", size=10) +
  geom_curve(aes(x=1.5, xend=2.25, y=3.4, yend=5.5), color="white", linewidth=6, curvature = -.4) +
  geom_curve(aes(x=1.5, xend=2.25, y=3.4, yend=5.5), color="skyblue2", linewidth=2, curvature = -.4) +
  
  # USA
  geom_point(aes(x=.85, y=4), color="white", size=10) +
  geom_curve(aes(x=.85, xend=2.25, y=4, yend=6.5), color="white", linewidth=6, curvature = -.3) +
  geom_curve(aes(x=.85, xend=2.25, y=4, yend=6.5), color="skyblue2", linewidth=2, curvature = -.3) +

  # Locations fill
  geom_point(aes(x=1.6, y=4.65), color="skyblue2", size=6) +
  geom_point(aes(x=1.2, y=2.4), color="skyblue2", size=6) +
  geom_point(aes(x=1.65, y=3.6), color="skyblue2", size=6) +
  geom_point(aes(x=1.5, y=3.4), color="skyblue2", size=6) +
  geom_point(aes(x=.85, y=4), color="skyblue2", size=6) +
  
  # Labels
  geom_text(aes(x=2.15, y=7.5-seq(1, 5, by=1), 
                 label=paste0(outer_space_objects[1:5,]$Entity, " (", outer_space_objects[1:5,]$perc, "%)")), 
             family="Play-Bold", color="skyblue2", hjust = 0, nudge_x = .2, size=rel(8)) +
  
  # Bullets fill
  geom_point(aes(x=2.2, y=7.5-seq(1, 5, by=1)), size = 20, color="skyblue2", show.legend = F) +
  
  # Rank numbers
  geom_text(aes(x=2.2, y=7.5-seq(1, 5, by=1), 
                label=1:5), 
            family="Play-Bold", color="grey5", size=rel(10)) +
  scale_color_identity() +
  scale_alpha_identity() +
  xlim(0.8, 3) +
  ylim(1, 7) +
  labs(title="Outer Space Objects", 
       subtitle=" Top 5 countries by % of objects ever sent to outer space",
       caption="Data: UN Office for Outer Space Affairs (2024) via Our World in Data & TidyTuesday\nPackages: {tidyverse,ggforce} | Visualization: @c_borstell") +
  theme_void(base_size=16, base_family="Play-Bold") +
  theme(plot.background = element_rect(fill="grey5", color="grey5"),
        plot.title = element_text(size=rel(5), color="skyblue1"),
        plot.subtitle = element_text(size=rel(1.8), color="skyblue4"),
        plot.caption = element_text(size=rel(.8), color="skyblue4", vjust = 25),
        plot.margin = margin(10, 15, -10, 15, "mm"))

# Save plot
ggsave("outer_space.jpg", width=12, height=10, units="in", dpi=600)
