
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(flagrant)
library(ggtext)



# Set themes and colors ---------------------------------------------------

swe_colors <- flagrant::get_palette("Sweden")

# https://fonts.google.com/specimen/Lexend
font <- "Lexend-Bold"



# Read data ---------------------------------------------------------------

life_expectancy <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-05/life_expectancy.csv")



# Process data ------------------------------------------------------------

sweden <- life_expectancy |> 
  filter(Entity=="Sweden") |> 
  rename("year" = Year) |> 
  mutate(le = LifeExpectancy)



# Functions for gridline endpoints ----------------------------------------

hends <- function(hs, df, min=0) {
  hvec <- c()
  for (h in hs) {
    if (h > min(df$le)) {
      hvec <- c(hvec, max(filter(df, le<h)$year))
    } else {
      hvec <- c(hvec, min)
    }
  }
  return(hvec)
}

vends <- function(vs, df) {
  vvec <- c()
  for (v in vs) {
    vvec <- c(vvec, filter(df, year==v)$le)
  }
  return(vvec)
}



# Plot data ---------------------------------------------------------------

# Title and subtitle
title <- ggplotGrob(
  ggplot() +
    labs(title="Life Expectancy in Sweden", subtitle=" Years 1751—2021") +
    theme_void(base_family=font) +
    theme(plot.title = element_textbox(size=rel(5), hjust=0, color=swe_colors[1], width=1.6),
          plot.subtitle = element_text(size=rel(2.5), hjust=0, color=swe_colors[1], vjust=-1),
          plot.background = element_blank(),
          panel.background = element_blank())
)

# Caption
caption <- ggplotGrob(
  ggplot() +
    labs(caption="Data: Our World in Data (via TidyTuesday)<br>Packages: {tidyverse,flagrant,ggtext}<br>Visualization: @c_borstell") +
    theme_void(base_family=font) +
    theme(plot.caption = element_textbox(size=rel(.7), hjust=0, color=swe_colors[1], lineheight = 1.5),
          plot.background = element_blank(),
          panel.background = element_blank())
)

# Main plot
ggplot() +
  geom_area(data=sweden, aes(x=year, y=le), fill=swe_colors[1]) +
  
  # Horizontal gridlines
  geom_text(aes(x=2015, y=seq(10, 80, by=10), label=seq(10, 80, by=10)), 
            family=font, size=rel(5), color=swe_colors[2]) +
  geom_segment(aes(x=hends(seq(10, 80, by=10), sweden, 1751), 
                   xend=2010, 
                   y=seq(10, 80, by=10), 
                   yend=seq(10, 80, by=10)), 
               color=swe_colors[2], linewidth=.2, lty=3) +
  
  # Vertical gridlines
  geom_text(aes(x=seq(1800, 2000, by=50), y=2, label=seq(1800, 2000, by=50)), 
            family=font, size=rel(5), color=swe_colors[2]) +
  geom_segment(aes(x=seq(1800, 2000, by=50), 
                   xend=seq(1800, 2000, by=50), 
                   y=4, 
                   yend=vends(seq(1800, 2000, by=50), sweden)), 
               color=swe_colors[2], linewidth=.2, lty=3) +
  
  scale_x_continuous(expand = c(-0.001, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void(base_family = font) +
  theme(panel.background = element_rect(fill=swe_colors[2], color=NA)) +
  annotation_custom(grob = title, xmin=1760, xmax=1880, ymin=50, ymax=81) +
  annotation_custom(grob = caption, xmin=1770, xmax=1880, ymin=48, ymax=81)

# Save plot
ggsave("./life_expectancy_sweden.jpg", width=10, height=7, units="in", dpi=600)

