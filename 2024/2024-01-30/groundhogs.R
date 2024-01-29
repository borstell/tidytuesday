
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggtext)
library(ggforce)



# Read data ---------------------------------------------------------------

groundhogs <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/groundhogs.csv")
predictions <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/predictions.csv")



# Process data ------------------------------------------------------------

# Get actual predictions
shadow <- 
  predictions |> 
  left_join(groundhogs, by = join_by(id)) |> 
  filter(!is.na(shadow))

# Filter out the top pessimistic predictors (with enough predictions)
top <- shadow |> 
  mutate(seen = sum(shadow), .by = id) |> 
  slice(1, .by = id) |> 
  mutate(pessimistic = seen/predictions_count) |> 
  filter(predictions_count>=40) |> 
  arrange(desc(predictions_count)) |> 
  mutate(rank = row_number()) |> 
  mutate(name = case_when(
    name == "General Beauregard Lee" ~ "Gen. Beauregard Lee",
    .default = name
  ))



# Plot data ---------------------------------------------------------------

# Themes
brown1 <- "#ad6715"
brown2 <- "#91550f"
brown3 <- "#78460c"
brown4 <- "#663a06"
brown5 <- "#4f2c03"


# Map data
m <- ggplot2::map_data("world") |> 
  as_data_frame()


# Plot map
ggplot() +
  
  # Map
  geom_polygon(data=m, aes(long, lat, group=group), color="goldenrod2", fill="goldenrod2", alpha=.7) +
  
  # Shadows
  geom_segment(data=top, aes(x=longitude, xend=longitude+pessimistic*20, y=latitude+.1, yend=latitude+pessimistic*5), color="grey45", size=4, alpha=.95) +
  ggforce::geom_ellipse(data=top, aes(x0=longitude+pessimistic*20, y0=latitude+pessimistic*5, a=.45, b=.3, angle=6), color="transparent", fill="grey45", alpha=.95) +
  ggforce::geom_ellipse(data=top, aes(x0=longitude+pessimistic*20+1.5, y0=latitude+pessimistic*5+.4, a=.3, b=.8, angle=1.7), color="transparent", fill="grey45", alpha=.8) +
  geom_segment(data=top, aes(x=longitude+pessimistic*20, xend=longitude+pessimistic*20+.9, y=latitude+pessimistic*5, yend=latitude+pessimistic*5+.22), color="grey45", size=1, alpha=.75) +
  
  # Logs
  ggforce::geom_ellipse(data=top, aes(x0=longitude, y0=latitude, a=.8, b=.2, angle=0), color=brown4, fill=brown3) +
  geom_rect(data=top, aes(xmin=longitude-.9, xmax=longitude+.9, ymin=latitude, ymax=latitude+pessimistic*10), fill=brown5) +
  geom_rect(data=top, aes(xmin=longitude-.9, xmax=longitude+.7, ymin=latitude, ymax=latitude+pessimistic*10), fill=brown4) +
  geom_rect(data=top, aes(xmin=longitude-.9, xmax=longitude+.5, ymin=latitude, ymax=latitude+pessimistic*10), fill=brown3, alpha=.3) +
  geom_rect(data=top, aes(xmin=longitude-.9, xmax=longitude+.3, ymin=latitude, ymax=latitude+pessimistic*10), fill=brown2, alpha=.2) +
  geom_rect(data=top, aes(xmin=longitude-.9, xmax=longitude+.1, ymin=latitude, ymax=latitude+pessimistic*10), fill=brown1, alpha=.2) +
  geom_rect(data=top, aes(xmin=longitude-.9, xmax=longitude-.1, ymin=latitude, ymax=latitude+pessimistic*10), fill=brown1, alpha=.1) +
  ggforce::geom_ellipse(data=top, aes(x0=longitude, y0=latitude+pessimistic*10, a=.8, b=.2, angle=0), color=brown2, fill=brown1) +

  # Signs
  geom_segment(data=top, aes(x=longitude, xend=longitude, y=latitude+pessimistic*10, yend=latitude+pessimistic*10+1), color="grey10") +
  geom_point(data=top, aes(x=longitude, y=latitude+pessimistic*10+1.3), shape=16, color="grey20", size=9.5) +
  geom_point(data=top, aes(x=longitude, y=latitude+pessimistic*10+1.3), shape=16, color="#FAFAFA", size=8) +
  geom_text(data=top, aes(x=longitude-.04, y=latitude+pessimistic*10+1.31, label=rank), family="Sonsie One") +
  
  # Text
  ggtext::geom_textbox(aes(x=-98, y=39, label="Pessimistic Groundhog Day"), 
                       fill="transparent", box.color="transparent", 
                       family="Sonsie One", size=8.5, color=brown4,
                       maxwidth = 1) +
  ggtext::geom_textbox(aes(x=-96, y=33, label="How often do the seven most pessimistic groundhogs see their own shadow?"), 
                       fill="transparent", box.color="transparent", 
                       family="American Typewriter Bold", size=4, color=brown3,
                       maxwidth = 1) +
  
  # Legend
  
  geom_point(data=top, aes(x=-74, y=38-rank*1.5), shape=16, color="grey20", size=8) +
  geom_point(data=top, aes(x=-74, y=38-rank*1.5), shape=21, color="transparent", fill="#FAFAFA", size=6.5) +
  geom_text(data=top, aes(x=-74-.05, y=38-rank*1.5, label=rank), color="grey10", family="Sonsie One", size=3) +
  ggtext::geom_textbox(data=top, aes(x=-66, y=38-rank*1.52, label=paste0(name, " (", round(pessimistic*100), "%)")), 
                       size=3.2, family="American Typewriter", box.color="transparent", fill="transparent", maxwidth = 2) +
  
  ggtext::geom_textbox(aes(x=-66, y=25.5, 
                           label="Data: groundhog-day.com via TidyTuesday Packages: {tidyverse,ggforce,ggtext} Visualization: @c_borstell"),
                       size=2, family="American Typewriter", box.color="transparent", fill="transparent", color="grey50") +
  
  coord_quickmap(xlim = c(min(top9$longitude)-15, max(top9$longitude)+16),
                 ylim = c(min(top9$latitude)-8, max(top9$latitude)+7)) +
  theme_void() +
  theme(plot.background = element_rect(fill="#FCFCCC", color="white"),
        panel.background = element_blank())

# Save plot
ggsave("./groundhogs.jpg", width = 7, height = 5.32, unit = "in", dpi = 600)
