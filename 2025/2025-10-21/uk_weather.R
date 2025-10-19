
# Load packages -----------------------------------------------------------

library(tidyverse)
library(geomtextpath)
library(glue)
library(ggarrow)
library(ggtext)
library(rnaturalearth)
library(scales)




# Themes and colors -------------------------------------------------------

main_font <- "Pragati Narrow"
bold_font <- "Pragati Narrow Bold"
col_bg <- "linen"
col_fill <- "ivory2"
col_hi <- "firebrick3"
col_lo <- "blue4"
col_station <- "goldenrod3"
col_station2 <- "yellow"



# Read data ---------------------------------------------------------------

stations <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-21/station_meta.csv")

weather <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-21/historic_station_met.csv") |> 
  left_join(stations)

# Get map of UK
uk <- 
  rnaturalearth::ne_countries(scale = "medium") |> 
  filter(geounit %in% c("United Kingdom"))



# Process data ------------------------------------------------------------

# Filter to stations with full coverage monthly data since 1970
weather_stations <- 
  weather |> 
  filter(!is.na(tmax)) |> 
  filter(year >= 1970) |> 
  filter(n() == (2024-1969) * 12, 
         .by = c(station))

# Estimate temperatures for mean max readings
max_model <- lm(tmax ~ year, data = weather_stations)

# Get predictions for key years (first, mid, last)
max_model_pred <- 
  predict(max_model, newdata = data.frame(year = c(1970, 1997, 2024)))

# Get increase from first to last year in data
max_increase <- last(max_model_pred) - first(max_model_pred)

# Estimate temperatures for mean min readings
min_model <- lm(tmin ~ year, data = weather_stations)

# Get predictions for key years (first, mid, last)
min_model_pred <- 
  predict(min_model, newdata = data.frame(year = c(1970, 1997, 2024)))

# Get increase from first to last year in data
min_increase <- last(min_model_pred) - first(min_model_pred)

# Get extreme values
extreme_temps <- 
  weather_stations |> 
  
  # Highest and lowest monthly max/min respectively
  mutate(record = case_when(
    tmax == max(tmax) ~ "high",
    tmin == min(tmin) ~ "low"
  )) |> 
  
  # Highest and lowest for expected low vs. high months
  mutate(record = case_when(
    tmax == max(tmax) & month == 1 ~ "jan_high",
    tmax == min(tmax) & month == 7 ~ "jul_low",
    .default = record
  ), .by = month) |> 
  drop_na(record) |> 
  
  # Slice to first record years only
  slice_min(order_by = year, by = record)

# Get values for individual records
coldest <- filter(extreme_temps, record == "low")
hottest <- filter(extreme_temps, record == "high")
coldest_jul <- filter(extreme_temps, record == "jul_low")
hottest_jan <- filter(extreme_temps, record == "jan_high")

# Print function for plotting with +/— signs
make_sign <- function(x) {
  if (x < 0) {
    return(paste0("&minus;", abs(x)))
  } else {
    return(paste0("+", x))
  }
}


# Plot data ---------------------------------------------------------------

# Plot UK map with weather stations and record readings
uk_map <- 
  ggplot() +
  geom_sf(data = uk, fill = col_fill, lwd = .5) +
  geom_arrow_curve(data = coldest, 
                   aes(x = lng, xend = -1.6,
                       y = lat, yend = 55.7),
                   color = col_lo, curvature = -.3, 
                   arrow_fins = arrow_cup(angle = 360), resect_fins = 1.5) +
    annotate("textbox", x = .92, y = 55.7, 
             label = glue::glue("<span style='color:{col_lo}'>**Lowest monthly mean**</span> ({make_sign(coldest$tmin)}°C), 
                                **{coldest$station_name}** ({month(coldest$month, label = T, abbr = F)} {coldest$year})"),
             fill = NA, box.color = NA, color = "grey30", family = main_font, size = 3, 
             width = .4, lineheight = .9) +
    geom_arrow_curve(data = filter(extreme_temps, record == "high"), 
                     aes(x = lng, xend = 1.9,
                         y = lat, yend = 50.5),
                     color = col_hi, curvature = -.5, 
                     arrow_fins = arrow_cup(angle = 360), resect_fins = 1.5) +
  annotate("textbox", x = .2, y = 50.1, 
           label = glue::glue("<span style='color:{col_hi}'>**Highest monthly mean**</span> ({make_sign(hottest$tmax)}°C), 
                                **{hottest$station_name}** ({month(hottest$month, label = T, abbr = F)} {hottest$year})"),
           fill = NA, box.color = NA, color = "grey30", family = main_font, size = 3, 
           width = .55, lineheight = .9) +
    geom_arrow_curve(data = filter(extreme_temps, record == "jul_low"), 
                     aes(x = lng, xend = 1.1,
                         y = lat, yend = 58.9),
                     color = col_lo, curvature = -.5, 
                     arrow_fins = arrow_cup(angle = 360), resect_fins = 1.5) +
  annotate("textbox", x = .6, y = 58.5, 
           label = glue::glue("<span style='color:{col_lo}'>**Coldest July**</span> ({make_sign(coldest_jul$tmin)}°C), 
                                **{coldest_jul$station_name}** ({coldest_jul$year})"),
           fill = NA, box.color = NA, color = "grey30", family = main_font, size = 3, 
           width = .4, lineheight = .9) +
    geom_arrow_curve(data = filter(extreme_temps, record == "jan_high"), 
                     aes(x = lng, xend = -6.2,
                         y = lat, yend = 51),
                     color = col_hi, curvature = -.35, 
                     arrow_fins = arrow_cup(angle = 360), resect_fins = 1.5) +
  annotate("textbox", x = -6.9, y = 51.2, 
           label = glue::glue("**<span style='color:{col_hi}'>Hottest January**</span> ({make_sign(hottest_jan$tmax)}°C),
                                **{hottest_jan$station_name}** ({hottest_jan$year})"),
           fill = NA, box.color = NA, color = "grey30", family = main_font, size = 3, 
           width = .45, lineheight = .9) +
  geom_point(data = slice(weather_stations, 1, .by = station), 
             aes(x = lng, y = lat), size = 2.5, color = col_station) +
    geom_point(data = slice(weather_stations, 1, .by = station), 
               aes(x = lng, y = lat), size = 1, color = col_station2) +
  coord_sf(xlim = c(-9, 3)) +
  theme_void()

# Plot line chart of yearly averages per station
# with linear fits and added UK map with annotations
weather_stations |> 
  
  # Summarize values per year and station for plotting lines per station
  summarize(tmax = mean(tmax),
            tmin = mean(tmin),
            .by = c(station, year)) |> 
  
  ggplot() +
  
  # Add custom grid lines and labels (y axis)
  annotate("segment", x = 1970, xend = 2024, y = seq(5, 15, by = 5), lwd = .5, color = "grey50", alpha = .5) +
  annotate("segment", x = 1970, xend = 2024, y = seq(7.5, 15, by = 5), lwd = .25, color = "grey50", alpha = .5) +
  annotate("text", x = 1970, y = seq(5, 15, by = 5), label = paste0(seq(5, 15, by = 5), "°C   "), 
           hjust = 1, family = main_font, size = 4) +
  
  # Add lines per station and year
  geom_line(aes(x = year, y = tmax, group = station), color = col_hi, alpha = .05, lwd = 1) +
  geom_line(aes(x = year, y = tmin, group = station), color = col_lo, alpha = .05, lwd = 1) +
  
  # Add linear fit for max and min temperatures
  geom_textsmooth(aes(x = year, y = tmax, label = "Average maximum temperature"), method = "lm",
                  family = bold_font, vjust = .6, hjust = .9, color = col_hi, lty = 1, lwd = 1, size = 5) +
  geom_textsmooth(aes(x = year, y = tmin, label = "Average minimum temperature"), method = "lm",
                family = bold_font, vjust = .6, hjust = .9, color = col_lo, lty = 1, lwd = 1, size = 5) +
  
  # Add reference lines, arrows and labels to show model temperature increase 
  annotate("segment", x = 1970, xend = 2024, y = max_model_pred[1], lty = 2, color = col_hi) +
  annotate_arrow("arrow", x = 2024.5, 
                 y = c(first(max_model_pred), yend = last(max_model_pred)), 
                 lwd = .7, color = col_hi, arrow_head = arrow_head_wings(), arrow_fins = arrow_head_wings()) +
  annotate("text", x = 2024, y = max_model_pred[2],
           label = paste0("+", round(max_increase, 1), "°C"), 
           hjust = -.2, vjust = .45, family = bold_font, size = 5, color = col_hi) +
  
  annotate("segment", x = 1970, xend = 2024, y = min_model_pred[1], lty = 2, color = col_lo) +
  annotate_arrow("arrow", x = 2024.5, 
                 y = c(first(min_model_pred), yend = last(min_model_pred)), 
                 lwd = .7, color = col_lo, arrow_head = arrow_head_wings(), arrow_fins = arrow_head_wings()) +
  annotate("text", x = 2024, y = min_model_pred[2],
           label = paste0("+", round(min_increase, 1), "°C"), 
           hjust = -.2, vjust = .45, family = bold_font, size = 5, color = col_lo) +
  
  # Add UK map with annotations
  annotation_custom(ggplotGrob(uk_map), 
                    xmin = 2026, ymin = 0, ymax = 23) +
  
  # Customization
  scale_y_continuous(breaks = breaks_pretty(), 
                     labels = label_number(suffix = "°C"), 
                     limits = c(2, 18)) +
  scale_x_continuous(breaks = breaks_pretty(8), 
                     labels = c("", seq(1970, 2020, by = 10), rep("", 5)),
                     limits = c(1970, 2060)) +
  labs(x = NULL, y = NULL, 
       title = "UK weather station temperature readings",
       subtitle = glue::glue("Mean daily temperatures per month from <span style='color:{col_station}'>**{length(unique(weather_stations$station))} weather stations**</span> across the UK (1970&ndash;2024)<br>Both <span style='color:{col_hi}'>**maximum**</span> and <span style='color:{col_lo}'>**minimum**</span> daily temperatures have increased over the years<span style='font-size:8pt;color:grey50'><br>**Data:** UK Met Office via TidyTuesday | **Packages:** {{tidyverse, geomtextpath, ggarrow, ggtext, glue, rnaturalearth, scales}} | **Visualization:** C. Börstell</span>")) +
  theme_minimal(base_size = 15, base_family = main_font, paper = col_bg) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "panel",
        plot.title = element_text(face = "bold", size = rel(2.1), color = "grey10"),
        plot.subtitle = element_textbox(width = .85, halign = 0, color = "grey30", margin = margin(0, 0, -10, 0, unit = "mm")),
        axis.text.x = element_text(vjust = 14),
        axis.text.y = element_blank(),
        plot.margin = margin(5, 1, -4, 5, unit = "mm"))

# Save plot
ggsave("uk_weather.png", width = 8, height = 5, units = "in", dpi = 600)


