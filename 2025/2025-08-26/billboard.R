library(tidyverse)
library(glue)
library(ggtext)



# Themes and colors -------------------------------------------------------


bg_col <- "cornsilk"
text_cols <- c("dodgerblue", "goldenrod", "palegreen3", "firebrick", 
              "cyan4", "deeppink4", "darkseagreen4", "purple4")

label_cols <- c("gold", "yellow", "navy", "darkgreen", 
                "dodgerblue4", "pink", "sandybrown", "hotpink3")



# Read data ---------------------------------------------------------------


billboard <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-26/billboard.csv")



# Process data ------------------------------------------------------------


billboard_durs <- 
  billboard |> 
  select(song, artist, date, length_sec) |> 
  mutate(decade = paste0(str_sub(as.character(year(date), -1), 1, 3), "0")) |> 
  mutate(avg_sec = mean(length_sec), .by = decade) |> 
  mutate(mins = avg_sec %/% 60,
         secs = round(avg_sec %% 60)) |> 
  mutate(time_label = paste0(str_pad(mins, pad = "0", width = 2), ":", str_pad(secs, pad = "0", width = 2)))



# Plot data ---------------------------------------------------------------

billboard_durs |> 
  slice(1, .by = decade) |> 
  ggplot() +
  annotate("segment", x = 0, xend = 5 * 60, y = 1.5, linewidth = 12, color = "gray10") +
  annotate("richtext", x = 0, y = 0, label = "⏺", 
           fill = NA, label.color = NA, color = "white", size = 20, vjust = .615) +
  geom_richtext(aes(x = 0, y = 0, label = "◒"), 
           fill = NA, label.color = NA, color = sample(label_cols, 8), size = 12.5, vjust = .665) +
  geom_segment(aes(x = 0, xend = avg_sec, y = 3, color = avg_sec), linewidth = 3) +
  geom_point(aes(x = avg_sec, y = 3, color = avg_sec), size = 9) +
  geom_point(aes(x = avg_sec, y = 3), color = bg_col, size = 4) +
  geom_richtext(aes(x = 0, y = 0, label = glue("{decade}<br>⏺<br>{time_label}")), 
                fill = NA, label.color = NA, color = sample(text_cols, 8), 
                size = 2, family = "Caprasimo", vjust = .6) +
  scale_x_continuous(expand = c(-0.01, -0.01)) +
  scale_color_gradient(low = "goldenrod1", high = "firebrick3") +
  coord_radial(direction = -1, inner.radius = 0) +
  guides(color = "none") +
  facet_wrap(~glue("<span style='color:firebrick4;font-size:18pt'>{decade}</span><br>({time_label})")) +
  labs(title = "Billboard Hot 100 Number Ones",
       subtitle = "Mean song length by decade (mins:secs)\n",
       caption = "Data: Billboard Hot 100\nNumber Ones Database\nvia TidyTuesday\nPackages: {glue, ggtext, tidyverse}\nVisualization: C. Börstell") +
  theme_void() +
  theme(strip.text = element_markdown(family = "Caprasimo"),
        plot.title = element_text(family = "Caprasimo", size = rel(2.4)),
        plot.subtitle = element_text(family = "Corben", size = rel(1.5), color = "grey25"),
        plot.caption = element_text(family = "Corben", hjust = 1, vjust = 55, size = rel(.5)),
        plot.margin = margin(5, 10, 0, 0, "mm"))



# Save plot ---------------------------------------------------------------

ggsave("billboard.png", width = 7, height = 8, units = "in", bg = bg_col, dpi = 600)


# Alt-text: Billboard Hot 100 Number Ones: Mean song length by decade (mins:secs). A visualization showing each decade 1950 to 2020 as an LP vinyl record with a curved line at its circumference showing the mean song length per decade. Songs averaged under 3 mins in the 1950s-60s, increased to around 4 mins in the 1980s-2000s, then decreased to around 3.5 mins in the 2010s-20s. Data: Billboard Hot 100 Number Ones Database via TidyTuesday; Packages: {glue, ggtext, tidyverse}; Visualization: C. Börstell 
