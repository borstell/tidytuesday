library(tidyverse)

monsters <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-27/monsters.csv")

monsters |> 
  filter(str_detect(size, " ", negate = T)) |> 
  mutate(name_len = nchar(name)) |> 
  ggplot() +
  geom_boxplot(aes(x = name_len, y = fct_rev(size), fill = size), 
               width = .1, position = position_nudge(y = .25), 
               outliers = F, show.legend = F) +
  geom_point(aes(x = name_len, y = fct_rev(size), fill = size), 
             position = position_jitter(width = .5, height = .1), 
             alpha = .3, show.legend = F, color = "grey20", shape = 21) +
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(6, "YlOrRd"))) +
  scale_x_continuous(breaks = seq(0, 30, by = 5)) +
  labs(x = "\nNumber of characters in name", y = NULL, 
       title = "Dungeons and Dragons Monsters", 
       subtitle = "Monster names (no of characters) and their size", 
       caption = "Data: Dungeons & Dragons System Reference Document via TidyTuesday\nPackages: {tidyverse,RColorBrewer}\nVisualization: @c_borstell") +
  theme_minimal(base_size = 15, base_family = "IBM Plex Sans") +
  theme(plot.title.position = "plot",
        plot.title = element_text(family = "IBMPlexSans-Bold"),
        plot.subtitle = element_text(family = "IBMPlexSans-LightItalic", size = rel(.9)),
        plot.background = element_rect(color = "transparent", fill = "grey99"),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_text(family = "IBMPlexSans-ThinItalic", size = rel(.5)))

ggsave("dnd_monster_size.jpg", 
       width = 6, height = 4.5, units = "in", dpi = 600, device = grDevices::jpeg)
        