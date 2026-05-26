
# Load packages -----------------------------------------------------------

library(tidyverse)
library(scales)



# Read & process data -----------------------------------------------------

food_beverages <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-05/food_beverages.csv")

sugar <- 
  food_beverages |> 
  drop_na(Sugar)



# Plot data ---------------------------------------------------------------

sugar |> 
  ggplot() +
  annotate("segment",
           x = 1875,
           xend = c(1888, 1896, 1900, 1952),
           y = c(1e3, 1e4, 1e5, 1e6),
           linetype = 3, alpha = .5) +
  annotate("rect", xmin = 1940, xmax = 1945, ymin = 0, ymax = 1e6 * 1.2,
           alpha = .1) +
  annotate("text",
           x = 1942.5,
           y = 16500,
           label = "Italy in World War II",
           angle = -90,
           alpha = .4,
           hjust = 0,
           size = 3) +
  annotate("rect",
           xmin = seq(1950, 1965, by = 7.5),
           xmax = 1972.5,
           ymin = 20000,
           ymax = 300000,
           fill = c("#008C45",
                    "#F4F5F0",
                    "#CD212A")) +
  annotate("text",
           x = 1950,
           y = 7000,
           label = "World War II had a\nmajor negative\nimpact on sugar\nproduction in Italy",
           hjust = 0, 
           size = rel(2), 
           lineheight = .8) +
  geom_path(aes(Year, Sugar), linetype = 6) +
  guides(
    x = guide_axis(cap = "both"),
    y = guide_axis(cap = "both"),
  ) +
  labs(x = NULL, y = NULL,
       title = str_c("Sugar production in Italy", " (", min(sugar$Year), "—", max(sugar$Year), ")"),
       caption = "Data: ISTAT\n      via TidyTuesday") +
  scale_y_log10(labels = label_number(suffix = " tons", big.mark = ",")) +
  theme_classic(base_size = 14, base_family = "Special Elite",
                paper = "snow", ink = "grey20") +
  theme(panel.border = element_blank(),
        plot.caption = element_text(size = rel(.4), hjust = -.3),
        plot.title.position = "plot",
        plot.title = element_text(size = rel(1.4)),
        axis.line = element_line(color = "grey20"))



# Save plot ---------------------------------------------------------------

ggsave("sugar.png", width = 5.7, height = 3.3, units = "in", dpi = 600)



# Alt-text ----------------------------------------------------------------

# An old-timey looking line chart showing "Sugar production in Italy (1877-1986)" with a clear downward spike during the World War II years. Data from ISTAT via TidyTuesday.