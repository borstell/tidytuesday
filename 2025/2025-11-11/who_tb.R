
# Load packages -----------------------------------------------------------

library(tidyverse)



# Read data ---------------------------------------------------------------

who_tb_data <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-11/who_tb_data.csv")



# Plot data ---------------------------------------------------------------

who_tb_data |> 
  filter(year >= 2012) |> 
  ggplot() +
  annotate("segment", x = 2012, xend = 2023, y = seq(250, 1250, by = 250), 
           color = "grey30", linewidth = .3) +
  annotate("segment", x = 2012, xend = 2023, y = 0 , 
           color = "grey90", linewidth = 1) +
  annotate("text", x = seq(2013, 2023, by = 2), y = 0, label = seq(2013, 2023, by = 2),
           vjust = 1.75, color = "grey70", size = 4) +
  geom_line(aes(x = year, y = e_inc_100k, group = country), 
            linewidth = .8, alpha = .2) +
  geom_line(data = \(x) filter(x, country == "Marshall Islands"),
            aes(x = year, y = e_inc_100k), color = "coral3",
            linewidth = 1.25) +
  annotate("text", x = 2023.2, y = filter(who_tb_data, country == "Marshall Islands", year == 2023)$e_inc_100k, label = "Marshall\nIslands", family = "Helvetica Bold", 
           size = 4, color = "coral3", hjust = 0, lineheight = .7) +
  coord_cartesian(ylim = c(0, 1200), xlim = c(2013, 2025)) +
  labs(x = NULL, y = NULL,
       title = "Estimated incidence of tuberculosis", 
       subtitle = "Per 100 000 population\n",
       caption = "Data: World Health Organization via {getTBinR} and Sam Abbott via TidyTuesday | Packages: {tidyverse, scales} | Visualization: C. Börstell") +
  theme_minimal(paper = "grey15", ink = "grey98",
                base_size = 12, base_family = "Helvetica") +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y.left = element_text(size = rel(1.2), margin = margin(0, 2, 0, 0, "pt")),
        axis.text.x = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", color = "white", size = rel(1.4)),
        plot.caption = element_text(color = "grey40", size = rel(.3), 
                                    hjust = 0, vjust = 10),
        plot.margin = margin(5, 5, 0, 5, "mm"))

# Save plot
ggsave("who_tb.png", width = 4.5, height = 7, units = "in", dpi = 600)
