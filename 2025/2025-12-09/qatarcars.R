
# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggtext)



# Read data ---------------------------------------------------------------

qatarcars <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-09/qatarcars.csv") |> 
  # Correct error in data (see: https://github.com/profmusgrave/qatarcars/issues/3#issue-3556298770)
  mutate(width = case_when(
    model == "GLB 200" ~ 1.834,
    .default = width
  ))



# Plot data ---------------------------------------------------------------

qatarcars |> 
  ggplot() +
  annotate("segment", x = -(2.5/2), xend = 2.5/2, y = (5/2), 
           linewidth = 5, color = "snow", alpha = .9, lineend = "square") +
  annotate("segment", x = c(-(2.5/2), 2.5/2), y = -(5/2), yend = 5/2, 
           linewidth = 5, color = "snow", alpha = .9) +
  annotate("text", x = 0, y = 0, label = "P", 
           size = 32, family = "Overpass Regular Black", color = "snow") +
  geom_rect(aes(xmin = -(width/2), xmax = width/2, 
                ymin = -(length/2), ymax = length/2), 
            fill = NA, linewidth = .75, linetype = 3, 
            color = "gold",
            show.legend = F) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Qat<span style='color:#8A1538'>ar Cars</span>",
       subtitle = str_glue("{nrow(qatarcars)} car models' length & width <span style='color:#8A1538'> relative to a standard parking space of 2.5×5 meters</span><br>"),
       caption = "**Data:** Musgrave (2025) {qatarcars} via TidyTuesday | **Packages:** {tidyverse, ggtext} | **Visualization:** C. Börstell") +
  coord_fixed() +
  facet_wrap(~toupper(type), ncol = 4, strip.position = "bottom") +
  theme_void(paper = "grey75", base_size = 15, base_family = "Overpass") +
  theme(strip.text = element_text(color = "snow", 
                                  margin = margin(0),
                                  size = rel(1.5), 
                                  family = "Overpass Regular Black"),
        plot.title = element_markdown(family = "Overpass Regular Black", 
                                      size = rel(8.4),
                                      color = "snow"),
        plot.subtitle = element_textbox(width = 1, family = "Overpass Regular Bold", color = "snow"),
        plot.caption = element_markdown(hjust = .5, color = "grey55", family = "Overpass Regular"),
        plot.margin = margin(3, 10, 2, 10, "mm"))



# Save plot ---------------------------------------------------------------

ggsave("qatarcars.png", width = 10, height = 7.5, units = "in", dpi = 600, device = grDevices::png)

