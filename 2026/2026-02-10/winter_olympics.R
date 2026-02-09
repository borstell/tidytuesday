
# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggrepel)
library(marquee)



# Read data ---------------------------------------------------------------

schedule <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-02-10/schedule.csv")


# Process and plot data ---------------------------------------------------

# Plot as a curling sheet
schedule |> 
  filter(is_medal_event) |> 
  count(discipline_name, sort = T) |> 
  mutate(surface = case_when(
    str_detect(discipline_name, "Skating|Luge|Bob|Skeleton|Curling|Ice") ~ "Ice",
    .default = "Snow"
  )) |> 
  mutate(event = case_when(
    str_detect(discipline_name, "Mountaineering") ~ "Ski Mountain-eering",
    .default = discipline_name
  )) |> 
  mutate(text_col = case_when(
    surface == "Ice" ~ "black",
    .default = "white"
  )) |> 
  mutate(x = c(2.5, rep(1:4, 3), 1:3)) |> 
  ggplot() +
  annotate("point", x = 2.5, y = 45, size = c(6, 20), color = c(I("red"), I("blue")), stroke = c(5, 7), shape = 21, alpha = .4) +
  geom_vline(xintercept = 2.5, alpha = .1) +
  geom_hline(yintercept = 45, alpha = .1) +
  geom_hline(yintercept = 30, alpha = .1, color = "red", linewidth = 1) +
  geom_point(aes(x = x, y = n, color = surface), size = 3, show.legend = F) +
  geom_point(aes(x = x, y = n), size = 3, shape = 21, stroke = 1.5, color = "grey40") +
  geom_text(aes(x = x, y = n, label = n, color = I(text_col)), 
            size = 2, family = "PT Sans Narrow Bold", show.legend = F) +
  ggrepel::geom_text_repel(aes(x = x, y = n, label = str_wrap(event, width = 8, whitespace_only = F)),
                           lineheight = .7, size = 1.5, color = "grey10", bg.color = "white", 
                           family = "PT Sans Bold", hjust = 0, nudge_x = .2) +
  annotate(GeomMarquee, x = 1, y = 1, label = "**Data:** Olympics via TidyTuesday  
           **Packages:** {tidyverse, ggrepel, marquee}  
           **Visualization:** C. Börstell", 
           family = "PT Sans Narrow", size = 1, color = "grey", lineheight = .8) +
  labs(title = "Medal Events in the\n2026 Winter Olympics",
       subtitle = "Total medals by discipline & surface: {.gold3 **ice**} vs. {.red3 **snow**}") +
  scale_color_manual(values = c("gold", "red3")) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(1, 50)) +
  theme_void(paper = "#FAFAFA") +
  theme(plot.title = element_text(family = "PT Sans Narrow Bold", 
                                  size = rel(1.1)),
        plot.subtitle = element_marquee(family = "PT Sans Narrow", 
                                        size = rel(.5),
                                        margin = margin(t = 0)),
        plot.margin = margin(1, 2, 2, 2, "mm"))



# Save plot ---------------------------------------------------------------

ggsave("winter_olympics.png", width = 1.5, height = 4, units = "in", dpi = 1200)



# Alt-text ----------------------------------------------------------------

# A scatter plot in the shape of a curling sheet (lane) showing the number of medal events at the 2026 Winter Olympics. Each discipline is shown as a curling stone, in either red (snow) or yellow (ice), with its discipline name labeled next to it, and numbered by the number of medal events within the discipline. Freestyle Skiing is at the top with 45 events, followed by Speed Skating (42) and Cross-Country Skiing (35).
