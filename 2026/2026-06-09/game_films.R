
# Load packages -----------------------------------------------------------

library(tidyverse)



# Functions ---------------------------------------------------------------

decadify <- function(x) {
  floor(x / 10) * 10
}



# Custom colors -----------------------------------------------------------

green1 <- "#B2D144"
green2 <- "#3D8A26"
green3 <- "#113108"

sky <- "#9290F7"
ground1 <- "#935520"
ground2 <- "#CCA48F"
ground3 <- "#39210E"
txt <- "#FAFAFA"



# Read data ---------------------------------------------------------------

game_films <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-09/game_films.csv") |> 
  mutate(decade = decadify(year(release_date)))



# Plot data ---------------------------------------------------------------

game_films |> 
  drop_na(decade) |> 
  count(decade) |> 
  ggplot() +
  
  # Literally drawing the pipes field by field lol
  geom_col(aes(x = str_c(decade, "s"), y = 10 + n), 
           width = .5, fill = green1) +
  geom_col(aes(x = str_c(decade, "s"), y = 10 + n), 
           width = .17, fill = green2, position = position_nudge(x = .06)) +
  geom_col(aes(x = str_c(decade, "s"), y = 10 + n), 
           width = .1, fill = green2, position = position_nudge(x = .17), alpha = .25) +
  
  geom_col(aes(x = str_c(decade, "s"), y = 10 + n), 
           width = .04, fill = green2, position = position_nudge(x = -.17)) +
  
  geom_col(aes(x = str_c(decade, "s"), y = 10 + n), 
           width = .02, fill = green2, position = position_nudge(x = -.07), alpha = .8) +
  
  geom_col(aes(x = str_c(decade, "s"), y = 10 + n), 
           width = .5, fill = NA, color = green3, linewidth = 1) +
  
  geom_rect(aes(xmin = 1:5 - .3, xmax = 1:5 + .3, ymin = n + 11, ymax = n + 25),
            fill = green1) +
  
  geom_rect(aes(xmin = 1:5 - .3, xmax = 1:5 - .21, ymin = n + 11, ymax = n + 23),
            fill = green2) +
  geom_rect(aes(xmin = 1:5 - .3, xmax = 1:5 - .25, ymin = n + 11, ymax = n + 22),
            fill = green1) +
  
  geom_rect(aes(xmin = 1:5 - .05, xmax = 1:5 + .17, ymin = n + 11, ymax = n + 23),
            fill = green2) +
  geom_rect(aes(xmin = 1:5 + .17, xmax = 1:5 + .27, ymin = n + 11, ymax = n + 23),
            fill = green2, alpha = .5) +
  
  geom_rect(aes(xmin = 1:5 - .1, xmax = 1:5 - .07, ymin = n + 11, ymax = n + 23),
            fill = green2) +
  geom_rect(aes(xmin = 1:5 - .1, xmax = 1:5 + .3, ymin = n + 22, ymax = n + 23),
            fill = green2) +
  
  
  geom_rect(aes(xmin = 1:5 - .3, xmax = 1:5 + .3, ymin = n + 11, ymax = n + 25),
            fill = NA, color = green3, linewidth = 1) +
  
  
  # Add labels of Ns on top of pipes
  geom_text(aes(x = 1:5, y = n + 34, label = n),
            color = txt, hjust = .5) +
  
  # Create ground and decade labels
  annotate("rect", xmin = 0, xmax = 6, ymin = -10, ymax = 10,
           color = ground2, fill = ground1) +
  annotate("segment", x = 0, xend = 6, y = 0, color = ground3, alpha = .25) +
  annotate("segment", x = seq(0, 6, by = .25), y = -9, yend = 9, 
           color = ground3, alpha = .25) +
  annotate("text", x = 1:5, y = 0, label = str_c(seq(1980, 2020, by = 10), "s"), size = rel(3)) +
  
  # Annotation text and caption
  annotate("text", x = I(.05), y = I(.93), label = "Number of films\nbased on video games\nby decade of release date", 
           color = txt, hjust = 0, size = rel(2.7)) +
  
  annotate("text", x = I(.05), y = I(.82), label = "----------------------------------------\nData: Wikipedia via TidyTuesday\nVisualization: C. Börstell\nPackages: {tidyverse}", 
           color = "lightblue", hjust = 0, size = rel(1.7)) +
  
  labs(title = "Films Based on Video Games\n") +
  theme_void(base_family = "Press Start 2P", 
             paper = sky, 
             ink = txt) +
  theme(plot.margin = margin(b = -15, unit = "pt"),
        plot.title = element_text(hjust = .5, color = "gold"))



# Save plot ---------------------------------------------------------------

ggsave("game_films.png", width = 5, height = 4, units = "in", dpi = 600)



# Alt-text ----------------------------------------------------------------

# A bar chart in the style of an 8bit video game, with green pipes showing the bars of "Films Based on Video Games: Number of films based on video games by decade of release date". Data from Wikipedia via TidyTuessday. The graph shows a rapid increase from 4 and 32 films in the 1980s and 1990s, respectively, to 87 and 168 in the 2000s and 2010s, respectively, down to the (current decade) 2020s at 59.
