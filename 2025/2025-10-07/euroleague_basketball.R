
# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggtext)
library(ggrepel)
library(glue)
library(scales)



# Read data ---------------------------------------------------------------

raw_euroleague_basketball <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-07/euroleague_basketball.csv")



# Process data ------------------------------------------------------------

# Calculate capacity per arena (collapse teams sharing an arena)
euroleague_basketball <- 
  raw_euroleague_basketball |> 
  mutate(Cap = str_split(str_remove_all(Capacity, ","), " ")) |> 
  unnest(Cap) |> 
  mutate(Cap = as.numeric(Cap)) |> 
  select(Team, Arena, Cap) |> 
  slice_max(order_by = Cap, n = 1, by = Team) |> 
  mutate(Team = paste0(Team, collapse = " / "), .by = c(Arena)) |> 
  slice(1, .by = Team) |> 
  
  # Create interval for parabola height matches
  mutate(ymin = Cap - 100,
         ymax = Cap + 100)


# Custom function for plotting a parabola
height <- function(x, g = 1/10, v = 1, theta = 45) {
  
  y <- v * x * sin(theta * pi / 180) - .5 * g * x^2
  
}

# Make a tibble with parabola coordinates
parabola <- 
  tibble(x = 1:950) |> 
  mutate(y = round(height(x - 550, theta = 60) + 18500))

# Set seed for scrambling
set.seed(12)

# Join parabola coordinates with basketball arena capacity,
# joining by y coordinates that fall within capacity intervals
basketball_parabola <- 
  parabola |> 
  left_join(euroleague_basketball, by = join_by(between(y, ymin, ymax))) |> 
  
  # Scramble order of x coordinates to distribute teams along parabola
  # Note: Less sophisticated than finding max distances between points
  arrange(sample(x)) |> 
  
  # Drop coordinates without join matches
  drop_na(Team) |> 
  
  # Slice one data point per team (technically arena)
  slice(1, .by = Team) 

# Get basic stats for annotation text
n_teams <- nrow(raw_euroleague_basketball)
n_arenas <- nrow(euroleague_basketball)
median_cap <- number(round(median(euroleague_basketball$Cap)), big.mark = ",")

max_data <- slice_max(euroleague_basketball, order_by = Cap)
max_team1 <- str_extract(max_data$Team, "(.*)( \\/ )", group = 1)
max_team2 <- str_extract(max_data$Team, "(.*)( \\/ )(.*)", group = 3)
max_arena <- max_data$Arena
max_cap <- number(max_data$Cap, big.mark = ",")

min_data <- slice_min(euroleague_basketball, order_by = Cap)
min_team <- min_data$Team
min_arena <- min_data$Arena
min_cap <- number(min_data$Cap, big.mark = ",")



# Plot data ---------------------------------------------------------------

# Plot data points along parabola simulating the
# trajectory of a basketball shot
parabola |> 
  ggplot() +
  
  # Create custom grid lines with labels
  annotate("segment", 
           x = -100, 
           xend = c(200, 250, 400, 1000, 1000, 1000, 1000), 
           y = seq(5000, 20000, by = 2500), 
           lwd = .2, color = "grey50") +
  annotate("text", 
           x = c(200, 250, 400, 1000, 1000, 1000, 1000), 
           y = seq(5000, 20000, by = 2500), 
           label = number(seq(5000, 20000, by = 2500), big.mark = ","),
           hjust = -.2, size = 3, family = "Asap Condensed Light", color = "grey50") +
  
  # Draw a basketball hoop
  annotate("segment",
           x = 1100, y = 3000, yend = 8050, lwd = 1.2) +
  annotate("segment",
           x = 1100, xend = 1020, y = 8000, yend = 9000, lwd = 1.2) +
  annotate("segment",
           x = 1100, xend = 1020, y = 8000) +
  annotate("segment",
           x = 1020, xend = 1000, y = 8200, lwd = 2, color = "orange2") +
  annotate("segment",
           x = 1020, xend = 960, y = 8300, lwd = 1, color = "orange3") +
  annotate("segment",
           x = 1020, y = 7700, yend = 10000, lwd = 1.5, color = "snow3") +
  
  # Annotate descriptive stats box
  annotate("textbox", x = 480, y = 7000, 
           label = glue("There are {n_teams} teams in <span style='font-family:AsapCondensed-Italic'>Euroleague Basketball</span>, playing at {n_arenas} unique arenas with a median capacity of {median_cap} spectators.<br><br>The biggest arena by capacity is <span style='font-family:AsapCondensed-Regular'>{max_arena}</span> which hosts the teams <span style='font-family:AsapCondensed-ExtraLightItalic'>{max_team1}</span> and <span style='font-family:AsapCondensed-ExtraLightItalic'>{max_team2}</span> and seats up to {max_cap} people.<br><br>The smallest arena is <span style='font-family:AsapCondensed-Regular'>{min_arena}</span> which hosts <span style='font-family:AsapCondensed-ExtraLightItalic'>{min_team}</span> and has a capacity of {min_cap}."),
           family = "Asap Condensed ExtraLight", size = 3.7, width = .35, hjust = 0,
           fill = NA, box.color = NA) +
  
  # Annotate caption
  annotate("textbox", x = 485, y = 3200, label = "**Data:** EuroLeague & Wikipedia via {TidyTuesday} | **Packages:** {tidyverse, ggrepel, ggtext, glue, scales} | **Visualization:** C. Börstell", 
           family = "Asap Condensed ExtraLight", size = 1.3, color = "grey50", hjust = 0,
           fill = NA, box.color = NA, width = 2) +
  
  # Plot parabola
  geom_line(aes(x, y), lty = 2) +
  
  # Plot basketball emojis as data points along parabola
  geom_richtext(data = basketball_parabola, aes(x, y, label = "🏀"), 
               fill = NA, label.color = NA,
               label.padding = grid::unit(rep(0, 4), "pt")) +
  
  # Plot text labels for teams
  ggrepel::geom_text_repel(data = basketball_parabola, 
            aes(x, Cap, label = Team), 
            color = "grey20", bg.color = "white", family = "Asap Condensed Light",
            seed = 10, segment.curvature = .5, force = 10) +
  labs(title = "Euroleague Basketball teams by stadium capacity") +
  coord_cartesian(xlim = c(-100, 1100), ylim = c(3000, 22000)) + 
  theme_void(paper = "snow") +
  theme(plot.title.position = "plot",
        plot.title = element_text(family = "Asap Condensed SemiBold", 
                                  size = rel(2.7), color = "grey10",
                                  margin = margin(5, 5, -12, 5, "mm")))

# Save plot
ggsave("euroleague_basketball.png", width = 8, height = 6, units = "in", dpi = 600)



