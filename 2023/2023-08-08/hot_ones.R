
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggchicklet)



# Read data ---------------------------------------------------------------

sauces <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/sauces.csv")
#episodes <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/episodes.csv")
#seasons <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/seasons.csv")

# Data wrangling
sauces <- sauces |> 
  
  # Cut heat levels into categories based on log10 intervals
  mutate(scoville_level = cut_number(log10(scoville), 
                              n = 5, 
                              labels = F)) |> 
  
  # Label heat levels
  mutate(scoville_label = case_when(
    scoville_level == 1 ~ "Mild",
    scoville_level == 2 ~ "Medium",
    scoville_level == 3 ~ "Hot",
    scoville_level == 4 ~ "Very Hot",
    scoville_level == 5 ~ "Extreme",
  )) |> 
  
  # Clean inconsistent sauce names
  mutate(sauce_name = case_when(
    str_starts(sauce_name, "Hot Ones – Los Calientes") ~ gsub("Hot Sauce", "", sauce_name),
    str_starts(sauce_name, "Da' Bomb") ~ "Da' Bomb Beyond Insanity",
    .default = sauce_name)) |> 
  
  # Categorize seasons into 7-season blocks
  mutate(season_block = case_when(
    season <= 7 ~ "Seasons 1–7",
    season > 7 & season <= 14 ~ "Seasons 8–14",
    season > 14 ~ "Seasons 15–21",
    .default = NA_character_
  )) |> 
  mutate(season_block = factor(season_block, levels=c("Seasons 1–7", "Seasons 8–14", "Seasons 15–21")))

# Create labeller for the heat categories
scoville_labs <- c(
  "Mild",
  "Medium",
  "Hot",
  "Very Hot",
  "Extreme!!!"
)
names(scoville_labs) <- 1:5


# Plotting ----------------------------------------------------------------

# Custom font "Fredericka the Great":
#https://fonts.google.com/specimen/Fredericka+the+Great

# Plot the data
sauces |> 
  
  # Create random labels for the sauce bottles
  mutate(label_shape = sample(c(1, 5, 9, 10), size=nrow(sauces), replace=T)) |> 
  
  # Unique sauces by season block
  group_by(season_block, sauce_name) |> 
  slice(1) |> 
  ungroup() |> 
  
  # Get location on shelf ...
  mutate(location = row_number(), .by = c(season_block, scoville_level)) |> 
  # ... and center at middle of each block shelf
  mutate(location = location-median(location), .by = c(season_block, scoville_level)) |> 
  
  # Plot data
  ggplot() +
  
  # Draw bottle body
  geom_rrect(aes(xmin=location-.45, xmax=location+.45, ymin=0, ymax=.3, fill=scoville_level), 
             radius = unit(1, "mm")) +
  # Draw bottleneck
  geom_rrect(aes(xmin=location-.2, xmax=location+.2, ymin=0, ymax=.45, fill=scoville_level), 
             radius = unit(.3, "mm")) +
  
  # Draw label
  geom_rect(aes(xmin=location-.45, xmax=location+.45, ymin=.08, ymax=.22), fill="wheat2") +
  geom_point(aes(x=location, y=.22/2+.08/2, shape=label_shape), color="wheat3") +
  
  # Draw bottle cap
  geom_rrect(aes(xmin=location-.25, xmax=location+.25, ymin=.4, ymax=.45), 
             radius = unit(.2, "mm"), fill="grey5") +
  
  # Draw shelves
  geom_segment(aes(x=min(location)-1, xend=max(location)+1, y=0, yend=0), 
               linewidth=2, color="sienna4") +
  
  # Bottle fill colors by heat level
  scale_fill_gradient(low="limegreen", high="firebrick") +
  
  # Label shapes
  scale_shape_identity() +
  
  # Labels
  labs(title="Hot Ones Sauces",
       subtitle="Unique hot sauces served\nby Scoville heat category\nper each 7-season block\n",
       caption="Data: Wikipedia via TidyTuesday | Packages: {tidyverse,ggchicklet} | Visualization: @c_borstell") +
  
  # Facet by heat level and season block
  facet_grid(scoville_level~season_block, labeller = labeller(scoville_level = scoville_labs), as.table = F, switch = "y") +
  
  # Themes
  theme_void() +
  theme(legend.position = "none",
        strip.text = element_text(family="Chalkduster", 
                                  color="cornsilk",
                                  size=rel(1.2)),
        plot.title = element_text(family="Fredericka the Great", 
                                  color="orange", 
                                  size=rel(4.8),
                                  hjust=.6),
        plot.subtitle = element_text(family="Chalkduster", 
                                  color="#FFCC55", 
                                  size=rel(1.8),
                                  hjust=.6),
        plot.caption = element_text(family = "Chalkduster",
                                    color="cornsilk", 
                                    hjust=.5, 
                                    vjust = -5,
                                    size=rel(.4)),
        plot.background = element_rect(fill="grey12", color=NA),
        panel.spacing.x = unit(-15, "pt"),
        plot.margin = margin(5, 25, 25, 15, unit = "pt"))

# Save plot
#ggsave(width = 7, height=5, units = "in", dpi=600)

# Alt-text
#"Graphic showing the distribution of Hot Ones Sauces: Unique hot sauces served by Scoville heat category per each 7-season block. The style is that of a blackboard with chalk writing in orange/yellow. Underneath the title, five brown shelves of hotness level from top to bottom in three columns each representing a season block is populated with small bottles of hot sauce colored in green (mild) to dark red (extreme). Data from Wikipedia via TidyTuesday. Packages tidyverse and ggchicklet. Visualization @c_borstell.