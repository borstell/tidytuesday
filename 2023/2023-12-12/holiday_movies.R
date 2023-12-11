
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(geomtextpath)



# Read data ---------------------------------------------------------------

holiday_movies <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-12/holiday_movies.csv")
holiday_movie_genres <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-12/holiday_movie_genres.csv")



# Process data ------------------------------------------------------------

# Get Xmas movies only
xmas <- 
  holiday_movie_genres |> 
  filter(tconst %in% filter(holiday_movies, christmas)$tconst)

# Count by genre
xmas_genres <- 
  xmas |> 
  drop_na(genres) |> 
  count(genres, sort = T) |> 
  rowid_to_column("rank") |> 
  mutate(lvl = abs(rank%/%2-rank)) |> 
  mutate(tip = case_when(
    rank%%2==0 ~ n**.2,
    .default = -n**.2
  )) |> 
  mutate(tip = scales::rescale(tip, to=c(-20, 20)))


# Plot data ---------------------------------------------------------------

# Plot Xmas movies
ggplot() +
  
  # Let it snow!
  geom_point(aes(x=sample(-20:20, 300, replace = T), 
                 y=sample(-8:36, 300, replace = T),
                 shape=sample(c(8, 9, 11), 300, replace = T)), color="#FAFAFA") +
  
  # Tree trunk
  geom_segment(aes(x=0, xend=0, y=-1.5, yend=26), color="brown4", linewidth=5) +
  
  # Tree branches
  geom_textcurve(data=filter(xmas_genres, tip>0), 
                 aes(x=0, xend=tip, 
                     y=rank, yend=rank, 
                     label=genres, size=abs(tip), linewidth=abs(tip)), 
                 curvature = .2, color="darkgreen", family="Brush Script MT") +
  geom_textcurve(data=filter(xmas_genres, tip<0), 
                 aes(x=0, xend=tip, 
                     y=rank, yend=rank, 
                     label=genres, size=abs(tip), linewidth=abs(tip)), 
                 curvature = -.2, color="darkgreen", family="Brush Script MT") +
  
  # Xmas balls
  geom_point(data=xmas_genres, aes(x=tip-tip/10, 
                                   y=rank-rank**.05,
                                   size=n,
                                   fill=sample(c("orangered", "green3", "gold3", "blue"), 26, replace=T)), 
             shape=21) +
  
  # Frequencies in Xmas balls
  geom_text(data=xmas_genres, aes(x=tip-tip/10, 
                                  y=rank-rank**.05,
                                  label=n), color="white", family="Lexend", size=2.5) +
  
  # Tree top title
  geom_text(aes(x=0, y=29.5, label="Top\nChristmas\nmovies by genre"), 
            color="darkgreen", family="Brush Script MT", size=6, lineheight=.8) +
  
  # Tree top star
  geom_point(aes(x=0, y=34), shape=8, size=12, color="gold") +
  geom_point(aes(x=0, y=34), shape=8, size=11, color="gold2") +
  geom_point(aes(x=0, y=34), shape=8, size=10, color="gold3") +
  geom_point(aes(x=0, y=34), shape=21, size=3, color="gold3") +
  geom_point(aes(x=0, y=34), shape=21, size=5, color="gold3") +
  geom_point(aes(x=0, y=34), shape=21, size=8, color="gold3") +
  
  # Tree foot text
  geom_text(aes(x=0, y=-2.5, label="Comedy, Drama,"), color="darkred", family="Brush Script MT", size=4.5) +
  geom_text(aes(x=0, y=-4, label="Family & Romance"), color="darkred", family="Brush Script MT", size=4.5) +
  geom_text(aes(x=0, y=-5.5, label="form the base of"), color="darkred", family="Brush Script MT", size=4.5) +
  geom_text(aes(x=0, y=-7, label="Christmas movies"), color="darkred",  family="Brush Script MT", size=4.5) +
  
  # Caption
  geom_text(aes(x=0, y=-9, 
                label="Data: IMDb | Packages: {tidyverse,geomtextpath} | Visualization: @c_borstell"), 
            color="coral3",  family="Brush Script MT", size=2.5) +
  
  geom_curve(aes(y=15, yend=2, x=-17, xend=min(xmas_genres$tip)), color="coral3", linewidth=.7, curvature = .3, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_text(aes(x=-16, y=18, 
                label="Comedy is the\nmost frequent\nXmas genre"), 
            color="coral3",  family="Brush Script MT", size=5.5, lineheight=.9) +
  
  scale_size_continuous(range = c(3, 11)) +
  scale_linewidth_continuous(range = c(3, 6)) +
  scale_fill_identity() +
  scale_shape_identity() +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill="#DDEEEF", color="transparent"))

# Save plot
ggsave("./holiday_movies.jpg", width=6, height=6, units="in", dpi=600)

