
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(rvest)
library(maps)



# Read data ---------------------------------------------------------------

# Get haunted data from TidyTuesday
haunted_places <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-10/haunted_places.csv")

# Get population data from Wikipedia
us_census_2020 <- read_html("https://en.wikipedia.org/wiki/2020_United_States_census") |> 
  html_table(header = TRUE) |> 
  _[3] |> 
  list_rbind() |> 
  rename(Population = 4) |> 
  select(Rank, State, Population) |> 
  mutate(Population = as.numeric(gsub(",", "", Population))) 



# Custom settings ---------------------------------------------------------

# Fonts:
# https://fonts.google.com/specimen/Rubik+Burned
# https://fonts.google.com/specimen/Righteous




# Process data ------------------------------------------------------------

# Process haunted data
spoopy_us <- haunted_places |> 
  mutate(longitude = case_when(
    is.na(longitude) ~ city_longitude,
    .default = longitude
  )) |> 
  mutate(latitude = case_when(
    is.na(latitude) ~ city_latitude,
    .default = latitude
  )) |> 
  filter(!is.na(longitude) & !is.na(latitude)) |> 
  mutate(state = case_when(
    state == "Washington DC" ~ "District of Columbia",
    .default = state
  ))

# Join with US population data
spoopy_states <- spoopy_us |> 
  count(state, sort = T) |> 
  left_join(us_census_2020, by = join_by(state == State)) |> 
  mutate(relative = n/Population*100000)

usmap <- maps::map("world", regions = "USA", plot = F, fill = T)

# Ranking the top 10 states with most haunted places relative to # of inhabitants
spoopiest <- ggplotGrob(spoopy_states |> 
  arrange(desc(relative)) |> 
  select(state, n, relative) |> 
  mutate(rank = row_number()) |> 
  filter(rank<=10) |> 
  uncount(round(relative)) |> 
  mutate(pos = row_number(), .by = state) |> 
  ggplot() +
  geom_point(aes(x=pos, y=fct_reorder(state, relative)), shape=21, color="lightblue", alpha=.8, size=2) +
  geom_point(aes(x=pos, y=fct_reorder(state, relative)), shape=21, color="lightblue", alpha=.8, size=1) +
  geom_point(aes(x=pos, y=fct_reorder(state, relative)), color="lightblue", alpha=.8, size=.1) +
  labs(title="Haunted places per 100,000 inhabitants\n") +
  theme_void(base_family="Righteous") +
  theme(axis.text.y = element_text(color="lightblue", size=4, hjust=1),
        plot.title = element_text(color="lightblue", size=7, hjust=1)))




# Plot data ---------------------------------------------------------------

# Title + subtitle
t <- ggplotGrob(
ggplot() +
  labs(title="Spoopy",
       subtitle="Haunted Places in the USA") +
  theme_void(base_family="Righteous") +
  theme(plot.title = element_text(family="Rubik Burned", size=rel(4), hjust=.5, color="lightblue"),
        plot.subtitle = element_text(size=rel(1.1), hjust=.5, color="lightblue"),
        plot.background = element_blank(),
        panel.background = element_blank())
)

# First caption
c1 <- ggplotGrob(
  ggplot() +
    labs(caption="Data: The Shadowlands Haunted Places Index via\n          Tim Renner and TidyTuesday; Wikipedia") +
    theme_void(base_family="Righteous") +
    theme(plot.caption = element_text(size=rel(.4), hjust=0, color="lightblue4"),
          plot.background = element_blank(),
          panel.background = element_blank())
)

# Second caption
c2 <- ggplotGrob(
  ggplot() +
    labs(caption="|  Visualization: @c_borstell\n|  Packages:      {tidyverse,maps,rvest}") +
    theme_void(base_family="Righteous") +
    theme(plot.caption = element_text(size=rel(.4), hjust=0, color="lightblue4"),
          plot.background = element_blank(),
          panel.background = element_blank())
)

# Plot map + annotations
ggplot() +
  geom_polygon(data=usmap, aes(long, lat, group=group), 
               fill="grey30", color="grey45", linewidth=.05, alpha=.2) +
  geom_point(data=spoopy_us, aes(longitude, latitude), alpha=.1, size=.1, color="lightblue") +
  coord_quickmap(xlim = c(-179, -65), ylim = c(18, 72)) +
  theme_void() +
  theme(plot.background = element_rect(fill="grey7", color="transparent"),
        panel.background = element_rect(fill="grey7", color="transparent")) +
  annotation_custom(grob = spoopiest, xmin=-180, xmax=-130, ymin=25, ymax=50) +
  annotation_custom(grob = t, xmin=-130, xmax=-65, ymin=45, ymax=71) +
  annotation_custom(grob = c1, xmin=-145, xmax=-65, ymin=19, ymax=40) +
  annotation_custom(grob = c2, xmin=-104.5, xmax=-45, ymin=19, ymax=40)

# Save plot
ggsave("./haunted_places.jpg", width=4.5, height=3, units="in", dpi=600)


