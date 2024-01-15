
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(tidytext)



# Read data ---------------------------------------------------------------

polling_places <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-16/polling_places.csv")



# Process and plot data ---------------------------------------------------

polling_places |> 
  mutate(year = str_sub(election_date, 1, 4)) |> 
  tidytext::unnest_ngrams(bigram, name, n = 2) |> 
  count(bigram, year, sort = T) |> 
  filter(bigram != "church of") |> 
  mutate(rank = row_number(), .by = year) |> 
  filter(rank < 11) |> 
  mutate(cat = case_when(
    str_detect(bigram, "school") ~ "School",
    str_detect(bigram, "hall|fire|center") ~ "Public building",
    str_detect(bigram, "church") ~ "Church",
    .default = "Other"
  )) |> 
  ggplot() +
  geom_col(aes(x=n, y=fct_reorder(bigram, -rank), fill=cat), 
           color="grey10", linewidth=.4) +
  labs(x=NULL, y=NULL, fill=NULL, 
       title="Most common polling places\nin US elections (2012-2020)",
       subtitle="# of polling stations based on bigram frequency of their names\n",
       caption="Data: The Center for Public Integrity (via TidyTuesday) | Packages: {tidyverse,tidytext} | Visualization: @c_borstell") +
  scale_fill_grey(start = .9, end = .1) +
  facet_wrap(~year, nrow = 1) +
  theme_minimal(base_size=15, base_family="Special Elite") +
  theme(legend.position = "top",
        plot.background = element_rect(fill="#FEFFF0", color="transparent"),
        plot.title = element_text(size=rel(2.3), color="grey10"),
        plot.subtitle = element_text(size=rel(1), color="grey40"),
        plot.caption = element_text(size=rel(.5), color="grey80"),
        strip.text = element_text(color="grey10", size=rel(1.2)),
        plot.margin = margin(30, 30, 15, 30, "pt"))

# Save plot
ggsave("./polling_stations.jpg", width=10, height=6, units="in", dpi=600)
