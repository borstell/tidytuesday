# TidyTuesday, 2022-10-18
# Stranger Things dialogue

# Load packages
library(tidyverse)
library(tidytext)

# Read data
stranger_things <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv")

# Extract words from stage_direction that end with "ing"
stranger_ings <- stranger_things %>% 
  select(season, episode, stage_direction) %>% 
  drop_na(stage_direction) %>% 
  unnest_tokens(word, stage_direction) %>% 
  filter(str_ends(word, "ing"))

# Custom fonts
font1 = "Rubik Glitch" #https://fonts.google.com/specimen/Rubik+Glitch
font2 = "IM FELL DW Pica SC" #https://fonts.google.com/specimen/IM+Fell+DW+Pica

# Plot ing forms
stranger_ings %>% 
  group_by(season) %>% 
  count(word, sort = T) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  top_n(10) %>% 
  mutate(word = reorder_within(word, n, season)) %>% 
  ggplot(aes(x=word, y=n)) +
  geom_col(fill="orangered", color="orangered",size=.5, alpha=.2) +
  coord_flip(clip="off") +
  scale_x_reordered(expand=c(0,0)) +
  labs(x="", y="",
       title="STRANGER\nINGS",
       subtitle="Top 10 -ing verb forms in Stranger Things stage directions seasons 1 to 4",
       caption="Visualization: @c_borstell | Packages: {tidyverse,tidytext} | Data: 8flix.com (via TidyTuesday)") +
  facet_wrap(.~paste0("Season ",season), scales="free", ncol=2) +
  theme_minimal(base_family=font1) +
  theme(strip.text = element_text(size=18, color="orangered"),
        axis.text = element_text(size=9, color="orangered"),
        plot.title = element_text(family=font2, size=70, color="orangered2", hjust=.5, lineheight=.8),
        plot.subtitle = element_text(family=font1, size=10, color="orangered2", hjust=.5, vjust=39.5),
        plot.caption = element_text(size=7.8, hjust=.5, vjust=288, family=font1,color="orangered2"),
        panel.grid = element_blank(),
        plot.background = element_rect(color=NA, fill="grey5"),
        plot.margin = unit(c(.2,1,0,0), "in"))

# Save plot
ggsave(width=9, height=7, units="in", dpi=600)

