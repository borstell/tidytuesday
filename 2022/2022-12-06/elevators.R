# Load libraries
library(tidyverse)

# Read data
elevators <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-06/elevators.csv")

# Custom font
# https://fonts.google.com/specimen/Warnes

# Plot data
elevators %>% 
  filter(!is.na(DV_SPEED_FPM)&!is.na(LATITUDE)) %>% 
  filter(gsub("[0-9]","",DV_SPEED_FPM)=="") %>% 
  mutate(DV_SPEED_FPM = as.numeric(DV_SPEED_FPM)) %>% 
  mutate(bin_lat = round(LATITUDE*1000, -1)) %>% 
  group_by(bin_lat) %>% 
  summarize(median = median(DV_SPEED_FPM)) %>% 
  mutate(bin_lat = bin_lat/1000) %>% 
  ggplot() +
  geom_segment(data=tibble(y=seq(150,350,50),x1=c(40.83, 40.8, 40.78, 40.775, 40.77),x2=40.91), aes(x=x1, xend=x2, y=y, yend=y), color="#f7f7df", size=.7, alpha=.1) +
  geom_segment(data=tibble(y=seq(150,350,50),x1=c(40.83, 40.8, 40.78, 40.775, 40.77),x2=40.91), aes(x=x1, xend=x2, y=y, yend=y), color="white", size=.05, alpha=.3) +
  geom_label(data=tibble(x=40.89, y=seq(150,350,50),label=seq(150,350,50)), 
            aes(x=x, y=y, label="        "),
            fill="#000614", label.size = 0) +
  geom_text(data=tibble(x=40.89, y=seq(150,350,50),label=seq(150,350,50)), 
             aes(x=x, y=y, label=paste0(label," ft/min")),
             color="#f7f7df", family="Warnes", size=1.5, alpha=.5) +
  geom_text(aes(x=40.625, y=105, label="Visualization: @c_borstell"), color="#f7f7df", family="Warnes", size=1.4, alpha=.05) +
  geom_line(aes(x=bin_lat, y=median), color="#f7f7df", linewidth=2, alpha=.1) +
  geom_line(aes(x=bin_lat, y=median), color="#f7f7df", linewidth=1, alpha=.4) +
  geom_line(aes(x=bin_lat, y=median), color="white", linewidth=.5) +
  labs(title="New York City skyline",
       subtitle="by median elevator speeds by latitude, south to north",
       caption="Data: Hvitfeldt E. (2022), {elevators}") +
  theme_void() +
  theme(plot.background = element_rect(fill="#000614"),
        plot.title = element_text(family="Warnes", color="#f7f7df", vjust=-30, hjust=.8, size=rel(2.3)),
        plot.subtitle = element_text(family="Warnes", color="#f7f7df", vjust=-77, hjust=.84, size=rel(.9)),
        plot.caption = element_text(family="Warnes", color="#f7f7df", vjust=60, hjust=.94, size=rel(.33)))
# Save plot
ggsave(width=6.2, height=4.2, units="in", bg="white", dpi=600)
