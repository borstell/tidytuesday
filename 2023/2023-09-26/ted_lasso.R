
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggtext)
library(ggforce)



# Read data ---------------------------------------------------------------

richmondway <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-26/richmondway.csv")




# Plot --------------------------------------------------------------------

# Make football pitch
pitch <- ggplot() +
  # Grass
  #geom_rect(aes(xmin=-5, xmax=73, ymin=-5, ymax=110), fill="green4", color="green4") +
  # Semicircles
  ggforce::geom_circle(aes(x0=68/2, y0=11, r=9.15), fill=NA, color="#fafafa") +
  ggforce::geom_circle(aes(x0=68/2, y0=105-11, r=9.15), fill=NA, color="#fafafa") +
  # Pitch
  geom_rect(aes(xmin=0, xmax=68, ymin=0, ymax=105), fill="transparent", color="#fafafa", linewidth=.5) +
  # Goals
  geom_rect(aes(xmin=68/2-7.32/2, xmax=68/2+7.32/2, ymin=c(0,105), ymax=c(-3,108)), fill="transparent", color="#fafafa", linewidth=.5) +
  # Goal area
  geom_rect(aes(xmin=68/2-(7.32+11+22)/2, xmax=68/2+(7.32+11+22)/2, ymin=c(0,105), ymax=c(5.5+11, 105-5.5-11)), fill="green4", color="#fafafa", linewidth=.5) +
  # Penalty area
  geom_rect(aes(xmin=68/2-(7.32+11)/2, xmax=68/2+(7.32+11)/2, ymin=c(0,105), ymax=c(5.5, 105-5.5)), fill="green4", color="#fafafa", linewidth=.5) +
  # Center circle
  ggforce::geom_circle(aes(x0=68/2, y0=105/2, r=9.15), fill=NA, color="#fafafa") +
  # Center spot
  geom_point(aes(x=68/2, y=c(11, 105/2, 105-11)), color="#fafafa", size=c(.5,1,.5)) +
  # Halfway line
  geom_segment(aes(x=0, xend=68, y=105/2, yend=105/2), color="#fafafa") +
  coord_flip() 

# Tibble for player positions
positions <- tibble(
  x=rep(c(34, 44, 24, 48, 34, 20, 56, 43, 25, 12, 34),2),
  y=c(c(48, 40, 40, 30, 30, 30, 20, 20, 20, 20, 3), abs(c(48, 40, 40, 30, 30, 30, 20, 20, 20, 20, 3)-105)))

positions$fs <- c(richmondway[1:10,]$F_count_RK,
                  richmondway[10,]$cum_rk_season,
                  richmondway[1:10,]$F_count_total-richmondway[1:10,]$F_count_RK,
                  richmondway[10,]$cum_total_season-richmondway[10,]$cum_rk_season)
positions <- positions |> 
  mutate(who = rep(c("Roy Kent", "Other"), each=11)) |> 
  mutate(ep = paste0("#", rep(1:11, 2))) |> 
  mutate(ep = case_when(
    ep=="#11" ~ "n=",
    .default = ep
  )) |> 
  mutate(nudge = case_when(
    x < 34 ~ -3,
    .default = 3
  ))

# Plot pitch with data
pitch +
  geom_point(data=positions, aes(x, y, color=who), 
             shape=21, fill="#fafafa", size=8, show.legend = F) +
  geom_text(data=positions, aes(x, y, label=fs), 
            family="Atkinson Hyperlegible Bold") +
  geom_text(data=positions, aes(x+nudge, y, label=ep), 
            size=3, color="#fafafa", family="Atkinson Hyperlegible Bold") +
  geom_textbox(aes(x=65, y=18, label=paste("Roy Kent", positions[11,]$fs)), 
               fill="transparent", box.color="transparent", hjust=0,
               size=8, color="#fafafa", family="Atkinson Hyperlegible Bold", width = 2) +
  geom_textbox(aes(x=65, y=53.5, label=paste(positions[22,]$fs, "Others")), 
               fill="transparent", box.color="transparent", hjust=0,
               size=8, color="#fafafa", family="Atkinson Hyperlegible Bold", width = 2) +
  scale_color_manual(values=c("red3", "dodgerblue4")) +
  labs(title="Who gave more f*cks, <span style='color:dodgerblue4'>Roy Kent</span> or <span style='color:red4'>others</span>?",
       subtitle='No of times the word <span style="color:#fafafa">"f*ck"</span> was uttered<br>per episode in Ted Lasso (season 1)',
       caption="Data: Deepsha Menghani via TidyTuesday | Visualization: @c_borstell | Packages: {tidyverse,ggforce,ggtext}") +
  theme_void() +
  theme(aspect.ratio = 2/3,
        plot.background = element_rect(fill="green4", color="green4"),
        panel.background = element_rect(fill="green4", color="green4"),
        plot.title = element_textbox(family="Atkinson Hyperlegible Bold", 
                                     size=rel(4), color="#fafafa", width = 1, halign = .5),
        plot.subtitle = element_textbox(family="Atkinson Hyperlegible Bold", 
                                     size=rel(2), color="lightgreen", width = .9, halign = .8),
        plot.caption = element_text(family="Atkinson Hyperlegible Bold", 
                                        size=rel(.7), color="limegreen", hjust=.5, vjust=10),
        plot.margin = margin(5, -30, 0, -30, unit = "mm"))

# Save plot
ggsave(width = 8, height = 7, units = "in", dpi = 600)

#Alt-text: A graphic in the style of a football pitch (a.k.a. soccer field) showing the number of times the word "fuck" occurs by episode in season 1 of Ted Lasso. Each episode is displayed as a player position on the pitch, showing the episode number and number of occurrences of the word "fuck". In total, 56 fucks are uttered by character Roy Kent and 94 by others.