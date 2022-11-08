# Load packages
library(tidyverse)
library(ggchicklet)
library(ggtext)
library(scales)

# Read data
fm <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/state_stations.csv')


# Find rock and country stations
fm <- fm %>% 
  mutate(rock = !is.na(str_extract(tolower(format), "rock"))) %>% 
  mutate(country = !is.na(str_extract(tolower(format), "country")))

# Count country stations
country <- fm %>% 
  filter(country == T) %>% 
  count(state, country, name = "n_country")

# Count rock stations
rock <- fm %>% 
  filter(rock == T) %>% 
  count(state, rock, name = "n_rock")

# Convert degrees to radians
deg2rad <- function(x){
  (x*pi)/180
}

# Add state and region data and rescale dial angles in radians
country_rock <- left_join(rock, country) %>% 
  select(-c(country, rock)) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(ratio = n_rock / (n_country + n_rock)) %>% 
  mutate(state_abb = state.abb) %>% 
  mutate(region = state.division) %>% 
  mutate(rads = rescale(ratio, from=c(0,1), to=c(deg2rad(-180), 0)))

# Calculate regional median ratio across each state's total ratio
regional_radio <- country_rock %>% 
  group_by(region) %>% 
  mutate(ratio = median(ratio)) %>% 
  slice(1) %>% 
  arrange(desc(ratio)) 

# Adjust y coordinates for regional stations for top or bottom placement
regional_radio$y <- c(1.45, 2.7, 1.45, 2.7, 1.45, 2.7, 1.45, 2.7, 1.45)

# Adjust x axis to have station frequency panel start from 0
xadjust <- -2.5

# Custom font
# https://fonts.google.com/specimen/Pacifico
font <- "Pacifico"

# Plot data
ggplot() +
  
  ### Dial panel (individual states)
  # Board
  geom_rrect(aes(xmin=0+xadjust, xmax=15+xadjust, ymin=10, ymax=15.7), 
             fill="#5c371a",
             radius = unit(.5, units = "cm")) +
  # Main dials
  geom_point(aes(x=c(1.4,13.57)+xadjust, y=c(13,13)), size=20, color="grey20") +
  geom_text(aes(x=c(1.4,13.5)+xadjust, y=c(14.7,14.7), label=c("Country","Rock")), 
            color="#ebe9df", size=4, family="Pacifico") +
  geom_segment(aes(x=.2+xadjust, xend=1.4+xadjust, y=13, yend=13), color="#ebe9df", size=1) +
  geom_segment(aes(x=13.6+xadjust, xend=14.8+xadjust, y=13, yend=13), color="#ebe9df", size=1) +
  geom_text(aes(x=c(1.3,13.5)+xadjust, y=c(11.5,11.5), label=c("Max","Max")), 
            color="#ebe9df", size=3.3, family="Futura-Medium") +
  geom_text(aes(x=c(1.3,13.5)+xadjust, y=c(11,11), label=c("Cntry","Rock")), 
            color="#ebe9df", size=3.3, family="Futura-Medium") +
  # Dials for individual states
  geom_point(aes(x=rep(1:10, 5)-.5, y=rep(5:1, each=10)+10), size=5, color="grey20") +
  geom_spoke(data=country_rock, aes(x=rep(1:10, 5)-.5, y=rep(5:1, each=10)+10, angle=-rads, radius=.33), color="#ebe9df") +
  geom_text(data=country_rock, aes(x=rep(1:10, 5)-.5, y=rep(5:1, each=10)+9.55, label=state_abb),
            family="Futura-Medium", size=1.7, color="#ebe9df") +
  
  # Make antenna
  geom_segment(aes(x=0, xend=10, y=7, yend=9.5), color="grey20", size=1) +
  geom_segment(aes(x=0, xend=10, y=7, yend=9.5), color="grey50") +
  geom_point(aes(x=10, y=9.5), size=1, fill="grey45", color="grey20", shape=21) +
  
  ### Make radio (regional data)
  #
  # Legs
  geom_rrect(aes(xmin=c(2,12)+xadjust, xmax=c(3,13)+xadjust, ymin=-1, ymax=2), 
             fill="#422712",
             radius = unit(.3, units = "cm")) +
  # Body
  geom_rrect(aes(xmin=0+xadjust, xmax=15+xadjust, ymin=0, ymax=8), 
                         fill="#5c371a",
                         radius = unit(1, units = "cm")) +
  geom_rrect(aes(xmin=0.5+xadjust, xmax=14.5+xadjust, ymin=0.5, ymax=7.5), 
                         fill=NA, color="grey80", size=1.5,
                         radius = unit(.7, units = "cm")) +
  geom_rrect(aes(xmin=1.2+xadjust, xmax=13.8+xadjust, ymin=3.5, ymax=6.8), 
             fill="grey50", color="grey10", size=.5,
             radius = unit(.3, units = "cm")) +
  # Grate in front of speaker
  geom_segment(aes(x=seq(1.5,13.5,.5)+xadjust, xend=seq(1.5,13.5,.5)+xadjust, y=3.5, yend=6.8), 
               color="grey20", size=.7) + 
  geom_segment(aes(x=1.2+xadjust, xend=13.8+xadjust, y=seq(3.9,6.5,.5), yend=seq(3.9,6.5,.5)), 
               color="grey20", size=.7) +
  
  # Make station frequency panel with ticks
  geom_rect(aes(xmin=0, xmax=10, ymin=1, ymax=3),
            fill="#f2efdc", color="grey20") +
  geom_segment(aes(x=0, xend=10, y=2, yend=2),
               color="grey20", size=.4) +
  geom_segment(aes(x=seq(0,10,.2), xend=seq(0,10,.2), y=1.7, yend=2.3),
               color="grey20", size=.2) +
  geom_segment(aes(x=seq(0,10,5), xend=seq(0,10,5), y=1.7, yend=2.3),
               color="grey20", size=.4) +
  # Indicating points and labels for regional averages
  geom_point(data=regional_radio, aes(x=ratio*10, y=y),
             color="grey50", size=.5,
             position = position_nudge(
               y=c(.1, -.25, .1, .1, -.25, .1, -.25, -.25, .1))) +
  geom_text(data=regional_radio, aes(x=ratio*10, y=y, label=region),
            position = position_nudge(
              y=c(.1, -.25, .1, .1, -.25, .1, -.25, -.25, .1),
              x=c(1.57, 1.73, .8, 2.12, 1.15, -.05, -.04, -.04, -.04)),
            size=1.5, family="Futura-Medium", color="grey50", hjust=1.05) +
  geom_text(aes(x=c(10,10), y=c(2.8,2.55), label=c("Regional Median", "National Median"), color=c("grey50","#94110a")),
            size=1.5, family="Futura-Bold", show.legend = F, hjust=1.07, alpha=.6) +
  scale_color_identity() +
  # Station indicators (red) showing national median by region
  geom_segment(aes(x=median(country_rock$ratio)*10, xend=median(country_rock$ratio)*10, y=1.02, yend=2.98), 
               color="#94110a", size=.8, alpha=.9) +
  
  # Station dials on radio (Country & Rock)
  geom_text(aes(x=c(1.5,13.5)+xadjust, y=c(2.7,2.7), label=c("Country","Rock")), 
             color="#ebe9df", size=3.5, family="Pacifico") +
  geom_point(aes(x=c(1.5,13.5)+xadjust, y=c(1.5,1.5)), 
             fill="#f2efdc", size=12, shape=21) +
  geom_point(aes(x=c(1.5,13.5)+xadjust, y=c(1.5,1.5)), 
             fill="#eae8de", size=7, shape=21) +
  
  # Titles, subtitles and caption
  geom_text(aes(x=5, y=17, label="\nby state ..."), family="Pacifico", size=4, color="grey30") +
  geom_text(aes(x=5, y=9, label="... and region"), family="Pacifico", size=4, color="grey30") +
  labs(title="<img src='https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/240/openmoji/338/musical-note_1f3b5.png' width='35'/><span style='color:grey20;font-family:Pacifico;font-size:20px'>... </span><span style='color:grey20;font-family:Pacifico;font-size:35px'>and the home of country!</span><img src='https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/240/openmoji/338/musical-notes_1f3b6.png' width='35'/>",
       subtitle="<span style='color:#ebe9df;font-family:Pacifico'>Proportion of Country vs. Rock radio stations in the US</span>",
       caption="Visualization: @c_borstell\nPackages: {tidyverse,ggchicklet,ggtext,scales}\nData: Wikipedia (via Frank Hull & TidyTuesday; Source: FCC)") +
  
  coord_equal() +
  # Themes
  theme_void() +
  theme(plot.background = element_rect(color=NA, fill="#d6be96"),
        plot.title = element_markdown(hjust=0.5),
        plot.subtitle = element_markdown(hjust=0.5, size=13),
        plot.caption = element_text(family="Futura-Medium", size=5, hjust=.5, color="#ebe9df", vjust=25),
        plot.margin = margin(3,0,-5,0, unit="mm"))

# Save plot
ggsave(width=5, height=6.5, units="in", dpi=600)

