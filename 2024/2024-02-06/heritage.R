
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(flagrant)
library(ggbump)
library(ggtext)



# Set fonts and colors ----------------------------------------------------

# Font
font <- "Rockwell"

# Palettes
den <- flagrant::get_palette("Denmark")
nor <- flagrant::get_palette("Norway")
swe <- flagrant::get_palette("Sweden")



# Read and process data ---------------------------------------------------

# Read data and rank countries
heritage <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-06/heritage.csv") |> 
  pivot_longer(cols = c("2004", "2022"), names_to = "year", values_to = "n", names_transform = list(year = as.integer)) |> 
  arrange(desc(n)) |> 
  mutate(rank = row_number(), .by = year) |> 
  mutate(current = case_when(
    year==2022 ~ rank,
    .default = NA
  )) |> 
  arrange(country, year) |> 
  fill(current, .direction = "up") |> 
  arrange(desc(n))

# Get map data for Scandinavia
scandi <- map_data("world", region = c("Denmark",  "Norway","Sweden"))



# Plot data ---------------------------------------------------------------

ggplot() +
  
  # Plot country map data
  geom_polygon(data=scandi, aes(long, lat, group=group, fill=region, color=region), linewidth=1) +
  
  # Make lines from countries to data points
  geom_sigmoid(data=filter(heritage, year==2004), aes(x=c(18, 10, 9), y=c(65, 61.5, 56),
                                                      xend=30+(year-2005)/2, yend=60+(5-rank*2), group=country, color=country), linewidth=2) +
  geom_sigmoid(data=filter(heritage, year==2004), aes(x=30+(year-2005)/2, y=60+(5-rank*2),
                                                      xend=30+(year-2005)/2+11.5, yend=60+(5-current*2), group=country, color=country), linewidth=2) +
  
  # Make endpoint markers for countries
  geom_point(data=heritage, aes(x=c(18, 18, 9.8, 9.8, 9.1, 9.1), y=c(65, 65, 61.5, 61.5, 56, 56), color=country, size=.01)) +
  
  # Make data points for numbers and ranks
  geom_point(data=heritage, aes(x=30+(year-2004)/1.7, y=60+(5-rank*2), size=n**1.3+rank, color=country), shape=16) +
  geom_point(data=heritage, aes(x=30+(year-2004)/1.7, y=60+(5-rank*2), size=n**1.1, fill=country, color=country), shape=21) +
  geom_text(data=filter(heritage, country!="Denmark"), aes(x=30+(year-2004)/1.7, y=60+(5-rank*2), label=n), color="grey99", hjust=.53, family=paste0(font, "-Bold"), size=5) +
  geom_text(data=filter(heritage, country=="Denmark"), aes(x=30+(year-2004)/1.7, y=60+(5-rank*2), label=n), color="grey10", hjust=.53, family=paste0(font, "-Bold"), size=5) +
  geom_text(data=filter(heritage, country=="Denmark"), aes(x=30+(year-2004)/1.7, y=60+4.8, label=year), family=paste0(font, "-Bold"), size=5, color="grey20") +
  
  # Text boxes
  geom_textbox(aes(x=38, y=67, label="UNESCO World Heritage Sites"), 
               family=paste0(font, "-Bold"), size=8.2, color="grey10", 
               box.color="transparent", fill="transparent", width = .5) +
  
  geom_textbox(aes(x=36, y=56.5, label="Number of heritage sites across Denmark, Norway and Sweden in the years 2004 and 2022"), 
               family=paste0(font), size=4, color="grey10", 
               box.color="transparent", fill="transparent", width = .4) +
  
  geom_textbox(aes(x=36, y=55, label="_____________________________________________________________<br>Data: UNESCO via 1 dataset, 100 visualizations and TidyTuesday<br>Packages: {tidyverse, flagrant, ggbump, ggtext}<br>Visualization: @c_borstell"), 
               family=paste0(font), size=2, color="grey60", 
               box.color="transparent", fill="transparent", width = .4) +
  
  # Scaling
  coord_quickmap(xlim = c(0, 46),
                 ylim = c(54, 71.5)) +
  scale_size_continuous(range = c(4, 18)) +
  scale_fill_manual(values=c(den[2], nor[1], swe[1])) +
  scale_color_manual(values=c(den[1],  nor[3], swe[2])) +
  labs(fill="", color="") +
  theme_void() +
  theme(legend.position = "none", 
        legend.text = element_text(size=rel(1.5)),
        plot.background = element_rect(fill="#FBFBF1", color="transparent"))

# Save plot
ggsave("./heritage.jpg", width=7, height=6, units="in")
