# Load libraries
library(tidyverse)
library(usmap)
library(ggtext)

# Read data
states <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-01/states.csv") %>% 
  mutate(biggest = capital_city==largest_city) %>% 
  drop_na(biggest)

# Plot states
plot_usmap(data = states, values = "biggest", color = "white") +
  scale_fill_manual(values=c("pink", "skyblue")) +
  labs(title = "US states where the<br><span style='color:grey60;font-size:30pt;font-family:Phosphate'>capital</span><br><span style='color:skyblue;font-family:Phosphate;font-size:50pt'>is</span><span style='color:pink;font-family:Phosphate;font-size:50pt'>n't</span><br>the <span style='color:grey60;font-family:Phosphate;font-size:30pt;'>biggest</span> city",
       caption = "Data: Wikipedia via TidyTuesday\nPackages: {ggtext,tidyverse,usmap}\nVisualization: @c_borstell") +
  theme(legend.position = "none",
        plot.title = element_markdown(family = "Rockwell", 
                                      color = "grey70",
                                      hjust = .5,
                                      size = rel(1.2)),
        plot.caption = element_text(family = "Rockwell",
                                    color="grey80", 
                                    hjust=.5, 
                                    vjust = 155,
                                    size=rel(.4)),
        plot.margin = margin(0, 0, -30, 0, unit = "pt"))

# Save plot
ggsave(width = 2.5, height = 3, units = "in", dpi = 600)

states %>% 
  filter(biggest) %>% 
  select(state) %>% 
  unlist() %>% 
  paste0(., collapse = ", ")
