
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(geomtextpath)
library(ggtext)



# Read data ---------------------------------------------------------------

wages <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/wages.csv")
#demographics <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/demographics.csv")
#states <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/states.csv")



# Transform data ----------------------------------------------------------

# Remove aggregated numbers (public vs. private) and duplicates for the total
wages <- wages |> 
  filter(!facet %in% c("public sector: all", "private sector: all")) |> 
  slice(1, .by = c(facet, year))

# Create a small data frame with the years when the average 
# union wage first broke another $10/h (i.e. $10, $20, $30)
first_10s <- wages |> 
  filter(facet=="all wage and salary workers") |> 
  mutate(tens = union_wage%/%10) |> 
  filter(tens > 0) |> 
  slice(1, .by = tens)



# Plot data ---------------------------------------------------------------

ggplot() +
  # Gridlines
  # Create vertical gridlines for every ten years 1980-2020, meeting then average union wage
  geom_segment(aes(x=seq(1980, 2020, 10), xend=seq(1980, 2020, 10), y=3, yend=slice(group_by(filter(wages, facet=="all wage and salary workers" & year %in% seq(1980, 2020, 10)), year, union_wage), 1)$union_wage),
               alpha=.2, linetype=3) +
  # Create horizontal gridlines for years when a new $10/h limit was reached for the average union wage
  geom_segment(aes(x=first_10s$year, xend=2022, y=first_10s$union_wage, yend=first_10s$union_wage),
               alpha=.2, linetype=3) +
  
  # Wages
  # Union member wages by type of profession
  geom_line(data = filter(wages, facet!="all wage and salary workers"), aes(year, union_wage, group=facet),
            linewidth=.2, alpha=.3, color="dodgerblue2") +
  # Non-union member wages by type of profession
  geom_line(data = filter(wages, facet!="all wage and salary workers"), aes(year, nonunion_wage, group=facet),
            linewidth=.2, alpha=.3, color="goldenrod3") +
  # Union member wages totals
  geom_textline(data = filter(wages, facet=="all wage and salary workers"), aes(x=year, y=union_wage), 
                label="Union wage", linewidth=1, color="dodgerblue2", 
                family="Atkinson Hyperlegible Bold", size=8, hjust=.8) +
  # Non-union member wages totals
  geom_textline(data = filter(wages, facet=="all wage and salary workers"), aes(year, nonunion_wage),
            label="Non-union wage", linewidth=1, color="goldenrod2", 
            family="Atkinson Hyperlegible Bold", size=8, hjust=.6) +
  
  # Add textboxes as title, subtitle and caption
  geom_textbox(aes(x=1990, y=42, label="Average wage for <span style=color:dodgerblue2>union</span> vs. <span style=color:goldenrod2>non-union</span> members in the US"), 
               box.color="transparent", fill="transparent", family="Atkinson Hyperlegible Bold", color="grey55", size=6, width = .5) +
  geom_textbox(aes(x=1990, y=33, label="Years 1973 to 2022<br><span style=color:grey85>(1982 missing)</span>"), 
               box.color="transparent", fill="transparent", family="Atkinson Hyperlegible Bold", color="grey65", size=3, width = .5) +
  geom_textbox(aes(x=1987, y=32.5, label="Visualization: @c_borstell<br>Packages: {tidyverse,geomtextpath,ggtext}<br>Data: unionstats.com (Hirsch, Macpherson & Even 2023)"), 
               box.color="transparent", fill="transparent", family="Atkinson Hyperlegible Bold", color="grey85", size=1.1, width = .5, halign=1) +
  
  # Make y axis tick labels $n/h and move to the right side
  scale_y_continuous(labels = scales::dollar_format(suffix = "/h"), position = "right") +
  
  # Customize
  scale_x_continuous(expand = c(0, .5)) +
  theme_void(base_family = "Atkinson Hyperlegible", base_size=15) +
  theme(axis.text = element_text(family = "Atkinson Hyperlegible Bold", color="grey60"),
        panel.background = element_rect(fill="grey95", color="transparent"),
        plot.background = element_rect(fill="grey95", color="transparent"),
        plot.margin = margin(5, 5, 5, 0, unit = "mm"))

# Save plot
ggsave(width = 6, height = 4, units = "in", dpi = 600)
