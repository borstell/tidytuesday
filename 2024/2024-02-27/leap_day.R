
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggtext)



# Fonts -------------------------------------------------------------------

# https://fonts.google.com/specimen/East+Sea+Dokdo
# https://fonts.google.com/specimen/Fredericka+the+Great


# Read data ---------------------------------------------------------------

#events <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/events.csv")
births <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/births.csv")
deaths <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/deaths.csv")

# Get name on both lists
name <- base::intersect(births$person, deaths$person)


# Plot data ---------------------------------------------------------------

ggplot() +
  
  geom_point(data=filter(births, person %in% deaths$person), 
             aes(x=year_birth, y="Births"), shape=21, fill="gold2", color="grey5", size=7) +
  geom_curve(data=filter(births, person %in% deaths$person), 
             aes(x=year_birth, xend=1828, y="Births", yend=1.5), 
             color="gold2", linewidth=1, curvature = .2) +
  geom_point(data=filter(births, person %in% deaths$person), 
             aes(x=year_birth, y="Births"), shape=21, fill="grey5", color="grey5", size=4) +
  
  geom_point(data=filter(deaths, person %in% births$person), 
             aes(x=year_death, y="Deaths"), shape=21, fill="gold2", color="grey5", size=7) +
  geom_curve(data=filter(deaths, person %in% births$person), 
             aes(x=1862, xend=year_death, y=1.5, yend="Deaths"), 
             color="gold2", linewidth=1, curvature = -.2) +
  geom_point(data=filter(deaths, person %in% births$person), 
             aes(x=year_death, y="Deaths"), shape=21, fill="grey5", color="grey5", size=4) +
  
  geom_point(data=births, aes(x=year_birth, y="Births"), shape=8, color="white") +
  geom_point(data=deaths, aes(x=year_death, y="Deaths"), shape=3, color="white") +
  
  geom_text(aes(x=1845, y=1.5, label=name), color="gold2", family="East Sea Dokdo", size=10) +
  geom_text(aes(x=filter(births, person==name)$year_birth, y=2.25, label=filter(births, person==name)$year_birth), color="gold2", family="East Sea Dokdo", size=10) +
  geom_text(aes(x=filter(deaths, person==name)$year_death, y=.75, label=filter(deaths, person==name)$year_death), color="gold2", family="East Sea Dokdo", size=10) +
  
  scale_x_continuous(limits = c(1800, 1900)) +
  scale_y_discrete(limits = rev) +
  guides(x = guide_axis(cap = "both")) +
  labs(x=NULL, y=NULL, title="With 121 births and 62 deaths listed on Wikipedia as having taken place on February 29th, only <span style='color:gold2'>one person</span> is found on both lists",
       subtitle=paste("<br>", name, "was a", filter(births, person==name)$description, "who was born and died on a leap day,", filter(deaths, person==name)$year_death-filter(births, person==name)$year_birth, "years apart"),
       caption="<br>Data: Wikipedia via TidyTuesday | Packages: {tidyverse, ggtext} | Visualization: @c_borstell") +
  theme_classic(base_size = 18, base_family = "East Sea Dokdo") +
  theme(axis.line.x = element_line(color="white"),
        axis.ticks.x = element_line(color="white"),
        axis.text = element_text(color="white"),
        axis.text.y = element_text(color="white", size=30),
        axis.title = element_text(color="white"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_textbox_simple(color="white", size=rel(1.5), family="Fredericka the Great"),
        plot.subtitle = element_textbox_simple(color="white", size=rel(1.2), family="East Sea Dokdo", width=.85),
        plot.caption = element_textbox_simple(color="grey30", size=rel(.8), family="East Sea Dokdo", halign=1),
        plot.background = element_rect(color="transparent", fill="grey5"),
        panel.background = element_rect(color="transparent", fill="grey5"),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        plot.margin = margin(10, 5, 10, 10, "mm"))

# Save plot
ggsave("./leap_day.jpg", width=9, height=6, units="in", dpi=600)


