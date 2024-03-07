
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggtext)


# Read data ---------------------------------------------------------------

trashwheel <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-05/trashwheel.csv")



# Process and plot data ---------------------------------------------------

trashwheel |> 
  drop_na(CigaretteButts) |> 
  rename("year" = Year) |> 
  summarize(cigarettes = sum(CigaretteButts), .by = c(year)) |> 
  ggplot() +
  geom_hline(yintercept = c(1e1), lty=3, color="grey40") +
  geom_col(aes(x=year, y=cigarettes*1.1), fill="orangered3", width=.8) +
  geom_col(aes(x=year, y=cigarettes), fill="#FAFAFA", color="grey20", linewidth=.3) +
  geom_col(aes(x=year, y=100), fill="gold2", color="grey20", linewidth=.3) +
  geom_hline(yintercept = c(1e3, 1e5, 1e7), lty=3, color="grey40") +
  geom_text(aes(x=year-.03, y=10, label=year), color="goldenrod3", family="Noteworthy Bold") +
  scale_y_log10(labels = scales::label_comma(), limits = c(1e0, 1e7)) +
  scale_x_reverse(breaks = scales::pretty_breaks(10), limits = c(2023.5, 2013.51)) +
  coord_flip(clip = "off") +
  labs(x=NULL, y=NULL, title="Number of <span style='color:red3'>cigarette butts</span> collected per year by the <span style='color:darkgreen'>Baltimore Healthy Harbor</span> initiative's four Trash Wheels",
       subtitle="Data: Baltimore Healthy Harbor via TidyTuesday<br>Packages: {tidyverse, ggtext}<br>Visualization: @c_borstell<br>") +
  theme_classic(base_size = 15, base_family = "Noteworthy Bold") +
  theme(plot.background = element_rect(fill="#AFDCDD"),
        panel.background = element_rect(fill="#AFDCDD"),
        panel.grid.major = element_blank(),
        plot.title = element_textbox_simple(width = 1, color="grey25"),
        plot.subtitle = element_textbox_simple(width = 1, color="lightblue4", size=rel(.4)),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(color="grey30"),
        plot.margin = margin(25, 35, 5, 20, unit="pt"))


# Save plot ---------------------------------------------------------------

ggsave("./trashwheel.jpg", width=5, height=5, units="in", dpi=600)
