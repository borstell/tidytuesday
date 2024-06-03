
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggchicklet)


# Set color ---------------------------------------------------------------

bg_col <- "#EEFAFA"


# Read data ---------------------------------------------------------------

cheeses <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-04/cheeses.csv")


# Process data ------------------------------------------------------------

all_cheeses <- 
  cheeses |> 
  filter(!is.na(milk)) |> 
  mutate(milk = str_split(milk, ", ")) |> 
  unnest_longer(milk) |> 
  count(milk, sort = T) |> 
  mutate(milk = case_when(
    n < 100 ~ "other",
    .default = milk
  )) |> 
  mutate(n = sum(n), .by = milk) |> 
  slice(1, .by = milk)



# Plot data ---------------------------------------------------------------

ggplot() +
  geom_chicklet(data=all_cheeses, aes(x=fct_reorder(str_to_title(milk), n), y=n), fill="yellow1", color="orange") +
  geom_jitter(aes(y=sample(0:800, 100, replace = T), x=sample(1:4, 100, replace = T), size=sample(1:4, 100, replace = T)), 
              fill=bg_col, color=bg_col, show.legend = F) +
  geom_hline(yintercept = seq(0, 800, 200), alpha=.4, color="red", linewidth=1, lty=3) +
  coord_flip() +
  scale_size_continuous(range = c(3, 8)) +
  labs(y="# of cheeses", x=NULL, title="Cheeses by type of milk", caption="Data: cheese.com via TidyTuesday\nPackages: {tidyverse,ggchicklet}\nVisualization: @c_borstell") +
  theme_classic(base_size=20, base_family = "Pacifico") +
  theme(axis.line = element_blank(), 
        axis.ticks = element_blank(),
        plot.background = element_rect(fill=bg_col),
        panel.background = element_rect(fill=bg_col),
        text = element_text(color="orangered3"),
        axis.text = element_text(color="orangered2"),
        plot.title = element_text(size=rel(2)),
        plot.caption = element_text(family="Lato", size=rel(.5), color="pink3"))



# Save plot ---------------------------------------------------------------

ggsave("cheeses.jpg", width = 8, height = 6, units = "in", dpi = 600)
