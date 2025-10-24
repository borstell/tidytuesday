
# Load packages -----------------------------------------------------------

library(tidyverse)
library(marquee)



# Read data ---------------------------------------------------------------

prizes <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-28/prizes.csv")



# Themes ------------------------------------------------------------------

# Function for categorical color sampling
sample_color <- function(x) {
  if (x == "man") {
    sample(colorRampPalette(colors = c("grey90", "grey88"))(5), 1)
  } else if (x == "woman") {
    sample(colorRampPalette(colors = c("pink", "pink2"))(7), 1)
  } else {
    sample(colorRampPalette(colors = c("skyblue", "skyblue2"))(4), 1)
  }
}



# Process data ------------------------------------------------------------

books <- 
  prizes |> 
  mutate(year5 = paste0(prize_year - prize_year %% 5, "–", prize_year - prize_year %% 5 + 4)) |> 
  mutate(year5 = str_replace(year5, "1990", as.character(min(prize_year)))) |> 
  mutate(year5 = str_replace(year5, "2024", as.character(max(prize_year)))) |> 
  filter(person_role == "winner") |> 
  arrange(year5, fct_rev(gender)) |> 
  mutate(index = row_number(), 
         w = 100 / n(), 
         .by = year5) |> 
  mutate(year5_id = consecutive_id(year5)) |> 
  select(year5, year5_id, gender, index, w) |> 
  rowwise() |> 
  mutate(h = sample(seq(0, .3, by = .05), size = 1, replace = T)) |> 
  ungroup() |> 
  mutate(color = map_chr(gender, sample_color)) 



# Plot data ---------------------------------------------------------------

books |> 
  ggplot() +
  annotate("segment", x = 50, y = 1, yend = 8, linewidth = 2, color = "wheat3", lineend = "square") +
  geom_segment(aes(x = index * w, xend = index * w, y = year5_id, yend = year5_id + .6 + h, color = I(color), linewidth = I(w*1.15))) +
  annotate("segment", x = c(0, 102), y = 0.75, yend = 8, linewidth = 2, color = "wheat3", lineend = "square") +
  annotate("segment", x = 0, xend = 102, y = 1:8, linewidth = 2, color = "wheat3") +
  annotate("text", x = 81, y = 1:7 + .35, label = unique(books$year5), color = "snow", family = "Lato Black", size = 6) +
  annotate("marquee", x = 50, y = 1, label = "**Data:** Post45 Data Collective via TidyTuesday | **Packages:** {tidyverse, marquee} | **Visualization:** C. Börstell", 
           size = 1.75, vjust = 1.7, hjust = .49, color = "grey50", family = "Lato") +
  labs(title = "British Literary Prizes (1991–2022)",
       subtitle = "Proportion of {.pink3 **women**} & {.skyblue3 **non-binary**} authors among literary prize recipients over time") +
  theme_void(paper = "snow") +
  theme(plot.title = element_text(family = "Lato Black", size = rel(1.55), color = "grey20"),
        plot.subtitle = element_marquee(family = "Lato", size = rel(1.25), color = "grey40", width = 1, lineheight = .75),
        plot.margin = margin(5, 10, 5, 10, unit = "pt"))

# Save plot
ggsave("literary_prizes.png", width = 4, height = 5, units = "in", dpi = 600)

