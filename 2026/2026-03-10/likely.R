
# Load packages -----------------------------------------------------------

library(tidyverse)
library(marquee)



# Themes ------------------------------------------------------------------

font_main <- "Futura"
font_bold <- "Futura Bold"



# Read data ---------------------------------------------------------------


absolute_judgements <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-10/absolute_judgements.csv")




# Process and plot data ---------------------------------------------------

absolute_judgements |> 
  summarize(med = median(probability), .by = term) |> 
  mutate(term = str_replace_all(term, "\\s", "X")) |> 
  mutate(len = str_length(term)) |> 
  mutate(filled = ceiling(len * (med / 100))) |> 
  mutate(part1 = str_extract(term, paste0("^\\w{", filled, "}")),
         part2 = str_extract(term, paste0("(^\\w{", filled, "})(.*$)"), group = 2)) |> 
  mutate(part1 = str_replace_all(part1, "X", " ") |> str_to_upper(),
         part2 = str_replace_all(part2, "X", " ") |> str_to_upper()) |> 
  arrange(desc(med)) |> 
  mutate(rank = row_number()) |> 
  reframe(word = str_c(str_glue("{{.snow2 {part1}}}"), str_glue("{{.grey30 {part2}}}"), collapse = " {.yellow •} ")) |> 
  ggplot() +
  geom_marquee(aes(x = 0, y = 0, label = word), 
               family = font_bold, width = .7) +
  labs(title = "lol") +
  annotate(GeomMarquee, x = 0, y = I(.9), label = "_Likelihood of likelihood phrases_", 
           color = "snow2", family = font_bold, size = 6) +
  annotate(GeomMarquee, x = 0, y = I(.85), label = "As judged by 5000+ people in Kucharski (2026)", 
           color = "grey30", family = font_bold, size = 4) +
  annotate(GeomMarquee, x = 0, y = I(.78), label = "The proportional **fill** of each word corresponds to its median %-likelihood", 
           color = "snow2", family = font_main, size = 3) +
  annotate(GeomMarquee, x = 0, y = I(.2), label = "**Data:** Kucharski (2026) | **Packages:** {tidyverse, marquee} | **Visualization:** C. Börstell", 
           color = "grey40", family = font_main, size = 2.25) +
  theme_void(paper = "grey5", base_family = font_main)



# Save plot ---------------------------------------------------------------

ggsave("likely.png", width = 5.5, height = 5, units = "in", dpi = 600)

  