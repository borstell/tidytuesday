
# Load packages -----------------------------------------------------------

library(tidyverse)
library(glue)
library(marquee)
library(scales)



# Read data ---------------------------------------------------------------

holmes <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-25/holmes.csv")



# Process data ------------------------------------------------------------

# Count words per story
holmes_words <- 
  holmes |> 
  mutate(word = str_extract_all(text, "\\w+")) |> 
  unnest(word) |> 
  count(book) |> 
  mutate(novel = n > 15000)

# Count occurrences of "[Mm]y dear Watson"
watson <- 
  holmes |> 
  drop_na(text) |> 
  mutate(dear = str_extract_all(text, "[Mm]y dear Watson")) |>
  unnest(dear) |> 
  count(book, str_to_lower(dear)) |> 
  right_join(holmes_words |> select(-n)) |> 
  mutate(n = case_match(
    n,
    NA ~ 0,
    .default = n
  ))



# Plot data ---------------------------------------------------------------

watson |> 
  ggplot() +
  geom_vline(aes(xintercept = median(n)), lty = 3, alpha = .5) +
  geom_segment(aes(x = 0, xend = n, y = fct_reorder(book, n)), color = "firebrick", linewidth = .25) +
  geom_point(aes(x = n, y = fct_reorder(book, n), shape = novel), color = "firebrick", show.legend = F) +
  annotate(GeomMarquee, x = 6, y = 35, label = "***My dear Watson***", 
           family = "Tinos", size = 12) +
  annotate(GeomMarquee, x = 6.1, y = 19, 
           label = glue("
           The number of times the phrase
           *My dear Watson* occurs in {length(unique(watson$book))} of Sir Arthur Conan Doyle's 
           ***Sherlock Holmes*** works — novels ({{.firebrick ■}}) and other stories ({{.firebrick ●}).
           
           On average, the phrase occurs
           about once in each text, but there are 
           {nrow(filter(watson, n == 0))} works in which the phrase
           is missing altogether, including *{filter(watson, n == 0, novel)$book}*.  
             
           In *{filter(watson, n == max(n))$book}* — one of the canonical novels —
           the phrase occurs {filter(watson, n == max(n))$n} times."),
           family = "Tinos", size = 4, width = .6, lineheight = .85) +
  annotate(GeomMarquee, x = 5.9, y = 6.5, 
           label = "**Data:** {sherlock} via TidyTuesday | **Packages:** {tidyverse, glue, marquee, scales} | **Visualization:** C. Börstell", family = "Tinos",
           size = 1.75, color = "wheat4") +
  scale_x_continuous(breaks = scales::breaks_pretty(10)) +
  scale_shape_manual(values = c(16, 15)) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_family = "Tinos", paper = "wheat1") +
  theme(axis.text.y.left = element_text(size = 6, 
                                        margin = margin(0, -10, 0, 0, "pt"),
                                        lineheight = .5, 
                                        face = "italic"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

# Save plot
ggsave("sherlock.png", width = 7, height = 4, units = "in", dpi = 600)
