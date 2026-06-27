
# Load packages -----------------------------------------------------------

library(tidyverse)
library(tidytext)
library(slider)
library(shadowtext)
library(patchwork)



# Read data ---------------------------------------------------------------

# Stress dictionary of English
raw_words <- 
  read_lines("https://raw.githubusercontent.com/cmusphinx/cmudict/refs/heads/master/cmudict.dict")

# Extract general stress pattern
dict <- 
  tibble(
    word = str_extract(raw_words, "([^\\s]+)(\\s.*)", group = 1),
    pronunciation = str_extract(raw_words, "([^\\s]+)(\\s.*)", group = 2)
  ) |> 
  mutate(pattern = str_extract_all(pronunciation, "\\d")) |> 
  mutate(pattern = map_chr(pattern, \(x) paste0(x, collapse = "")))

# Papal encyclicals
encyclicals <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-23/encyclicals.csv")



# Process data ------------------------------------------------------------

# Extract sentences
sentences <- 
  encyclicals |> 
  unnest_sentences(sentence, text)

# Extract words and join with stress patterns
words <- 
  sentences |> 
  mutate(sentence_id = row_number()) |> 
  unnest_tokens(word, sentence) |> 
  left_join(dict)

# Target pattern
target <- 
  tibble(
    word = c("teenage", "mutant", "ninja", "turtles"),
    id = 1:4
  )

target_pattern <- 
  dict |> 
  inner_join(target) |> 
  arrange(id) |> 
  pull(pattern)

# Find sequences that match stress pattern 
# (not necessarily the same word boundaries)
target_words <- 
  words |> 
  mutate(target_match = slide_lgl(pattern, \(x) identical(paste0(x, collapse = ""), paste0(target_pattern, collapse = "")), .before = 0, .after = 3), .by = sentence_id)

# Extract the exact word sequence matches
target_matches <- 
  target_words |> 
  filter(target_match | lag(target_match) | lag(target_match, 2) | lag(target_match, 3)) |> 
  summarize(phrase = paste(word, collapse = " "), 
         .by = c(encyclical, pope, year, paragraph, sentence_id)) |> 
  mutate(id = consecutive_id(sentence_id))


# Plot data ---------------------------------------------------------------

# Define custom plot function
plot_tmnt <- function(df, index = 1) {
  df |> 
    filter(id == index) |> 
    ggplot() +
    geom_text(aes(x = I(.5), y = I(.85), label = str_glue("{encyclical} ({year}), Pope {pope}, §{paragraph}:")),
              family = "Reenie Beanie") +
    geom_label(aes(x = I(.5), y = I(.6), label = str_to_upper(str_extract(phrase, "\\w+ \\w+ \\w+"))), 
               label.r = unit(0, "mm"), linewidth = 1, fill = "red", border.color = "black", color = "white", family = "PT Sans Narrow Bold") +
    geom_shadowtext(aes(x = I(.5), y = I(.35), label = str_to_upper(str_extract(phrase, "\\w+$"))), 
                    family = "Rubik Distressed", size = 10, color = "#00BB00") +
    theme_void(paper = "ghostwhite")
}

# Plot four of the hits
papal_plots <- 
  map(3:6, \(i) plot_tmnt(target_matches, i))

# Plot data
wrap_plots(papal_plots, ncol = 1) + 
  labs(caption = "Data: Vatican via TidyTuesday & CMU Sphinx | Packages: {tidyverse, patchwork, shadowtext, slider, tidytext} | Visualization: C. Börstell") +
  theme(plot.caption = element_text(family = "Reenie Beanie", 
                                    color = "antiquewhite4",
                                    size = 4,
                                    hjust = .5,
                                    vjust = 5))

# Save plot
ggsave("papal.png", width = 3, height = 4, units = "in", dpi = 600)

# Alt-text: A stacked plot with text only in the style of the Teenage Mutant Ninja Turtles (TMNT) logo, each showing a word sequence from Pope Leo's encyclical that matches the stress pattern of TMNT, e.g., the bottom one reading "Therefore action is required". Data: Vatican via TidyTuesday & CMU Sphinx; Packages: {tidyverse, patchwork, shadowtext, slider, tidytext}; Visualization: C. Börstell
