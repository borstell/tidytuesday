
# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggview)
library(marquee)



# Read data ---------------------------------------------------------------

holmes <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-18/holmes.csv")

# Manual data about novels
novels <- 
  tribble(
    ~book,~year,
    "A Study In Scarlet",1887,
    "The Sign of the Four",1890,
    "The Hound of the Baskervilles",1901,
    "The Valley Of Fear",1914
  )


# Process data ------------------------------------------------------------

# Get only novels, sort and index lines chronologically, find lines with phrase
novel_lines <- 
  holmes |> 
  filter(book %in% novels$book) |> 
  left_join(novels) |> 
  drop_na(text) |> 
  arrange(year) |> 
  mutate(watson = str_detect(text, "[Mm]y dear Watson")) |> 
  mutate(x = row_number() %% 140,
         y = row_number() %/% 140)



# Plot data ---------------------------------------------------------------

novel_lines |> 
  ggplot() +
  geom_path(data = \(x) filter(x, watson), aes(x, y), alpha = .1) +
  geom_tile(data = \(x) filter(x, !watson), aes(x, y, fill = book), 
            show.legend = F, color = "grey20", linewidth = .01, alpha = .1) +
  geom_point(data = \(x) filter(x, watson), aes(x-.55, y+.55), shape = "🔎", size = 3, color = "grey40") +
  geom_tile(data = \(x) filter(x, watson), aes(x, y, fill = book), 
            show.legend = F) +
  annotate("text", x = 136, y = c(4, 32, 59, 98), label = c("A Study in Scarlet",
                                              "The Sign of the Four",
                                              "The Hound of the Baskervilles",
                                              "The Valley of Fear"), 
           color = c("firebrick4", "coral3", "orange3", "goldenrod3"),
           family = "Tinos-Bold", size = 4.5, hjust = 1) +
  annotate(GeomMarquee, x = 70, y = 135, 
           label = "**Data:** {sherlock} via TidyTuesday |
           **Packages:** {tidyverse, ggview, marquee} |
           **Visualization:** C. Börstell",
           size = 2.5, family = "Tinos", hjust = .5, color = "grey") +
  scale_y_reverse() +
  scale_fill_manual(values = c("A Study In Scarlet" = "firebrick",
                               "The Sign of the Four" = "coral3",
                               "The Hound of the Baskervilles" = "orange3",
                               "The Valley Of Fear" = "gold2")) +
  labs(title = " “My dear Watson ...”",
       subtitle = "In the **four canonical Sherlock Holmes novels**,
       the phrase *“My dear Watson”* occurs 15 times — 10 times in 
       *The Hound of the Baskervilles*; not even once in *A Study in Scarlet*.  
       \nIn the panel below, each square tile (■) represents a line of text — in chronological 
       order — with the highlighted tiles showing the location of the famous phrase in each of the novels.") +
  coord_equal() +
  theme_void(paper = "snow2") +
  theme(plot.title = element_text(family = "Tinos-BoldItalic", 
                                  size = rel(3.2),
                                  color = "grey20",
                                  hjust = .2),
        plot.subtitle = element_marquee(family = "Tinos",
                                        size = rel(.75),
                                        lineheight = .9,
                                        width = .86,
                                        hjust = .45,
                                        margin = margin(2, 0, -10, 0, "pt")),
        plot.margin = margin(3, 0, -1, 0, "mm")) +
  canvas(5, 6, units = "in")

# Save plot
ggsave("sherlock.png", width = 5, height = 6, unit = "in", dpi = 600)


