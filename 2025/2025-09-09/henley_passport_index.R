
# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggbump)
library(ggtext)
library(patchwork)


# Themes and colors -------------------------------------------------------

typewriter_font <- "Special Elite"
stamp_font <- "Rubik Burned"
stamp_cols <- c("red3", "orangered4", "firebrick", "navy", "dodgerblue4", "purple4")
region_cols <- c("EUROPE" = "blue4",
                 "CARIBBEAN" = "dodgerblue2",
                 "AMERICAS" = "palegreen3",
                 "OCEANIA" = "skyblue",
                 "MIDDLE EAST" = "darkgreen",
                 "ASIA" = "red2",
                 "AFRICA" = "goldenrod1")
bg_col <- "#F3F3F0"



# Read data ---------------------------------------------------------------

rank_by_year <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-09/rank_by_year.csv")


# Calculate median per region and year
plot <- 
  rank_by_year |> 
  filter(year >= 2010) |> 
  summarize(visa_free_median = round(median(visa_free_count, na.rm = T)), .by = c(year, region)) |> 
  mutate(total_median = median(visa_free_median), .by = region) |> 
  
  # Resolve tied by which has a higher total median
  arrange(year, desc(visa_free_median), desc(total_median)) |>
  mutate(rank = row_number(), .by = year) |> 
  
  # Plot bump chart by rank
  ggplot() +
  ggbump::geom_bump(aes(year, rank, color = region), lwd = 3) +
  geom_point(data = \(x) filter(x, year %% 5 == 0), aes(year, rank, fill = region), size = 18, shape = 22, color = "transparent") +
  geom_point(data = \(x) filter(x, year %% 5 != 0), aes(year, rank, color = region), size = 4) +
  geom_point(data = \(x) filter(x, year %% 5 == 0), aes(year, rank), size = 13, shape = 22, fill = bg_col, color = "transparent") +
  geom_label(data = \(x) filter(x, year %% 5 == 0), aes(year, rank, label = visa_free_median), 
            size = 5, family = stamp_font, angle = sample(-45:45, 28), color = sample(stamp_cols, 28, replace = T), fill = "transparent") +
  geom_text(data = \(x) filter(x, year == 2023), aes(year - .5, rank - .35, label = region), family = typewriter_font) +
  scale_y_reverse(breaks = 1:7, expand = c(.1, 0)) +
  scale_x_continuous(position = "top") +
  scale_color_manual(values = region_cols) +
  scale_fill_manual(values = region_cols) +
  guides(color = "none", fill = "none") +
  labs(title = "Henley Passport Index Data", 
       subtitle = "<br>Passports ranked by the median number of countries that can be visited visa-free<br><span style='font-size:7pt'>Stamps show the median number of visa-free countries that could be visited at each 5-year mark</span>") +
  theme_void() +
  theme(axis.text.x = element_text(family = typewriter_font, size = rel(1.5), vjust = -2),
        plot.margin = margin(5, 5, 5, 5, "mm"),
        plot.background = element_rect(color = "transparent", fill = bg_col),
        plot.title.position = "plot",
        plot.title = element_text(family = typewriter_font, size = rel(2.6)),
        plot.subtitle = ggtext::element_textbox_simple(family = typewriter_font, size = rel(1.5), width = .7, hjust = 0),
        plot.caption = element_textbox(family = typewriter_font, size = rel(.6), hjust = 1, vjust = -50))

# Make corner stamp
stamp <- 
  ggplot() +
  annotate("richtext", x = 0, y = 0, label = "2010<br><br>2025", 
           size = 8, angle = -25, family = stamp_font, vjust = .3,
           fill = "transparent", color = "red3", label.color = "transparent") +
  annotate("richtext", x = 0, y = 0, label = "◌", 
           size = 45, angle = -25, family = stamp_font, fill = "transparent", color = "red3", 
           label.color = "transparent") +
  annotate("richtext", x = 0, y = 0, label = "Data: Henley & Partners via TidyTuesday<br>Packages: {tidyverse, ggbump, ggtext, patchwork}<br>Visualization: C. Börstell",
           size = 1.5, angle = -25, vjust = -.4,
           family = typewriter_font, fill = "transparent", color = "red3", label.color = "transparent") +
  theme_void()

# Combine plot and stamp
plot + inset_element(stamp, left = .75, bottom = .8, right = 1, top = 1.6)

# Save plot
ggsave("henley_passport_index.png", 
       width = 8, height = 7, units = "in", dpi = 600, bg = bg_col)

