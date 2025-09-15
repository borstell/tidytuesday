
# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggtext)



# Themes & colors ---------------------------------------------------------

black_sq <- "peachpuff4"
white_sq <- "floralwhite"
#bg_col <- "lightgoldenrod4"
bg_col <- "thistle4"
engraving_col <- "goldenrod"
engraving_col2 <- "cornsilk2"
engraving_col3 <- "cornsilk3"



# Read data ---------------------------------------------------------------

fide_sep <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_september.csv")



# Process data ------------------------------------------------------------

# Split players into birth year cohorts
fide_cohorts <- 
  fide_sep |> 
  filter(bday > 1920) |> 
  mutate(cohort = cut(bday, 
                      breaks = c(-Inf, 1950, 1960, 1970, 1980, 1990, 2000, 2010, Inf), 
                      right = F,
                      labels = c("1920-1949", "1950-1959", "1960-1969", "1970-1979", 
                                 "1980-1989", "1990-1999", "2000-2009", paste0("2010-", max(bday))))) |> 
  mutate(x = cut(bday, 
                 breaks = c(-Inf, 1950, 1960, 1970, 1980, 1990, 2000, 2010, Inf),
                 right = F,
                 labels = 1:8)) |> 
  mutate(x = as.numeric(x))

# Calculate country ranks by cohort 
fide_cohort_rank <- 
  fide_cohorts |> 
  count(cohort, x, fed, sort = T) |>
  mutate(rank = row_number(), .by = cohort) |> 
  mutate(perc = n / sum(n), .by = cohort)

# Find unique countries among top-8
fide_cohort_rank |> 
  filter(rank < 9) |> 
  pull(fed) |> 
  unique()

# Data frame with flag emojis for top-8 countries
flags <- 
  tibble(
    fed = c("IND", "ESP", "GER", "FRA", "RUS", "POL", "ITA", "SRI", "NED", "CZE", "IRI", "USA", "SRB", "DEN", "SWE", "AUT"),
    emo = c("🇮🇳", "🇪🇸", "🇩🇪", "🇫🇷", "🇷🇺", "🇵🇱", "🇮🇹", "🇱🇰", "🇳🇱", "🇨🇿", "🇮🇷", "🇺🇸", "🇷🇸", "🇩🇰", "🇸🇪", "🇦🇹")
  )

# Custom labels for plot
cohort_labs <- c("**1920**     <br>     --**1949**", "**1950**     <br>     --**1959**", 
                 "**1960**     <br>     --**1969**", "**1970**     <br>     --**1979**", 
                 "**1980**     <br>     --**1989**", "**1990**     <br>     --**1999**", 
                 "**2000**     <br>     --**2009**", "**2010**     <br>     --**2021**")
names(cohort_labs) <- levels(fide_cohort_rank$cohort)



# Plot data ---------------------------------------------------------------

# Get top-8 countries only
fide_cohort_rank |> 
  filter(rank < 9) |> 
  
  # Join with flag emojis
  left_join(flags) |> 
  
  # Find which squares are black
  mutate(is_black = map2_lgl(rank, x, \(r, x) r %in% seq((1 + x %% 2), 8, by = 2))) |> 
  
  # Conditional fills for black/white squares
  mutate(fill_col = case_when(
    is_black ~ black_sq,
    .default = white_sq
  )) |> 
  
  # Conditional text colors for black/white squares
  mutate(text_col = case_when(
    !is_black ~ "grey20",
    .default = white_sq
  )) |> 
  
  # Plot data
  ggplot() +
  
  # Chessboard squares
  geom_tile(aes(x = cohort, y = rank, fill = I(fill_col))) +
  
  # Flag emojis
  geom_richtext(aes(x = cohort, y = rank, label = emo), 
                label.size = 0, fill = "transparent", size = 17) +
  
  # Stats for each chess square
  geom_richtext(aes(x = cohort, y = rank, label = paste0("**", n, "**", " (", round(perc*100), "%)"), color = I(text_col)), 
                label.size = 0, fill = "transparent", size = 4, nudge_y = -.3) +
  
  # Customization and labels
  scale_y_reverse(breaks = 1:8, labels = 1:8, expand = c(0, 0), sec.axis = dup_axis()) +
  scale_x_discrete(expand = c(0, 0), sec.axis = dup_axis(),
                   labels = cohort_labs) +
  coord_equal() +
  labs(title = "FIDE chess players by country & year of birth",
       subtitle = "<br>**Ranking** of the top International Chess Federation (FIDE) **countries** by the number of rated players<br>(Elo rating ⩾1400) per age group (**year of birth**)<span style='font-size:12pt'><br>Numbers under the flags show the number of players (with percentage of totals in brackets)<br></span>",
       caption = "**Data:** FIDE (September 2025) via TidyTuesday | **Packages:** {ggtext, tidyverse} | **Visualization:** C. Börstell") +
  guides(fill = "none") +
  theme_void(paper = bg_col) +
  theme(axis.text.y = element_text(color = engraving_col2, 
                                   face = "bold", 
                                   size = rel(1.8),  
                                   margin = margin(0, 8, 0, 8, "pt")),
        axis.text.x = element_markdown(color = engraving_col2, 
                                       size = rel(1.5),
                                       margin = margin(7, 0, 5, 0, "pt")),
        panel.border = element_rect(color = engraving_col, linewidth = 2),
        plot.margin = margin(15, 5, 5, 5, "mm"),
        plot.title.position = "panel",
        plot.title = element_textbox_simple(color = engraving_col2, 
                                            size = rel(4.6),
                                            face = "bold",
                                            hjust = .5,
                                            width = 1),
        plot.subtitle = element_textbox_simple(color = engraving_col3,
                                               width = .95, 
                                               hjust = .5,
                                               face = "italic",
                                               size = rel(2)),
        plot.caption = element_markdown(color = engraving_col3,
                                        hjust = .5))

# Save plot
ggsave("fide_ratings.png", width = 9, height = 13.5, units = "in", bg = bg_col, dpi = 600)

