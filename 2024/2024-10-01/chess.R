
# Load packages -----------------------------------------------------------

library(ggdist)
library(ggtext)
library(glue)
library(patchwork)
library(scales)
library(tidytext)
library(tidyverse)



# Directory ---------------------------------------------------------------

dirname <- ""



# Read data ---------------------------------------------------------------

chess <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-01/chess.csv")



# Process data ------------------------------------------------------------

# Find checks
checks <- 
  chess |> 
  unnest_regex(move, moves, to_lower = F) |> 
  mutate(move_n = row_number(), .by = game_id) |> 
  mutate(move_rel_n = rescale(move_n), .by = game_id) |> 
  filter(str_detect(move, "\\+")) |> 
  mutate(piece = case_when(
    str_sub(move, 1, 1) == "Q" ~ "queen",
    str_sub(move, 1, 1) == "K" ~ "king",
    str_sub(move, 1, 1) == "B" ~ "bishop",
    str_sub(move, 1, 1) == "R" ~ "rook",
    str_sub(move, 1, 1) == "N" ~ "knight",
    str_sub(move, 1, 1) == "O" ~ "castling",
    str_sub(move, 1, 1) == str_to_lower(str_sub(move, 1, 1)) ~ "pawn",
    .default = NA
  )) |> 
  mutate(square = str_sub(str_remove_all(str_remove_all(move, "\\+"), "=."), -2, -1)) |> 
  mutate(square = case_when(
    !str_sub(square, 1, 1) %in% letters ~ NA,
    .default = square
  )) |> 
  mutate(x = str_sub(square, 1, 1),
         y = as.numeric(str_sub(square, 2, 2)))

# Draw board
board <- 
  tibble(x = rep(letters[1:8], 8),
         y = rep(1:8, each = 8),
         i = 1:64) |> 
  mutate(fill = case_when(
    y%%2 == 0 & i%%2 == 0 ~ "wheat3",
    y%%2 != 0 & i%%2 != 0 ~ "wheat3",
    .default = "wheat"))

# Count squares on board where checks happen
sq <- 
  checks |> 
  count(square, x, y, sort = T) |>
  drop_na()


# Compile plots -----------------------------------------------------------

p1 <- 
  ggplot() +
  geom_tile(data = board, aes(paste0(x, "\n"), y, fill = fill)) +
  geom_point(data = sq, aes(paste0(x, "\n"), y, size = n, alpha = n, color = n)) +
  scale_y_continuous(breaks = pretty_breaks(8), expand = c(0, .2)) +
  scale_fill_identity() +
  scale_color_gradient(low = "orange", high = "darkred") +
  labs(title = "<br><span style='font-family:PTSans-Bold'>Where</span> do checks happen on the board?") +
  coord_fixed() +
  theme_void(base_family = "PT Sans Narrow", base_size = 15) +
  theme(legend.position = "none",
        axis.text = element_text(family = "PT Sans Bold", color = "wheat4"),
        plot.title = element_markdown(size = 25))

p2 <- 
  ggplot() +
  ggdist::stat_dots(data = checks, aes(x = y, fill = y > 4), 
                    quantiles = 50, color = "grey", alpha = .6, 
                    show.legend = F) +
  scale_x_continuous(expand = c(0.09, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("#FAFAFA", "grey5")) +
  coord_flip() +
  theme_void() +
  theme()

p3 <- 
  ggplot() +
  ggridges::geom_density_line(data = checks, aes(x = move_rel_n), fill = "wheat3", color = "wheat4") +
  guides(
    y = guide_axis(position = "none"),
    x = guide_axis(cap = "both")
  ) +
  scale_x_continuous(breaks = c(0, 1), labels = c("Start", "End")) +
  labs(x = NULL, y = NULL, title = "<span style='font-family:PTSans-Bold'>When</span> do checks happen in the game?") +
  theme_classic(base_family = "PT Sans Narrow", base_size = 15) +
  theme(axis.line = element_line(linewidth = .4),
        axis.ticks = element_line(linewidth = .4),
        plot.title = element_markdown(size = 25)) 

pieces <- 
  checks |> 
  count(piece, sort = T) |> 
  filter(!piece %in% c("castling")) |> 
  arrange(n) |> 
  mutate(color = factor(row_number()%%2))

p4 <- 
  pieces |> 
  mutate(label = glue("<img src='{dirname}chess-pieces/{piece}.png' width='15'/>")) |> 
  ggplot() +
  geom_col(aes(x = n, y = fct_reorder(label, n), fill = color), show.legend = F) +
  geom_text(aes(x = n*.6, y = fct_reorder(label, n), label = prettyNum(n, big.mark = " "), color = color), family = "PT Sans Bold", show.legend = F) +
  scale_x_log10(breaks = c(1e0, 1e1, 1e2, 1e3, 1e4, 1e5), labels = scales::label_number(), expand = c(0, 0)) +
  scale_fill_manual(values = c("wheat4", "wheat2")) +
  scale_color_manual(values = c("wheat1", "grey40")) +
  labs(x = NULL, y = NULL, title = "<span style='font-family:PTSans-Bold'>Which</span> pieces move to create a check?") +
  guides(
    y = guide_axis(cap = "none"),
    x = guide_axis(cap = "both")
  ) +
  theme_classic(base_family = "PT Sans Narrow", base_size = 15) +
  theme(axis.line.x = element_line(linewidth = .4),
        axis.line.y = element_blank(),
        axis.ticks.x = element_line(linewidth = .4),
        axis.ticks.y = element_blank(),
        axis.text = element_markdown(),
        plot.title = element_markdown(size = 25)) 


((p1 + p2) / p3) / p4 + plot_layout(heights = c(5, 1.5, 3)) +
  plot_annotation(title = "Checking in Chess", 
                  subtitle = "   Based on 20 058 Lichess games", 
                  caption = glue("<img src='{dirname}chess-pieces/queen.png' width='8'/> <span style='font-family:PTSans-Bold'>Data:</span> Lichess.org via Kaggle/Mitchell J <img src='{dirname}chess-pieces/rook.png' width='8'/><span style='font-family:PTSans-Bold'>Packages:</span> ggdist, ggtext, glue, patchwork, scales, tidytext, tidyverse <img src='{dirname}chess-pieces/knight.png' width='8'/> <span style='font-family:PTSans-Bold'>Visualization:</span> @c_borstell <img src='{dirname}chess-pieces/bishop.png' width='9'/>"),
                  theme = theme(
                    plot.title = element_text(family = "PT Sans Bold", size = rel(4)), 
                    plot.subtitle = element_text(family = "PT Sans Narrow", size = rel(2), color = "wheat3"), 
                    plot.caption = element_markdown(family = "PT Sans Narrow", size = rel(.8), color = "wheat4"),
                    plot.margin = margin(10, 5, 10, 5, "mm")))


# Save plot ---------------------------------------------------------------

ggsave("chess.png", width = 7, height = 9, units = "in", dpi = 600)

