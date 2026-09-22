
# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggh4x)



# Read data ---------------------------------------------------------------

urban <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-22/urban.csv")



# Process data ------------------------------------------------------------

# 100x100 grid
ids <- 
  tibble(
    id = 1:10000,
    x = rep(1:100, 100),
    y = rep(1:100, each = 100)
  )

# Filter to Scandinavian capitals only and sample tile positions
# for the 100x100 grid based on the percentage green per decade
green_areas <- 
  urban |> 
  filter(cityName %in% c("Oslo", "Stockholm", "København (Copenhagen)")) |> 
  mutate(city = str_replace(cityName, "København \\(Copenhagen\\)", "Copenhagen")) |> 
  mutate(perc = as.integer(round(averageShareOfGreenAreaInCityUrbanAreaPct, 2) * 100)) |> 
  select(country = countryOrTerritoryName, city, year, perc) |> 
  drop_na() |> 
  uncount(perc) |> 
  mutate(id = sample(1:10000, size = n()), .by = city) |> 
  inner_join(ids)

# Power bars for the proportion to the max and previous decade
bars <- 
  green_areas |> 
  count(city, year) |> 
  mutate(y = -10) |> 
  mutate(x = n / 100) |> 
  mutate(mx = max(x), 
         diff10 = round((x / lag(x) - 1), 2) * 100,
         .by = city) |> 
  mutate(diff_string = case_when(
    diff10 > 0 ~ str_glue("+{diff10}%"),
    diff10 < 0 ~ str_glue("−{abs(diff10)}%"),
    diff10 == 0 ~ str_glue("±{diff10}%"),
    .default = NA
  )) |> 
  mutate(clr = case_when(
    x / mx < .75 ~ "goldenrod1",
    .default = "limegreen"
  ))



# Plot data ---------------------------------------------------------------

green_areas |> 
  ggplot() +
  geom_tile(data = ids, aes(x, y), fill = "transparent") +
  geom_segment(data = bars, aes(y, x = 5, xend = mx), 
               color = "grey85", linewidth = 3, lineend = "round") +
  geom_segment(data = bars, aes(y, x = 5, xend = x, color = I(clr)),
               linewidth = 1.7, lineend = "round") +
  geom_text(data = filter(bars, year != 1990), aes(x = 98, y = -8, label = diff_string),
            size = 3.25, hjust = 1) +
  geom_tile(aes(x, y), fill = "limegreen") +
  coord_equal() +
  labs(x = NULL, y = NULL,
       caption = "Data: UN Habitat Urban Indicators Dataset via TidyTuesday | Packages: {tidyverse, ggh4x} | Visualization: C. Börstell",
       title = "Proportion of green areas across\nScandinavian capitals") +
  facet_nested_wrap(vars(city, year), 
                    nest_line = element_line(color = "black")) +
  theme_void(base_size = 18, base_family = "PT Sans Narrow", paper = "aliceblue") +
  theme(strip.text = element_text(hjust = 0, 
                                  margin = margin(t = 1, b = 1, l = 1, unit = "mm"),
                                  color = "grey10"),
        plot.caption = element_text(size = rel(.39),
                                    color = "grey80"),
        plot.margin = margin(6, 5, 4, 3, unit = "mm"),
        plot.title = element_text(lineheight = .8,
                                  face = "bold",
                                  margin = margin(b = 2, unit = "mm")))


# Save plot ---------------------------------------------------------------

ggsave("urban_green_areas.png", width = 4.5, height = 6, units = "in", dpi = 600)



# Alt-text ----------------------------------------------------------------

# A small-multiples plot of the "Proportion of green areas across Scandinavian capitals: Copenhagen, Oslo and Stockholm". Each city has four panels - 1990, 2000, 2010, 2020 - showing a square of scattered green tiles representing the proportion of green areas. Below each square is an power bar-type plot showing each year's fill compared to the maximum across the 1990-2020 span, and next to it is a text label showing the percent change from the previous decade. While Oslo has had a quite stable level, Copenhagen lost a lot of relative green space in 2010, and Stockholm has been decreasing steadily and quite dramatically from the overall highest proportion in 1990, about half the proportion of 1990 in 2020. Data: UN Habitat Urban Indicators Dataset via TidyTuesday; Packages: {tidyverse, ggh4x}; Visualization: C. Börstell
