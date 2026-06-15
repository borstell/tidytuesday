
# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggforce)
library(patchwork)
library(proporz)



# Read data ---------------------------------------------------------------

england_wales_names <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-16/england_wales_names.csv")

ni_names <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-16/ni_names.csv")

scotland_names <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-16/scotland_names.csv")



# Process data ------------------------------------------------------------

# Extract years in common across datasets
first_year <- 
  max(
    c(
      min(england_wales_names$Year), 
      min(ni_names$Year), 
      min(scotland_names$Year)
      )
    )

last_year <- 
  min(
    c(
      max(england_wales_names$Year), 
      max(ni_names$Year), 
      max(scotland_names$Year)
    )
  )

# Unify and get proportions per country
uk_names <- 
  bind_rows(
    england_wales_names |> mutate(Country = "England & Wales"),
    ni_names |> mutate(Country = "Northern Ireland"),
    scotland_names |> mutate(Country = "Scotland")
  ) |> 
  filter(Year >= first_year,
         Year <= last_year) |> 
  summarize(Number = sum(Number, na.rm = T), .by = c(Country, Sex, Name)) |> 
  filter(n() == 3, .by = c(Sex, Name)) |> 
  arrange(Name, Sex, desc(Number)) |> 
  mutate(Rank = row_number(), .by = c(Name, Sex)) |> 
  mutate(Prop = Number / sum(Number), .by = c(Name, Sex))

# Function for plotting individual names with name tag
plot_name <- function(df, name, sex) {
  sticker <- 
    ggplot() +
    geom_regon(aes(x0 = 0, y0 = 0, sides = 4, angle = 0, r = .5), 
               radius = unit(1, "cm"), fill = "coral2") +
    geom_shape(aes(x = c(-.35, -.35, .35, .35), y = c(-.25, .07, .07, -.25)), 
               fill = "white") +
    annotate("text", x = 0, y = .25, label = "Hello", 
             size = 18, family = "Futura", color = "white") +
    annotate("text", x = 0, y = .13, label = "my name is", 
             size = 8, family = "Futura", color = "white") +
    annotate("text", x = 0, y = -.1, label = name, family = "Fuzzy Bubbles",
             size = 20, color = "grey10") +
    theme_void(paper = "#FAFAFA")
  
  plot <- 
    df |> 
    filter(Name == name, Sex == sex) |> 
    mutate(N100 = proporz::divisor_round(Number, n_seats = 100)) |> 
    uncount(N100) |> 
    mutate(x = rep(10:1, 10),
           y = rep(1:10, each = 10)) |> 
    ggplot() +
    geom_tile(aes(x, y, fill = Country), color = "#FAFAFA", linewidth = 1.5) +
    scale_fill_manual(values = c("firebrick3", "antiquewhite3", "dodgerblue4")) +
    coord_equal() +
    labs(fill = NULL) +
    theme_void(base_size = 15, 
               base_family = "Archivo Narrow",
               paper = "#FAFAFA")
  
  sticker / plot + plot_layout(heights = c(5, 8))
}

# Sample names to plot
sample_names <- 
  uk_names |> 
  filter(sum(Number) >= 1000, 
         all(Prop > 0.01),
         .by = c(Sex, Name)) |> 
  slice_sample(n = 1, by = c(Country, Rank, Sex)) |> 
  filter(Rank == 1) |> 
  arrange(Sex, Country)

# Plot sampled names
names_to_plot <- 
  map2(
    sample_names$Name,
    sample_names$Sex,
    \(name, sex) plot_name(uk_names, name, sex)
  )


# Plot data ---------------------------------------------------------------

# Custom header
header <- 
  ggplot() +
  annotate("text", x = I(.5), y = I(.65), label = str_glue("Out of 100 people in the UK named ____\nin {first_year}–{last_year}, how many are found in …"),
           family = "Archivo Narrow", size = 24, lineheight = .85) +
  annotate("label", x = I(.215), y = I(.24), label = "England & Wales", 
           label.r = unit(0, "pt"), label.padding = unit(15, "pt"), linewidth = 0, 
           fill = "firebrick3", color = "white", family = "ArchivoNarrow-Bold", size = 20) +
  annotate("label", x = I(.58), y = I(.24), label = "Northern Ireland", 
           label.r = unit(0, "pt"), label.padding = unit(15, "pt"), linewidth = 0, 
           fill = "antiquewhite3", color = "black", family = "ArchivoNarrow-Bold", size = 20) +
  annotate("label", x = I(.865), y = I(.24), label = "Scotland", 
           label.r = unit(0, "pt"), label.padding = unit(15, "pt"), linewidth = 0, 
           fill = "dodgerblue4", color = "white", family = "ArchivoNarrow-Bold", size = 20) +
  annotate("text", x = I(.5), y = I(.05), label = "Data:  Office for National Statistics, Northern Ireland Statistics and Research Agency and National Records of Scotland. | Packages: {tidyverse, ggforce, patchwork, proporz} | Visualization: C. Börstell",
           family = "Archivo Narrow", size = 4.5) +
  theme_void(paper = "#FAFAFA")

# Combine header with name tags and population tiles
header / 
  wrap_plots(names_to_plot) + 
  plot_layout(guides = "collect", heights = c(.2, .8)) & 
  theme(legend.position = "none")


# Save plot ---------------------------------------------------------------

ggsave("uk_names.png", width = 15, height = 20, units = "in", bg = "#FAFAFA", dpi = 600)



# Alt-text ----------------------------------------------------------------

# A complex graphic with the title "Out of 100 people in the UK named ___ in 1997-2024, how many are found in ... England & Wales, Northern Ireland, Scotland". Underneath, there are six panels each with a header resembling the "Hello my name is" name tag sticker, with a different name filled out for each (Ryan, Padraig, Iain, Gemma, Aine, Catriona), above a 10x10 grid of filled squares representing the three country regions. Ryan & Gemma are mainly England & Wales; Padraig & Aine are mainly Northern Ireland; Iain & Catriona are mainly Scotland.
