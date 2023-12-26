
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggtext)
library(scales)




# Themes ------------------------------------------------------------------

theme_set(theme_minimal(base_size = 15, base_family = "Lexend"))



# Read data ---------------------------------------------------------------

cran_20221122 <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-26/cran_20221122.csv")



# Data processing ---------------------------------------------------------

cran <- cran_20221122 |> 
  select(package, date) |> 
  rowwise() |> 
  mutate(c_first = first(first(str_split(package, ""))),
         c_last = last(first(str_split(package, "")))) |> 
  mutate(caps = paste0(first(str_extract_all(package, pattern = "[A-Z]")), collapse = ""),
         smol = paste0(first(str_extract_all(package, pattern = "[a-z]")), collapse = "")) |> 
  ungroup()



# Plot data ---------------------------------------------------------------

cran |> 
  filter(c_last %in% c(letters, LETTERS)) |> 
  mutate(only_upper = c_last %in% LETTERS & c_last==caps) |> 
  ggplot() +
  geom_bar(aes(x=toupper(c_last), fill=only_upper, alpha=c_last %in% c("r", "R")), show.legend = F, color="grey20", linewidth = .2) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = c("dodgerblue", "orange")) +
  scale_alpha_manual(values = c(.2, .9)) +
  labs(x = NULL, y = NULL, title = "Last Lett<span style='color:dodgerblue'>R</span> of <span style='color:dodgerblue'>R</span> Packages on C<span style='color:dodgerblue'>R</span>AN (2022-11-22)",
       subtitle = "Package names where <span style='font-family:Lexend'>only</span> the last letter is <span style='color:orange;font-family:Lexend'>UPPER CASE</span>",
       caption = "<span style='font-family:Lexend'>Data:</span> Padgham & Ross (2022) and {pkgstats} via TidyTuesday<br><span style='font-family:Lexend'>Packages:</span> {tidyverse,ggtext,scales} | <span style='font-family:Lexend'>Visualization:</span> @c_borstell") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(margin = margin(t = -10, unit = "pt")),
        plot.title = element_markdown(family = "Lexend-Bold", color="grey20"),
        plot.subtitle = element_markdown(family = "Lexend-Light", color="grey35"),
        plot.caption = element_markdown(family = "Lexend-Light", color="grey65", size=rel(.5), hjust = 0),
        plot.background = element_rect(color="transparent", fill="#FCFCFC"))




# Save plot ---------------------------------------------------------------

ggsave("./r_packages.jpg", width=7, height=5, units="in", dpi=600)

        