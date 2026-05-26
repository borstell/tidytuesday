
# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggrepel)
library(ggtext)
library(gt)
library(marginaleffects)
library(marquee)
library(scales)
library(WDI)



# Read data ---------------------------------------------------------------

# GDP data
gdp <- 
  WDI(indicator = "NY.GDP.PCAP.PP.KD", start = 2024, end = 2024) |> 
  rename(gdppc = NY.GDP.PCAP.PP.KD)

# Population data
pop2024 <- 
  countrypops |> 
  filter(year == 2024)

# Crossref data
metadata_coverage_stats_by_country <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-19/metadata_coverage_stats_by_country.csv")



# Process data ------------------------------------------------------------

papers <- 
  metadata_coverage_stats_by_country |> 
  filter(current_up_to == "2024-12-31") |> 
  filter(document_type == "journal-article") |> 
  select(1:6) |> 
  inner_join(gdp, by = join_by(iso3_code == iso3c)) |> 
  drop_na(gdppc) |> 
  inner_join(pop2024, by = join_by(year, iso3_code == country_code_3)) |> 
  mutate(ppmc = n_dois / population * 1e6) |> 
  mutate(prop = ppmc / gdppc) |> 
  mutate(country_name = str_remove(country, ", .*|Darussalam")) |> 
  mutate(country_name = str_remove(country_name, " SAR.*| PDR")) |> 
  mutate(country_name = str_replace(country_name, " and ", " & "))



# Model data --------------------------------------------------------------

mod <- lm(log(ppmc) ~ log(gdppc), data = papers)

fx <- 
  marginaleffects::avg_predictions(mod, by = c("gdppc"), transform = "exp") |> 
  as_tibble()



# Plot data ---------------------------------------------------------------

papers |> 
  ggplot() +
  geom_ribbon(data = fx, aes(x = gdppc, ymin = conf.low, ymax = conf.high), 
              fill = "antiquewhite2", color = "antiquewhite2", linetype = 2, alpha = .5) +
  geom_line(data = fx, aes(x = gdppc, y = estimate), color = "antiquewhite3", linewidth = 1) +
  geom_point(aes(x = gdppc, y = ppmc, fill = log(prop)), 
             shape = 21, stroke = .75, color = "grey30", show.legend = F) +
  geom_text_repel(aes(x = gdppc, y = ppmc, label = country_name), 
                  family = "PT Sans Narrow", color = "grey30", bg.color = "ghostwhite", 
                  min.segment.length = 999) +
  annotate(GeomMarquee, x = I(-.15), y = I(.2), 
           label = "{.grey80 Visualization:} C. Börstell  
           {.grey80 Data:} Crossref & the World Bank  
           {.grey80 Packages:} {tidyverse, ggrepel, ggtext,  
           gt, marginaleffects, marquee, scales, WDI}",
           family = "PT Sans Narrow", 
           angle = 90,
           size = rel(3.5),
           lineheight = .8,
           color = "grey85") +
  scale_x_log10(labels = label_comma(prefix = "$")) +
  scale_y_log10(labels = label_comma()) +
  scale_fill_viridis_c(option = "inferno", begin = .4) +
  labs(x = "GDP per capita (USD)",
       y = "Journal articles per\n  1,000,000 people",
       title = "Crossref registered journal articles by country, population & GDP",
       subtitle = "Looking at the Crossref database records by end of 2024, 
       there is a clear correlation in the **total number of journal articles 
       recorded by country**, relative to its population, and the country's **GDP per capita**,
       with richer countries being responsible for more scholarly output, reflecting a bias 
       in available academic resources.<br>") +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 14, 
                base_family = "PT Sans Narrow",
                paper = "whitesmoke") +
  theme(axis.title.x = element_text(color = "antiquewhite4", 
                                    face = "bold",
                                    hjust = 1),
        axis.title.y = element_text(angle = 0, 
                                    color = "antiquewhite4", 
                                    face = "bold",
                                    margin = margin(r = -10, unit = "mm")),
        axis.text = element_text(color = "antiquewhite4"),
        plot.title.position = "plot",
        plot.title = element_text(size = rel(1.65),
                                  color = "grey20",
                                  face = "bold"),
        plot.subtitle = element_textbox_simple(color = "antiquewhite4",
                                               width = .985,
                                               lineheight = .8,
                                               margin = margin(t = 2, b = 2, l = 0, unit = "pt")),
        plot.caption = element_markdown(angle = 0,
                                        halign = -100,
                                        valign = -100))


# Save plot ---------------------------------------------------------------

ggsave("crossref.png", width = 7.6, height = 6, units = "in", dpi = 600)



# Alt-text ----------------------------------------------------------------

# A scatterplot showing the number of Crossref registered journal articles by country, population & GDP, showing a log-linear correlation between journal articles published by 1 million people and GDP per capita, reflecting a bias in available academic resources.
