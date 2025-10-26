
# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggarrow)
library(ggdist)
library(marquee)
library(patchwork)
library(rsample)
library(rnaturalearth)
library(scales)



# Read data ---------------------------------------------------------------

# MDEQ data
flint_mdeq <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-04/flint_mdeq.csv")

# VT data
flint_vt <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-04/flint_vt.csv")

# Get Michigan map
michigan <- 
  rnaturalearth::ne_states(country = "United States of America") |> 
  filter(name == "Michigan")

# Coordinates for Flint
flint <- 
  tibble(name = "Flint",
         lon = -83.691971,
         lat = 43.02442)



# Process data ------------------------------------------------------------

# Function for getting the proportion of values above threshold
prop_above_threshold <- function(x, threshold = 15) {
  x <- x[!is.na(x)]
  length(x[x > threshold]) / length(x)
}

# Bootstrap each dataset
boot_mdeq1 <- 
  flint_mdeq |> 
  rsample::bootstraps(times = 10000) |> 
  mutate(above = map_dbl(splits, \(x) as_tibble(x) |> pull(lead) |> prop_above_threshold()))

boot_mdeq2 <- 
  flint_mdeq |> 
  rsample::bootstraps(times = 10000) |> 
  mutate(above = map_dbl(splits, \(x) as_tibble(x) |> pull(lead2) |> prop_above_threshold()))

boot_vt <- 
  flint_vt |> 
  rsample::bootstraps(times = 10000) |> 
  mutate(above = map_dbl(splits, \(x) as_tibble(x) |> pull(lead) |> prop_above_threshold()))

# Combine resamples and add labels 
flint_bootstraps <- 
  tibble(source = rep(c("**Michigan Dept.  \nof Environment**  \n(*n* = 69)", "**Michigan Dept.  \nof Environment**  \n(*n* = 71)", "**Virginia Tech**  \n(*n* = 271)"), each = 10000),
         abbr = rep(c("MDEQ", "VT"), c(20000, 10000)),
         above = c(boot_mdeq2$above, boot_mdeq1$above, boot_vt$above))



# Plot data ---------------------------------------------------------------

# Plot Michigan with Flint and custom water pipe and faucet emoji
flint_map <- 
  michigan |> 
  ggplot() +
  geom_sf(fill = "thistle", color = NA) +
  geom_point(data = flint, aes(lon, lat), size = 3, color = "grey20") +
  annotate("text", x = -86, y = 48, label = "Michigan, USA", size = 6, family = "Asap Condensed Bold", color = "grey20") +
  geom_text(data = flint, aes(lon, lat, label = name), size = 3.4, family = "Asap Condensed Bold", hjust = 1.3, vjust = 1.25, color = "grey20") +
  annotate("segment", x = flint$lon, y = flint$lat, xend = flint$lon, yend = 45, lineend = "round", size = 2, color = "grey35") +
  annotate("segment", x = flint$lon, y = 45, xend = flint$lon - 6, yend = 45, lineend = "round", size = 2, color = "grey35") +
  annotate("segment", x = flint$lon, y = 45.05, xend = flint$lon - 6, yend = 45.05, lineend = "round", size = .7, color = "steelblue", alpha = .5) +
  annotate("segment", x = flint$lon, y = 45.02, xend = flint$lon - 6, yend = 45.02, lineend = "round", size = .4, color = "grey75", alpha = .7) +
  annotate("point", x = flint$lon - 6.3, y = 44.88, shape = I("🚰"), size = 10) +
  annotate(GeomMarquee, x = flint$lon - 4.2, y = 43.3, 
           label = "In 2014, changes made to 
           the water supply source in Flint, Michigan 
           resulted in a public health crisis with
           many residents being exposed to dangerous
           levels of lead in their drinking water.", 
           family = "Asap Condensed", width = .29, lineheight = .8, size = 2.5) +
  theme_void()

# Plot bootstrap resamples
flint_lead <- 
  flint_bootstraps |> 
  ggplot() +
  ggdist::stat_gradientinterval(aes(x = above, y = fct_rev(source), fill = source), show.legend = F) +
  geom_vline(xintercept = .1, linetype = 3, color = "grey30") +
  annotate(GeomMarquee, x = .245, y = 3, 
           label = "Points show median values; whiskers show 66% and 95% intervals", 
           width = .15, lineheight = .8, fill = "snow",
           size = 2.5, family = "Asap Condensed", color = "grey30") +
  geom_arrow_curve(data = tibble(x = .22, xend = median(filter(flint_bootstraps, abbr == "VT")$above), y = 3, yend = 1.1), 
                   aes(x = x, y = y, xend = xend, yend = yend), 
                   curvature = .3, color = "grey35") +
  annotate(GeomMarquee, x = .063, y = 1, 
           #label = "% of above-threshold samples requiring official action", 
           label = "Threshold at which official action is required", 
           width = .13, lineheight = .8, fill = "snow",
           size = 2.5, family = "Asap Condensed", color = "grey30") +
  geom_arrow_curve(data = tibble(x = .068, xend = .099, y = .92, yend = .45), 
                   aes(x = x, y = y, xend = xend, yend = yend), 
                   curvature = .2, color = "grey35") +
  scale_x_continuous(breaks = breaks_pretty(), labels = label_percent()) +
  scale_fill_manual(values = c("wheat1", "wheat2", "salmon")) +
  labs(x = "Samples with a lead concentration above 15 parts per billion (ppb)", y = NULL) +
  theme_minimal(paper = "snow", base_size = 14, base_family = "Asap Condensed") +
  theme(axis.text.y.left = element_marquee(hjust = 0, margin = margin(0, -10, 0, 0, "pt")),
        axis.title.x = element_text(vjust = -1),
        panel.grid.major.y = element_blank())

# Combine and annotate plot
free(flint_lead) + flint_map + 
  plot_annotation(
    title = "Lead concentration in Flint water samples in 2015",
    subtitle = "In 2015, the Michigan Department of Environment (MDEQ) 
    collected 71 water samples in Flint to evaluate 
    the concentration of poisonous lead in the drinking water.
    The *Lead and Copper Rule* posits that if more than 10% of samples 
    are above 15 parts per billion (ppb), action is required by officials. 
    The MDEQ originally excluded two samples with high readings, 
    thus resulting in below-threshold readings overall. Suspicious of the results,
    a citizen science project coordinated by Prof. Marc Edwards and 
    colleagues at Virginia Tech collected 271 new samples, 
    which pointed to generally higher levels of lead concentration than the official 
    MDEQ data had suggested.  \n\nThe plot below shows the distribution of samples 
    in relation to the 10% threshold when each dataset is 
    resampled 10,000 times using bootstrapping (the MDEQ data bootstrapped both with and 
    without the samples originally excluded). When including the originally excluded 
    samples, both datasets would indicate that the Flint drinking water is likely to contain
    **dangerous levels of lead** at a rate which would require official action 
    for public safety.",
    caption = 
    "**Data:** MDEQ & Virginia Tech in Loux & Gibson (2018) via TidyTuesday |
    **Packages:** {tidyverse, ggarrow, ggdist, marquee, patchwork, rnaturalearth, rsample, scales} |
    **Visualization:** C. Börstell",
    theme = theme(
      panel.background = element_rect(fill = "snow", color = "snow"),
      plot.background = element_rect(fill = "snow", color = "snow"),
      plot.title.position = "plot",
      plot.title = element_text(family = "Asap Condensed Bold", 
                                color = "grey20", size = rel(2.65)),
      plot.subtitle = element_marquee(family = "Asap Condensed",
                                      width = .95, size = rel(.8)),
      plot.caption = element_marquee(family = "Asap Condensed", 
                                     color = "grey80",
                                     hjust = .3,
                                     size = rel(.75)),
      plot.margin = margin(3, 2, 2, 2, "mm"))
    )

# Save plot
ggsave("flint.png", width = 8, height = 4.8, units = "in", dpi = 600)

