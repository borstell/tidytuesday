
# Load packages -----------------------------------------------------------

library(tidyverse)
library(patchwork)
library(ggchicklet) # "hrbrmstr/ggchicklet"



# Colors ------------------------------------------------------------------

sofa1 <- "orange"
sofa2 <- "orange2"
sofa3 <- "orange3"
sofa4 <- "goldenrod"



# Read data ---------------------------------------------------------------

ikea_seating <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-10-27/ikea_seating.csv")



# Process data ------------------------------------------------------------

# Extract n-seat sofas only
sofas <- 
  ikea_seating |>
  filter(str_detect(product_type, "^\\d-seat sofa$")) |> 
  mutate(seats = as.numeric(str_extract(product_type, "^\\d")))

# Calculate average USD cost per seat
avg_sofas <- 
  sofas |> 
  mutate(seat_price_usd = price_usd / seats) |> 
  summarize(mean_seat_price_usd = mean(seat_price_usd), .by = country) |> 
  mutate(minus = min(mean_seat_price_usd) / mean_seat_price_usd)

# Make selected country labels
countries <- 
  tibble(
    country = c("ca", "de", "se", "mx", "th"),
    name = c("Cänadå", "Germäny", "Swëden", "Mexicö", "Thailånd")
  )



# Plot data ---------------------------------------------------------------

# Side banner plot
banner <- 
  ggplot() +
  annotate("text", x = I(.35), y = I(.05), label = "IKEA sofa index", angle = 90, hjust = 0,
           family = "Helvetica Bold", color = "dodgerblue4", size = 14) +
  annotate("text", x = I(.65), y = I(.055), label = str_glue("How much IKEA couch do you get for ${format(round(min(avg_sofas$mean_seat_price_usd)*5, -2), big.mark = ',')} US?"), angle = 90, hjust = 0,
           family = "Helvetica Bold", color = "grey20", lineheight = .9, size = 4.5) +
  annotate("text", x = I(.8), y = I(.055), label = "Data: Ikea | Packages: {tidyverse, ggchicklet, patchwork} | Visualization: C. Börstell", angle = 90, hjust = 0,
           family = "Helvetica Bold", color = "goldenrod1", size = 2.5) +
  theme_void(paper = "gold")

# Couch plot
couch <- 
  avg_sofas |> 
  inner_join(countries) |> 
  ggplot() +
  annotate("chicklet", x = I(seq(.185, .815, length = 5)), y = I(.75), width = .157, radius = unit(7, "pt"), size = 0, fill = sofa2) +
  annotate("chicklet", x = I(seq(.185, .815, length = 5)), y = I(.4), width = .157, radius = unit(3, "pt"), size = 0, fill = sofa1) +
  annotate("chicklet", x = I(.5), y = I(.22), width = .8, radius = unit(3, "pt"), size = 0, fill = sofa3) +
  annotate("chicklet", x = I(c(.055, .945)), y = I(.6), width = .105,, radius = unit(5, "pt"), size = 0, fill = sofa1) +
  geom_rect(aes(xmin = I(minus), xmax = I(1), ymin = I(0), ymax = I(1)), 
            color = NA, fill = "white") +
  geom_segment(data = \(x) filter(x, country != "th"),
               aes(x = I(minus), y = I(0), yend = I(.85)), linewidth = .8, linetype = 2) +
  geom_text(data = \(x) filter(x, country != "th"),
            aes(x = I(minus + .03), y = I(.85), label = "\u2701"), 
            angle = 270, size = 7) +
  geom_text(aes(x = I(.13), y = I(.11), label = str_to_upper(name)), 
            family = "Helvetica Bold", color = "white", hjust = 0) +
  geom_hline(yintercept = I(0), linewidth = 1) +
  facet_wrap(~fct_reorder(name, -minus), ncol = 1) +
  theme_void(paper = "white") +
  theme(strip.text = element_blank(),
        plot.margin = margin(5, 5, 5, 5, "pt"))

# Combine plot
banner + couch + plot_layout(widths = c(1, 1.75))



# Save plot ---------------------------------------------------------------

ggsave("ikea.png", height = 5, width = 4, units = "in", dpi = 600)



# Alt-text ----------------------------------------------------------------

# A graphic showing a "IKEA sofa index: How much IKEA couch do you get for US $1000?". Thailand is the baseline, giving you a full 5-seat couch for the amount, whereas in Mexico, you get less about a 2-seat couch. Country names Thailand, Canada, Sweden, Germany and Mexico are given with faux umlauts. Data: Ikea; Packages: {tidyverse, ggchicklet, patchwork}; Visualization: C. Börstell.


