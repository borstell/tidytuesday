
# Load packages -----------------------------------------------------------

library(tidyverse)



# Read data ---------------------------------------------------------------

pi_digits <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-24/pi_digits.csv")



# Make color palette ------------------------------------------------------

pal <- colorRampPalette(c("gold", "orangered"))(10)


# Plot 1 million decimals of pi -------------------------------------------

pi_digits[2:1000001, ] |> 
  mutate(clr = pal[10 - digit]) |> 
  mutate(x = rep(1:1000, 1000),
         y = rep(1:1000, each = 1000)) |> 
  ggplot() +
  geom_tile(aes(x, y, fill = I(clr))) +
  annotate("text", x = 1, y = 1, label = "\u03C0", 
           size = 85, vjust = .35, family = "Times New Roman", 
           color = "orange", alpha = .4) +
  coord_polar() +
  theme_void(paper = "snow")

# Save plot
ggsave("pi.jpg", width = 4, height = 4, units = "in", dpi = 600)
