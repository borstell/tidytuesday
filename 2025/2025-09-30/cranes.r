
# Load packages -----------------------------------------------------------

library(tidyverse)
library(magick)





# Themes & colors ---------------------------------------------------------

font <- "Rock Salt"
pencil_color <- "grey30"
paper_color <- "snow2"



# Read data ---------------------------------------------------------------

# Read crane observation data
cranes <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-30/cranes.csv")

# Read image of cranes in flight from Wikimedia Commons
# Photo by Gllawm (CC BY-SA 4.0)
# URL: https://commons.wikimedia.org/wiki/File:Grus_grus_(Séligné)_07032024_02.jpg
raw_img <- 
  magick::image_read("https://upload.wikimedia.org/wikipedia/commons/4/41/Grus_grus_%28Séligné%29_07032024_02.jpg")



# Process data ------------------------------------------------------------

# Extract crane subimage 1 (three cranes in group)
img1 <- 
  raw_img |> 
  magick::image_crop(geometry = "1200x620+3500+2700") |> 
  magick::image_convert("png") |> 
  magick::image_transparent(color = "gray95", fuzz = 10) |> 
  magick::image_raster(tidy = F)

# Extract crane subimage 2 (single crane with straight wings)
img2 <- 
  raw_img |> 
  magick::image_crop(geometry = "500x500+1090+2650") |> 
  magick::image_convert("png") |> 
  magick::image_transparent(color = "gray95", fuzz = 10) |> 
  magick::image_raster(tidy = F)

# Extract crane subimage 3 (single crane with bent wings)
img3 <- 
  raw_img |> 
  magick::image_crop(geometry = "500x350+220+3400") |> 
  magick::image_convert("png") |> 
  magick::image_transparent(color = "gray95", fuzz = 10) |> 
  magick::image_raster(tidy = F)

# Process crane observations
crane_observations <- 
  cranes |>
  
  # Filter to spring observations only
  filter(month(date) %in% 1:7) |> 
  arrange(date) |> 
  
  # Drop observation reports that are blank (NA)
  drop_na(observations) |> 
  
  # Extract year from date
  mutate(year = year(date)) |> 
  
  # Find observation dates equal to the highest number of observations by year
  # and single out which date is the earliest in the year to reach the peak
  mutate(max_day = observations == max(observations, na.rm = T), 
         first_max = cumsum(max_day == TRUE),
         .by = year) |> 
  mutate(first_max_by_year = case_when(
    max_day & first_max == 1 ~ yday(date),
    .default = NA)) |> 
  
  # Calculate the proportional observations to that year's maximum
  mutate(prop_obs = observations / max(observations), 
         .by = year) |> 
  
  # Add year sequence starting at 1
  mutate(seq_year = consecutive_id(year))



# Plot data ---------------------------------------------------------------

# Plot crane observations
crane_observations |> 
  ggplot() +
  
  # Draw ridgelines per year for the proportionally scaled observations
  # NB: Only including dates with more than 5% of the year's maximum
  geom_path(data = \(x) filter(x, prop_obs > .05), 
            aes(x = yday(date), y = seq_year + prop_obs, group = seq_year), color = pencil_color) +
  
  # Draw X'es at the first observation maximum for each year
  geom_point(data = \(x) filter(x, !is.na(first_max_by_year)), 
             aes(x = first_max_by_year, y = seq_year + 1), shape = "✗", size = 3, color = pencil_color) +
  
  # Add mean and median lines for first observation maximums across years
  geom_vline(aes(xintercept = mean(first_max_by_year, na.rm = T)), color = pencil_color) +
  geom_vline(aes(xintercept = median(first_max_by_year, na.rm = T)), lty = 2, color = pencil_color) +
  
  # Add custom y-axis labels for years
  annotate("text", x = 70, y = seq(1, 31, by = 5) + .5, label = seq(1994, 2024, by = 5), family = font, color = pencil_color) +
  
  # Add custom crane images to the margins of the plot
  annotation_raster(img1, xmin = 72, xmax = 88, ymin = 1, ymax = 6) +
  annotation_raster(img2, xmin = 107, xmax = 117, ymin = 28, ymax = 34) +
  annotation_raster(img2, xmin = 110, xmax = 118, ymin = 23, ymax = 28) +
  annotation_raster(img2, xmin = 73, xmax = 80, ymin = 10, ymax = 14) +
  annotation_raster(img3, xmin = 111, xmax = 119, ymin = 13, ymax = 16) +
  
  # Scale the x-axis with relevant dates
  scale_x_continuous(breaks = yday(c("1990-03-15", "1990-04-01", "1990-04-15", "1990-04-30")), 
                     labels = c("Mar 15", "Apr 1", "Apr 15", "Apr 30")) +
  
  # Customization
  labs(title = "Cranes at Lake Hornborgasjön", 
       subtitle = "\nObservations of cranes at the Swedish lake peak earlier over time\n\n\n✗ First observation maximum by year       — Mean       -- Median\n\n",
       caption = '\n\nData: "Transtatistik", Naturum Hornborgasjön via TidyTuesday | Image: Gllawm, Wikimedia Commons curid=147233798 | Packages: {tidyverse, magick} | Visualization: C. Börstell') +
  theme_void() +
  theme(plot.margin = margin(5, 10, 5, 10, "mm"),
        axis.text.x = element_text(family = font, color = pencil_color),
        plot.title = element_text(family = font, hjust = .5, size = rel(2.1), color = "grey20"),
        plot.subtitle = element_text(family = font, hjust = .5, size = rel(1), color = pencil_color),
        plot.caption = element_text(family = font, hjust = 1, size = rel(.35)))

# Save plot
ggsave("cranes.png", width = 7, height = 8, units = "in", dpi = 600, bg = paper_color)

