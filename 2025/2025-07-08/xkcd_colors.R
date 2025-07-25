
# Load packages -----------------------------------------------------------

library(tidyverse)
library(scales)
library(colorspace)
library(marginaleffects)
library(ggtext)
library(ggforce)
library(glue)



# Read data ---------------------------------------------------------------

color_ranks <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/color_ranks.csv")
users <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/users.csv")

answers <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/answers.csv") |> 
  left_join(users) |> 
  left_join(color_ranks |> rename(col_hex = hex), by = join_by(rank))

# Default ggplot2 color palette
col_pal2 <- hue_pal()(2)

# Extract the 2nd color
col2 <- col_pal2[2]



# Custom functions --------------------------------------------------------

# Function to get the HLS (incl hue) from hex colors
hex2hls <- function(hex) {
  
  hls <- as(colorspace::hex2RGB(hex), "HLS")@coords
  as.list(hls)
  
}



# Processing --------------------------------------------------------------

# Get HLS of default color
col2_hls <- hex2hls(col2)

# Filter out uniform, reliable responses and get HLS color info
blue_green <- 
  answers |> 
  filter(spam_prob < .1,
         color %in% c("blue", "green"),
         colorblind == 0,
         monitor == "LCD") |> 
  mutate(hls = map(hex, hex2hls))

# Filter out colors that do not match the HLS range of the default color
target_range <- 
  blue_green |> 
  unnest_wider(hls, names_sep = "_") |> 
  filter(between(hls_2, col2_hls[[2]] - .05, col2_hls[[2]] + .05),
         hls_3 > col2_hls[[3]] - .1) |> 
  mutate(response = as.factor(color))

# Sort color responses by descending hue (for plotting)
color_responses <- 
  target_range |> 
  arrange(hls_1) |>
  mutate(y = case_when(
    color == "green" ~ .75,
    .default = .25
  )) 

# Fit a logistic regression model for hue predicting the response (color label)
mod <- glm(response ~ hls_1, data = target_range, family = binomial)

# Get model predictions
mod_predictions <- 
  plot_predictions(mod, condition = "hls_1", draw = F)

# Get predicted estimate of the default color
col2_prediction <- 
  predictions(mod, newdata = data.frame(hls_1 = col2_hls[[1]]))$estimate



# Plotting ----------------------------------------------------------------

# Plot the color naming (responses) + model prediction + annotations
ggplot() +
  
  # Color responses
  geom_tile(data = test, aes(x = hls_1, y = y, color = I(hex))) +
  
  # Annotations
  annotate("point", x = col2_hls[[1]], y = col2_prediction, 
           shape = 21, size = 2, fill = "black") +
  annotate("text", x = col2_hls[[1]], y = .5, 
           label = "×", size = 7, fill = "black", family = "Lato Bold") +
  annotate("text", x = 115, y = .77, 
           label = "green", color = "white", size = 15, family = "Lato") +
  annotate("text", x = 220, y = .25, 
           label = "blue", color = "white", size = 15, family = "Lato") +
  
  # Draw model predictions
  geom_line(data = mod_predictions, aes(x = hls_1, y = estimate), 
            linetype = 1, color = "grey20", linewidth = .5) +
  geom_line(data = mod_predictions, aes(x = hls_1, y = estimate), 
            linetype = 3, color = "black", linewidth = 1, lineend = "round") +
  
  # Annotations
  annotate("segment", x = 50, xend = 300, y = -0.05, lineend = "square", color = "grey40") +
  annotate("segment", x = 45, xend = 41, y = seq(from = 0, to = 1, length = 5), color = "grey40") +
  annotate("segment", x = 45, y = 0, yend = 1, lineend = "square", color = "grey40") +
  annotate("segment", x = seq(from = 50, to = 300, length = 6), y = -0.08, yend = -0.05, color = "grey40") +
  geom_mark_circle(aes(col2_hls[[1]], y = col2_prediction, 
                       label = paste0(round(col2_prediction*100, 1), "% probability"), 
                       description = glue('... of {col2} labeled "green"'),
                       x0 = 110, y0 = .25), 
                   label.family = "Lato", label.fontsize = 8, 
                   label.buffer = unit(10, "mm"), con.cap = 0, 
                   expand = unit(2, "mm"), linewidth = 1, label.minwidth = 50, label.fill = col2) +
  geom_mark_circle(aes(col2_hls[[1]], y = .5, 
                       label = col2, 
                       description = "One of the colors in the default {ggplot2} palette",
                       x0 = 225, y0 = .68), 
                   label.family = "Lato", label.fontsize = 8, 
                   label.buffer = unit(10, "mm"), con.cap = 0, 
                   expand = unit(.05, "mm"), linewidth = 1, label.minwidth = 40, label.fill = col2) +
  
  # Scaling and esthetics
  scale_x_continuous(breaks = pretty_breaks(), limits = c(40, 300)) +
  scale_y_continuous(labels = label_percent()) +
  labs(x = "<span style='font-family:Lato-Bold'>Color hue</span> in visual stimulus", 
       y = "<span style='font-family:Lato-Bold'>Probability</span> of <span style='color:green4;font-family:Lato-Bold'>green</span> named",
       title = glue("What is <span style='color:{col2}'>that default ggplot2 color</span>?"),
       subtitle = "Predicting color naming based on the xkcd Color Survey\n",
       caption = "Data: xkcd Color Survey via TidyTuesday | Packages: {tidyverse,colorspace,ggtext,ggforce,glue,marginaleffects,scales} | Visualization: @c_borstell") +
  theme_minimal(base_size = 15, 
                base_family = "Lato") +
  theme(panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.title = element_markdown(size = rel(1.5), family = "Lato Bold Italic"),
        plot.subtitle = element_text(family = "Lato-LightItalic", color = "grey50"),
        plot.caption = element_text(family = "Lato-Hairline", size = rel(.45), color = "black"),
        axis.title = element_markdown(color = "grey20"),
        axis.text.x = element_text(margin = margin(-3, 0, 3, 0, "mm"), color = "grey40"),
        axis.text.y = element_text(margin = margin(0, -5, 0, 0, "mm"), color = "grey40"),
        plot.margin = margin(5, 5, 5, 10, unit = "mm"))

# Save plot
ggsave("xkcd_green.jpg", 
       width = 6, height = 4.5, units = "in", dpi = 600)


# Alt-text: A two-dimensional plot with the title "What is that default ggplot2 color?: Predicting color naming based on the xkcd Color Survey". The x-axis shows color hue, with more green colors on the left and more blue colors on the right. The y-axis shows the predicted probability of "green" being used as label for the color hue in the visual stimulus. Each survey response is organized by its hue in the visual stimulus (x-axis) and whether the label is green (top) or blue (bottom) on the y-axis. There is an overlaid line drawn to show the model's predicted probability of a color label being used at various hue points, with a sharp shift from "green" to "blue" around the middle (hue about 165). The ggplot2 default color is slightly to the right, indicating that it is predicted as "blue", with the model predicting only 6.5% of "green" responses for this hue. Caption reads "Data: xkcd Color Survey via TidyTuesday | Packages: {tidyverse,colorspace,ggtext,ggforce,glue,marginaleffects,scales} | Visualization: @c_borstell."
