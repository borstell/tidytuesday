
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggtext)
library(imager)


# Read data ---------------------------------------------------------------

#fair_use_findings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-29/fair_use_findings.csv")
fair_use_cases <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-29/fair_use_cases.csv")



# Read pipe image ---------------------------------------------------------

# Read pipe-like image file
img <- imager::load.image("pipe_bw.png")

# Make data frame from image data and subset by subsampling pixels
pipe <- img |> 
  as.data.frame() |> 
  filter(value == min(value),
         cc == 1,
         x%%26==0 & y%%27==0) |> 
  arrange(x) |> 
  mutate(index = row_number()) |> 
  filter(index <= nrow(fair_use_cases))



# Plot data ---------------------------------------------------------------

# Plot the pipe data and fill points based on the fair use data
pipe |>
  mutate(fair = if_else(index <= nrow(filter(fair_use_cases, !fair_use_found)), "unfair", "fair")) |> 
  mutate(alpha = if_else(index <= nrow(filter(fair_use_cases, !fair_use_found)), .8, 1)) |> 
  ggplot() +
  geom_point(aes(x, y, fill = fair, alpha = alpha), 
             shape = 21, size = 3, show.legend = F) +
  scale_y_reverse() +
  scale_fill_manual(values=c("#683B2F", "grey20")) +
  coord_fixed() +
  labs(title="Out of 251 copyright cases,<br><span style='color:#683B2F;font-size:28pt'>101 were found to constitute fair use</span>",
       caption="Ceci <span style='color:grey80'>n'</span>est <span style='color:grey80'>pas</span> fair use.",
       subtitle="Visualization: @c_borstell | Data: U.S. Copyright Office Fair Use Index via TidyTuesday | Packages: {tidyverse,ggtext,imager}\n\n") +
  theme_void() +
  theme(plot.background = element_rect(color="transparent", fill="wheat"),
        plot.title = element_markdown(face="bold", 
                                      color="grey45", 
                                      family="DIN Condensed",
                                      size=rel(3.5),
                                      hjust=.5),
        plot.subtitle = element_text(family="DIN Condensed", 
                                     color="wheat4", 
                                     size=rel(.6),
                                     hjust=.5),
        plot.caption = element_markdown(hjust=.5, 
                                      family="Homemade Apple", 
                                      size=rel(2),
                                      color="grey30"))

# Save plot
ggsave(width=5, height=5, units="in", dpi=600)
