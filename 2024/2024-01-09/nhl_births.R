
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(geomtextpath)
library(ggtext)



# Read data ---------------------------------------------------------------

nhl_player_births <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_player_births.csv")



# Data processing ---------------------------------------------------------

# Find min and max years
min(filter(nhl_player_births, birth_country=="CAN")$birth_year)
max(filter(nhl_player_births, birth_country=="CAN")$birth_year)
1919-1879
1949-1920
1979-1950
2005-1980

birth_groups <- nhl_player_births |> 
  filter(birth_country == "CAN") |> 
  slice(1, .by = player_id) |> 
  mutate(birth_year = as.numeric(str_sub(birth_date, 1, 4))) |> 
  
  mutate(birth_group = case_when(
    birth_year < 1920 ~ "1879-1919",
    birth_year < 1950 ~ "1920-1949",
    birth_year < 1980 ~ "1950-1979",
    .default = "1980-2005"
  )) |> 
  mutate(qrtr = (birth_month-1)%/%3+1) |> 
  mutate(country = case_when(
    birth_country == "CAN" ~ "Canada",
    .default = birth_country)
  ) |> 
  count(qrtr, country, birth_group) |> 
  mutate(qrtr_lab = case_when(
    qrtr == 1 ~ "Jan-Mar",
    qrtr == 2 ~ "Apr-Jun",
    qrtr == 3 ~ "Jul-Sep",
    qrtr == 4 ~ "Oct-Dec",
    .default = NA_character_
  )) |> 
  mutate(perc = n/sum(n), .by = c(country, birth_group))


# Plot data ---------------------------------------------------------------

ggplot() +
  geom_col(data=birth_groups, 
           aes(x=fct_reorder(qrtr_lab, qrtr), 
               y=perc, 
               fill=perc), 
           width = 1, show.legend = F) +
  geom_text(data=birth_groups, 
            aes(x=qrtr_lab, 
                y=.11, 
                label=paste0(round(perc, 2)*100, "%")),
            family="Lexend", color="white") +
  coord_curvedpolar() +
  scale_fill_gradient(low = "grey20", high = "grey10") +
  labs(title="The Lopsided Puck",
       subtitle="<br><span style='font-family:Lexend-Bold;'>Canadian-born ice hockey players in the NHL<br>are born earlier and earlier in the year</span><br>___________________________________________<br><br><span style='color:dodgerblue4;font-family:Lexend-Bold;'>Players by birth quarter from 1879 to 2005</span><br><br>",
       caption="Data: NHL API (via TidyTuesday) | Packages: {tidyverse,ggtext,geomtextpath} | Visualization: @c_borstell") +
  facet_wrap(birth_group~., ncol=4) +
  theme_void(base_size = 15) +
  theme(axis.text.x = element_text(family="Atkinson Hyperlegible Bold", size=rel(.8), color="dodgerblue3"),
        strip.text = element_text(size=rel(1.4), family="Lexend-Bold", color="dodgerblue4"),
        plot.title = element_text(size=rel(4.5), family="Faster One", color="grey20", hjust=.5),
        plot.subtitle = element_markdown(size=rel(1.8), family="DIN Alternate Bold", color="red2", hjust=.5),
        plot.caption = element_markdown(size=rel(.8), family="Lexend", color="grey70", hjust=.5, vjust=1),
        plot.background = element_rect(fill="#FBFBFB", color="transparent"),
        plot.margin = margin(-70, 30, -70, 30, unit="pt"))

# Save plot
ggsave("./nhl_births.jpg", width=11, height=7, units="in", dpi=600)

