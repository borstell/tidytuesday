
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggbump)
library(ggtext)
library(scales)



# Fonts -------------------------------------------------------------------

# https://fonts.google.com/noto/specimen/Noto+Emoji
# https://fonts.google.com/specimen/Odibee+Sans



# Read data ---------------------------------------------------------------

gifts_gender <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/gifts_gender.csv")




# Get emoji paths ---------------------------------------------------------

emos <- c("Candy" = "https://em-content.zobj.net/source/noto-emoji/377/candy_1f36c.jpg",
          "Clothing" = "https://em-content.zobj.net/source/noto-emoji/377/socks_1f9e6.jpg",
          "EveningOut" = "https://em-content.zobj.net/source/noto-emoji/377/fork-and-knife-with-plate_1f37d-fe0f.jpg",
          "Flowers" = "https://em-content.zobj.net/source/noto-emoji/377/tulip_1f337.jpg",
          "GiftCards" = "https://em-content.zobj.net/source/noto-emoji/377/credit-card_1f4b3.jpg",
          "GreetingCards" = "https://em-content.zobj.net/source/noto-emoji/377/love-letter_1f48c.jpg",
          "Jewelry" = "https://em-content.zobj.net/source/noto-emoji/377/ring_1f48d.jpg")



# Process and plot data ---------------------------------------------------

gifts_age |> 
  select(-SpendingCelebrating) |> 
  pivot_longer(Candy:GiftCards, names_to = "Cat", values_to = "Perc") |> 
  arrange(Age, desc(Perc)) |> 
  mutate(Rank = row_number(), .by = Age) |> 
  mutate(emoji = paste0("<img src='", emos[Cat], "' width='10'/>")) |> 
  ggplot() +
  ggbump::geom_bump(aes(x=Age, y=Rank, group=Cat, color=Cat), linewidth=2) +
  geom_point(aes(x=Age, y=Rank, color=Cat), size=9) +
  geom_point(aes(x=Age, y=Rank, color=Cat), shape = 21, fill="white", size = 6.5) +
  ggtext::geom_richtext(aes(x=Age, y=Rank, label=emoji), 
                        family="Noto Emoji", fill = NA, label.color = NA, label.r = unit(10, "pt"),
                        label.padding = grid::unit(c(0, 0, 0, 0), "pt")) +
  labs(title="Top categories by amount spent for <span style='color:red'>Valentine's Day</span> by age group",
       subtitle="<br><span style='color:grey50'>Candy</span> | <span style='color:#E6AA00'>Clothing</span> | <span style='color:#56B4E9'>Evening out</span> | <span style='color:#009E73'>Flowers</span> | <span style='color:#0072B2'>Gift card</span> | <span style='color:#D55E00'>Greeting card</span> | <span style='color:#CC79A7'>Jewelry</span><br>",
       caption="Data: National Retail Federation's Valentine's Day Data Center via TidyTuesday | Packages: {tidyverse, ggbump, ggtext, scales} | Visualization: @c_borstell") +
  labs(y=NULL, x=NULL) +
  scale_y_reverse(breaks=scales::pretty_breaks()) +
  scale_color_manual(values = c("grey50", "#E6AA00", "#56B4E9", "#009E73", "#0072B2", "#D55E00","#CC79A7")) +
  theme_minimal(base_size=16, base_family="Odibee Sans") +
  theme(plot.title = ggtext::element_textbox(family = "Odibee Sans", color="grey10", hjust = 0.5),
        plot.subtitle = ggtext::element_textbox(family = "Odibee Sans", color="grey30", hjust = 0.5),
        plot.caption = element_text(family = "Odibee Sans", color="lightpink2", size=rel(.4), hjust=0.5),
        plot.background = element_rect(color="transparent", fill="#F8E8E1"),
        panel.grid = element_blank(),
        axis.text.y = element_text(margin = margin(r = -15)),
        legend.position = "none",
        plot.margin = margin(10, 10, 12, 12, unit="mm"))

# Save plot ---------------------------------------------------------------

#ggsave("./valentines.jpg", width = 6, height=5.7, units="in", dpi = 600)


