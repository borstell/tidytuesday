
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(scales)
library(ggchicklet)
library(ggtext)



# Read data ---------------------------------------------------------------

spam <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-15/spam.csv")



# Plot data ---------------------------------------------------------------

# Spam can-style yellow
spam_yellow <- "yellow"

# Plot mean % of "$" and "!" by spam vs non-spam
spam %>% 
  
  # Get % of emails that contain $ and ! per category
  mutate(has_dollar = if_else(dollar>0, 1, 0)) %>% 
  mutate(has_bang = if_else(bang>0, 1, 0)) %>% 
  summarize(dollars = sum(has_dollar)/n(),
         bangs = sum(has_bang)/n(),
         .by = yesno) %>% 
  
  # Rename categories
  mutate(spam = case_when(
    yesno == "y" ~ "spam",
    yesno == "n" ~ "not spam",
    .default = NA_character_
  )) %>% 
  
  # Make long format
  pivot_longer(cols = c(dollars, bangs), 
               names_to = "sign", 
               values_to = "percent") %>% 
  
  # Relabel to actual characters
  mutate(sign = case_when(
    sign == "bangs" ~ "!",
    sign == "dollars" ~ "$",
    .default = NA_character_
  )) %>% 
  
  # Plot data
  ggplot() +
  
  # Make spam-looking columns
  geom_chicklet(aes(x=sign, y=percent), 
           position = "dodge", 
           radius = unit(3, "mm"),
           show.legend = F, 
           color="white",
           fill="lightpink2") +
  
  # Plot % labels
  geom_text(aes(x=sign, y=percent+.05, label=paste0(round(percent*100),"%")),
            color=spam_yellow,
            family="Corben-Bold",
            size=4) +
  
  # Customize plot
  scale_y_continuous(labels=percent_format()) +
  labs(x="", 
       y="", 
       title="$PAM!", 
       subtitle="~500% more likely to contain $ <span style='color:black;'>than non-spam </span><br>~200% more likely to contain ! <span style='color:black;'> than non-spam </span>",
       caption="Visualization: @c_borstell | Data: Rdatasets via TidyTuesday | Packages: {tidyverse,ggchicklet,ggtext,scales}") +
  facet_wrap(~toupper(spam)) +
  theme_minimal(base_size=20, base_family="Caprasimo") +
  theme(strip.text = element_text(size=25, 
                                  family="Caprasimo", 
                                  color=spam_yellow),
        axis.text.x = element_text(size=rel(2), 
                                   color=spam_yellow,
                                   family="Copperplate"),
        axis.text.y = element_blank(),
        plot.title = element_text(size=rel(5.5), 
                                  family="Caprasimo", 
                                  color=spam_yellow,
                                  hjust=.5),
        plot.subtitle = element_textbox(family="Copperplate", 
                                        fill=spam_yellow, 
                                        color="red3",
                                        padding=margin(5, 5, 5, 5, unit = "pt"),
                                        hjust=.5,
                                        size=rel(.8)),
        plot.caption = element_text(size=rel(.25),
                                    family="Copperplate",
                                    color=spam_yellow,
                                    hjust=.5,
                                    vjust=670),
        plot.background = element_rect(fill="#12226B"),
        plot.margin = margin(30, 15, -15, -15, unit = "pt"),
        panel.grid = element_blank())

# Save plot
ggsave(width = 5.5, height = 7, units = "in", dpi = 600)

#Alt-text: Graphic in the style of a SPAM can, blue background and yellow text, showing the percentage of emails containing exclamation points "!" and dollar signs "$" in spam vs non-spam emails. The columns (showing the higher values for spam emails) are a light pink shade, resembling slices of spam.
