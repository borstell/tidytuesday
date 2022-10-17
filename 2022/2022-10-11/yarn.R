# TidyTuesday, 2022-10-11
# Ravelry yarn data

# Load packages
library(tidyverse)
library(ggbump)
library(MetBrewer)

# Read yarn data
yarn <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-11/yarn.csv")

# Find top 10 yarn companies and count number of products and rating distributions
top10_yarn <- yarn %>% 
  select(yarn_company_name, name, rating_average, yardage) %>% 
  drop_na(rating_average) %>% 
  mutate(rating = round(rating_average)) %>% 
  group_by(yarn_company_name) %>% 
  add_count(yarn_company_name, name = "n_products") %>%
  group_by(yarn_company_name, n_products) %>% 
  count(rating) %>% 
  ungroup() %>% 
  group_by(yarn_company_name) %>% 
  pivot_wider(names_from = rating, 
              values_from = n, 
              values_fill = 0) %>% 
  arrange(desc(n_products)) %>% 
  ungroup() %>% 
  mutate(quantity_rank = row_number()) %>% 
  filter(quantity_rank<11) %>% 
  select(yarn_company_name, n_products, quantity_rank, `2`:`1`) %>% 
  pivot_longer(cols = `2`:`1`, 
               names_to = "rating", 
               values_to = "n", 
               names_transform = as.double) %>% 
  group_by(yarn_company_name) %>% 
  mutate(prop_rating = n/sum(n)) %>% 
  ungroup() 

# Join with another data frame with averaged rating per product
top10_yarn <- left_join(
  top10_yarn,
    top10_yarn %>% 
      select(yarn_company_name, rating, n) %>% 
      uncount(n) %>% 
      group_by(yarn_company_name) %>% 
      summarize(rating_mean = mean(rating)) %>% 
      arrange(desc(rating_mean)) %>% 
      mutate(quality_rank = row_number()) %>% 
      ungroup()
  )
  
# Custom font
# https://fonts.google.com/specimen/Pacifico
font <- "Pacifico"

# Plot data
top10_yarn %>% 
  ggplot() +
  geom_sigmoid(aes(x=10, xend=20, y=quality_rank, yend=quantity_rank, group=yarn_company_name, color=yarn_company_name),
               show.legend = F, size=1.2) +
  geom_sigmoid(aes(x=25, xend=20+n_products/20, y=quantity_rank, yend=10-(10-quantity_rank)/2, group=yarn_company_name, color=yarn_company_name),
               show.legend = F, size=1.2) +
  geom_sigmoid(aes(x=5, xend=10-rating_mean*3-30, y=quality_rank, yend=10-(10-quality_rank)/2, group=yarn_company_name, color=yarn_company_name),
               show.legend = F, size=1.2) +
  geom_label(aes(20, quantity_rank, label=yarn_company_name, hjust=0, color=yarn_company_name),
            fill="#fffcf0", label.size = 0, label.padding = unit(0, "mm"),
            family=font, size=5, show.legend = F) +
  geom_label(aes(10, quality_rank, label=yarn_company_name, hjust=1, color=yarn_company_name),
            fill="#fffcf0", label.size = 0, label.padding = unit(0, "mm"),
            family=font, size=5, show.legend = F) +
  geom_label(aes(20+n_products/20, 10-(10-quantity_rank)/2, label=format(round(n_products),big.mark=" "), hjust=0, color=yarn_company_name),
             fill="#fffcf0", label.size = 0, label.padding = unit(1, "mm"),
            family=font, size=3, show.legend = F) +
  geom_label(aes(10-rating_mean*3-30, 10-(10-quality_rank)/2, label=round(rating_mean,2), hjust=1, color=yarn_company_name),
             fill="#fffcf0", label.size = 0, label.padding = unit(1, "mm"),
            family=font, size=3, show.legend = F) +
  annotate("text", x=-32, y=3.8, label="Mean product rating\n(1 to 5)", 
           family=font, size=5, color="coral") +
  annotate("text", x=68, y=3.8, label="Number of products\n(on Ravelry)", 
           family=font, size=5, color="coral") +
  annotate("text", x=8, y=-.5, label="Quality rank", 
           family=font, size=7, hjust=.8, color="coral") +
  annotate("text", x=22, y=-.5, label="Quantity rank", 
           family=font, size=7, hjust=.2, color="coral") +
  labs(title="Quality vs. Quantity",
       subtitle="Mean product ratings vs. number of products\nin the 10 yarn companies with the most products\n",
       caption="Visualization: @c_borstell\nData: ravelry.com (via TidyTuesday)\nPackages: {tidyverse,ggbump,MetBrewer}") +
  scale_y_reverse() +
  scale_x_continuous(expand=c(.1,.1)) +
  scale_color_met_d("Signac") +
  theme_void() + 
  theme(text=element_text(family=font),
        plot.background = element_rect(color=NA, fill="#fffcf0"),
        plot.margin = unit(c(5,5,5,5), "mm"),
        plot.title = element_text(size=75, hjust=0.5, color="coral"),
        plot.subtitle = element_text(size=16, hjust=0.5, color="orange"),
        plot.caption = element_text(size=6, hjust=.99, family="Futura-Medium",color="#f7d894"))

ggsave(width=10, height=7, units="in", dpi=600)
