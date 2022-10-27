# TidyTuesday, 2022-10-27
# GBBO data from {bakeoff}

# Load packages
library(tidyverse)
library(tidytext)
library(bakeoff)
library(ggbump)
library(ggtext)

# Get the descriptions of baking challenges from GBBO
foods <- bakeoff::challenges %>% 
  mutate(series = paste0("series_",series)) %>% 
  select(series, signature, showstopper) %>% 
  pivot_longer(c(signature, showstopper), values_to="food") %>% 
  mutate(food = gsub(" and"," and ", food)) %>% 
  #mutate(food = gsub("with"," with ", food)) %>% 
  drop_na() %>% 
  select(-name) %>% 
  mutate(index = row_number())

# Make trigrams for "cake" and "and" contexts
cake_words <- foods %>% 
  group_by(index) %>% 
  unnest_ngrams(trigram, food, n=3) %>% 
  separate(trigram, into=c("word1", "word2", "word3"), sep=" ") %>% 
  filter(word3 == "cake" | word2 == "and") %>% 
  mutate(cat = if_else(word3=="cake", "cake", "and")) %>% 
  pivot_longer(word1:word3, values_to="word") %>% 
  unite("position", cat:name, sep="") %>% 
  filter(!position %in% c("cakeword3","andword2")) %>% 
  filter(!word %in% stop_words$word) %>% 
  ungroup() %>% 
  count(position, word, sort=T) %>% 
  mutate(position = if_else(position=="cakeword1","A",
                            if_else(position=="cakeword2","B",
                                    if_else(position=="andword1","X","Y")))) %>% 
  group_by(position) %>% 
  arrange(desc(n)) %>% 
  mutate(rank = row_number()) %>% 
  ungroup()

# Custom colors
fill_color="#fffefa"
fill_color2="#c98524"
rainbow = c("#FEB0B0", "#F5D64E", "#D0F099", "#AFDBF5","#DBCDEF","#FFCAE9")

# Custom font
# https://fonts.google.com/specimen/Pacifico
font="Pacifico Regular"

# Plot the 10 highest ranked word for each trigram context
ggplot() +
  geom_rect(aes(xmin=-1, xmax=11, ymin=0, ymax=11), fill=fill_color, color=rainbow[6], linejoin = "round", size=2) +
  geom_point(aes(x=-1, y=seq(0,11, .5), color=as.character(c(rep(5:6, 11),5))), size=5, show.legend = F) +
  geom_point(aes(x=11, y=seq(0,11, .5), color=as.character(c(rep(5:6, 11),5))), size=5, show.legend = F) +
  geom_point(aes(x=seq(-1,11, .5), y=0, color=as.character(c(rep(1:6, 4),1))), size=13, show.legend = F) +
  geom_point(aes(x=seq(11,-1, -.5), y=11, color=as.character(c(rep(1:6, 4),1))), size=13, show.legend = F) +
  scale_color_manual(values=rep(rainbow,6)) +
  geom_sigmoid(data=filter(cake_words, position=="A" & rank<11), 
               aes(x=0, xend=1.5, y=rank, yend=5, group=word, color=word),
               show.legend = F) +
  geom_label(data=filter(cake_words, position=="A" & rank<11), 
             aes(x=0, y=rank, label=str_to_title(word), color=word),
             fill=fill_color, label.size = 0, label.padding = unit(1, "mm"),
             family=font, size=5, show.legend = F) +
  geom_sigmoid(data=filter(cake_words, position=="B" & rank<11), 
               aes(x=1.5, xend=3, y=5, yend=rank, group=word, color=word), 
               show.legend = F) +
  geom_sigmoid(data=filter(cake_words, position=="B" & rank<11), 
               aes(x=3, xend=4.5, y=rank, yend=5, group=word, color=word),
               show.legend = F) +
  geom_label(data=filter(cake_words, position=="B" & rank<11), 
             aes(x=3, y=rank, label=str_to_title(word), color=word),
             fill=fill_color, label.size = 0, label.padding = unit(1, "mm"),
             family=font, size=5, show.legend = F) +
  geom_sigmoid(data=filter(cake_words, position=="X" & rank<11), 
               aes(x=5.5, xend=7, y=5, yend=rank, group=word, color=word),
               show.legend = F) +
  geom_label(aes(x=5, y=4.8, label="cake"),
             color=rainbow[1],fill=fill_color, label.size = 0, label.padding = unit(1, "mm"),
             family=font, size=10, show.legend = F) +
  geom_label(aes(x=5.5, y=5.8, label="with"),
            color=rainbow[6],fill=fill_color, label.size = 0, label.padding = unit(1, "mm"),
            family=font, size=5, show.legend = F) +
  geom_sigmoid(data=filter(cake_words, position=="X" & rank<11), 
               aes(x=7, xend=8.5, y=rank, yend=5, group=word, color=word),
               show.legend = F) +
  geom_sigmoid(data=filter(cake_words, position=="Y" & rank<11),
               aes(x=8.5, xend=10, y=5, yend=rank, group=word, color=word),
               show.legend = F) +
  geom_label(data=filter(cake_words, position=="X" & rank<11), 
             aes(x=7, y=rank, label=str_to_title(word), color=word),
            fill=fill_color, label.size = 0, label.padding = unit(1, "mm"),
            family=font, size=5, show.legend = F) +
  geom_label(aes(x=8.5, y=5, label="&"),
             color=rainbow[6],fill=fill_color, label.size = 0, label.padding = unit(1, "mm"),
             family=font, size=10, show.legend = F) +
  geom_label(data=filter(cake_words, position=="Y" & rank<11), 
             aes(x=10, y=rank, label=str_to_title(word), color=word),
             fill=fill_color, label.size = 0, label.padding = unit(1, "mm"),
             family=font, size=5, show.legend = F) +
  labs(title="Great British <span style='color:#FFCAE9;'>Cake</span> Off",
       subtitle='Cake trigrams "_ _ cake" with "_ and _" from the Great British Bake Off',
       caption="Visualization: @c_borstell\nData: {bakeoff} (via TidyTuesday)\nPackages: {bakeoff,ggbump,ggtext,tidytext,tidyverse}") +
  scale_y_reverse() +
  theme_void() +
  theme(plot.margin = margin(5,5,5,5, unit="mm"),
        plot.title = element_markdown(family=font, size=45, vjust=2, hjust=.5, color=rainbow[1]),
        plot.subtitle = element_text(family=font, size=11.5, vjust=5, hjust=.5, color=rainbow[5]),
        plot.caption = element_text(family="Futura-Medium", size=5, color=rainbow[6]),
        plot.background = element_rect(color=NA, fill="#fafae6"))

# Save plot
ggsave(width=11, height=7, units="in", dpi=600)
