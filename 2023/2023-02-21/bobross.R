library(tidyverse)
library(ggtext)

bob_ross <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv")


bob_ross %>% 
  mutate(color_hex = gsub("'", "", gsub("\\]", "", gsub("\\[", "", color_hex)))) %>% 
  separate_rows(color_hex, sep = ", ") %>% 
  select(season, painting_index, color_hex) %>% 
  count(season, color_hex) %>% 
  ggplot() +
  geom_rect(aes(xmin=-1.5+.4, xmax=33.5+.4, ymin=-.1+.02, ymax=1.1+.02), fill="grey30", color="transparent") +
  geom_rect(aes(xmin=-1.5, xmax=33.5, ymin=-.1, ymax=1.1), fill="goldenrod3", color="grey20", linewidth=.2) +
  geom_rect(aes(xmin=0.35, xmax=31.65, ymin=-0.01, ymax=1.01), fill="grey20", color="transparent") +
  geom_segment(aes(x=-1.5, xend=0.5, y=-.1, yend=0), linewidth=.1, color="grey20") +
  geom_segment(aes(x=31.5, xend=33.5, y=0, yend=-.1), linewidth=.1, color="grey20") +
  geom_segment(aes(x=-1.5, xend=0.5, y=1.1, yend=1), linewidth=.1, color="grey20") +
  geom_segment(aes(x=31.5, xend=33.5, y=1, yend=1.1), linewidth=.1, color="grey20") +
  geom_col(aes(x=season, y=n, fill=color_hex, color=color_hex), 
           position="fill", linewidth=2) + 
  geom_rect(aes(xmin=8.3, xmax=23.3, ymin=1.17, ymax=1.42), fill="grey30", color="transparent") +
  geom_rect(aes(xmin=8, xmax=23, ymin=1.15, ymax=1.4), fill="#FAFAFA", color="grey20") +
  geom_richtext(aes(x=15, y=1.28, label="Colors of Bob Ross<br><span style='font-size:8pt'>Seasons 1 to 31<span><br><span style='font-size:5pt'>Data from Jared Wilber on Bob Ross Paintings via the BobRossColors package.<br>Data visualization by @c_borstell using tidyverse and ggtext.<span>"), 
                        #label.r = unit(0, "mm"), 
                        #label.padding = unit(3, "mm"),
                        family="Futura-Medium", label.color=NA, fill=NA) +
  scale_y_reverse() +
  scale_fill_identity() +
  scale_color_identity() +
  theme_void()

ggsave(width=8, height=6.5, units="in", dpi=600, bg="#FAF9E0")
