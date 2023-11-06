
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggtext)
library(tidytext)
library(tidylo)



# Read data ---------------------------------------------------------------

house <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-07/house.csv")


# Tidy data ---------------------------------------------------------------

house_names <- house |> 
  filter(!writein) |> 
  select(c(year, state, party, candidate)) |> 
  mutate(party = case_when(
    str_detect(party, "DEMOCRAT") ~ "DEMOCRAT",
    str_detect(party, "REPUBLICAN") ~ "REPUBLICAN",
    str_detect(party, "INDEPENDENT") ~ "INDEPENDENT",
    .default = "OTHER"
  )) |> 
  mutate(candidate = str_to_title(candidate),
         state = str_to_title(state),
         party = str_to_title(party)) |> 
  slice(1, .by = c(candidate)) |> 
  unnest_tokens(name, candidate, to_lower = F) |> 
  filter(!name %in% c("Vote", "Other", "Blank", "Scattering", "Undervotes", "Void", "Overvotes")) |> 
  count(party, name) |> 
  bind_log_odds(party, name, n) |> 
  rename("wlo" = log_odds_weighted)



# Plot data ---------------------------------------------------------------

house_names |> 
  mutate(name = case_when(
    name %in% c("Iii", "Ii") ~ toupper(name),
    .default = name
  )) |> 
  filter(name %in% c("Jr", "III", "II")) |> 
  mutate(party = factor(party, levels=c("Democrat", "Republican", "Independent", "Other"))) |> 
  ggplot() +
  geom_col(aes(x=fct_rev(party), y=wlo, fill=wlo), 
           color="grey40",
           show.legend=F) +
  geom_hline(yintercept=0) +
  scale_fill_gradient2(low="skyblue", mid="lightblue2", high="goldenrod2") +
  labs(y="\nLog odds (weighted)\n", x="",
       caption="Data: MIT Election Data & Science Lab (2017), doi:10.7910/DVN/IG0UN2  \nVisualization: @c_borstell | Packages: {tidyverse, ggtext, tidylo, tidytext}",
       title="Junior Independents",
       subtitle='<br>Candidates named "___ II/III/Jr" in US House elections (1976-2022)<br><br>... these suffixes are overrepresented among Independents<br>') +
  coord_flip() +
  facet_wrap(~ paste0("___ ", name), ncol=3, scales="free") +
  theme_minimal(base_family = "Rockwell", base_size=15) +
  theme(strip.text = element_text(size=rel(1.2), family="Rockwell-Italic", color="grey20"),
        axis.title.x = element_text(color="grey60"),
        plot.title = element_textbox(size=rel(3), width=1, color="grey15"),
        plot.subtitle = element_textbox(size=rel(1.2), width=1, color="grey25", halign=0.5, family="Lato"),
        plot.caption = element_text(size=rel(.6), color="grey65"),
        plot.background = element_rect(fill="#FAFAFA", color=NA),
        plot.margin = margin(5, 10, 5, 5, "mm"))

# Save plot
ggsave("./us_house.jpg", width = 10, height = 6, units = "in", dpi = 600)  


# Alt-text: A bar plot with the title "Junior Independents: Candidates named "___ II/III/Jr" in US House elections (1976-2022)", showing that among Democrat, Republican, Independent and Other candidates in US House elections, the name suffixes II, III and Jr are oveerrepresented among Independents (using a weighted log odds metric). The bars are horizontal left (low) to right (high), in blue to yellow color scales, on a light grey background. Data: MIT Election Data & Science Lab (2017), doi:10.7910/DVN/IG0UN2 