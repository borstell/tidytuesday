
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(geomtextpath)
library(ggtext)




# Read data ---------------------------------------------------------------

grants <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grants.csv")
grant_opportunity_details <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grant_opportunity_details.csv")




# Custom colors and fonts -------------------------------------------------

# https://fonts.google.com/specimen/Lexend+Deca
# https://fonts.google.com/specimen/Love+Ya+Like+A+Sister

clr1 <- "skyblue4"
clr2 <- "gold"
clr3 <- "lightblue"
clr4 <- "lightblue1"
clr5 <- "skyblue2"




# Process data ------------------------------------------------------------

grant_money <- grant_opportunity_details |> 
  select(estimated_total_program_funding, posted_date, starts_with("category")) |> 
  drop_na(estimated_total_program_funding) |>
  rename("funds" = estimated_total_program_funding) |> 
  filter(funds>0) |> 
  filter(str_sub(posted_date, 1, 4)=="2023") |> 
  select(-category_explanation) |> 
  pivot_longer(cols = 3:26, names_pattern = "category_(.*)", names_to = "field") |> 
  filter(value) |> 
  select(-value) |> 
  summarize(total = sum(funds), .by = field)




# Plotting ----------------------------------------------------------------

grant_money |> 
  mutate(field = str_to_title(gsub("_", " ", field))) |> 
  mutate(field = case_when(
    field=="Iija" ~ "Infrastructure",
    .default = field
  )) |> 
  arrange(desc(total)) |> 
  mutate(rank = row_number()) |> 
  mutate(field = case_when(
    rank > 21 ~ "Other",
    .default = field
  )) |> 
  summarize(total = sum(total), .by = field) |> 
  arrange(desc(total)) |> 
  mutate(rank = row_number()) |> 
  filter(rank < 21) |> 
  mutate(rank2 = case_when(
    field == "Other" ~ 21,
    .default = rank
  )) |> 
  filter(rank < 21) |> 
  mutate(clr = case_when(
    field == "Humanities" ~ clr2,
    .default = clr3
  )) |> 
  ggplot() +
  geom_point(aes(x=fct_reorder(factor(rank), -rank2), y=total, color=clr), size=5) +
  geom_segment(aes(y=1e10, yend=1e10, x=20.5, xend=19), lty=3, linewidth=.1, color=clr3, alpha=.2) +
  geom_segment(aes(y=1e9, yend=1e9, x=20.5, xend=11), lty=3, linewidth=.1, color=clr3, alpha=.2) +
  geom_segment(aes(y=1e8, yend=1e8, x=20.5, xend=5), lty=3, linewidth=.1, color=clr3, alpha=.2) +
  geom_segment(aes(y=1e7, yend=1e7, x=20.5, xend=7.3), lty=3, linewidth=.1, color=clr3, alpha=.2) +
  geom_segment(aes(y=1e7, yend=1e7, x=6.4, xend=1), lty=3, linewidth=.1, color=clr3, alpha=.2) +
  geom_textsegment(aes(x=fct_reorder(factor(rank), -rank2), xend=fct_reorder(factor(rank), rank2), y=1e5, yend=total, label=field, color=clr), 
                   hjust=0, linewidth=1, family="LexendDeca-Medium") +
  geom_point(aes(x=fct_reorder(factor(rank), -rank2), y=total), size=3, color=clr1) +
  geom_text(aes(x=fct_reorder(factor(rank), -rank2), y=total, label=rank, color=clr), 
            size=2, family="LexendDeca-Light") +
  geom_text(aes(y=1e7, x=21.2, label="$10M"), family="LexendDeca-Light", color=clr3, alpha=.05) +
  geom_text(aes(y=1e8, x=21.2, label="$100M"), family="LexendDeca-Light", color=clr3, alpha=.05) +
  geom_text(aes(y=1e9, x=21.2, label="$1B"), family="LexendDeca-Light", color=clr3, alpha=.05) +
  geom_text(aes(y=1e10, x=21.2, label="$10B"), family="LexendDeca-Light", color=clr3, alpha=.05) +
  geom_textbox(aes(y=10**9.9, x=7, label="Humanities placed 16th among areas eligible for grant opportunities"), 
               box.color="transparent", fill="transparent", width = .35, 
               family="Love Ya Like A Sister", color=clr2, size=4) +
  geom_curve(aes(y=10**8.9, yend=10**8.3, x=7, xend=6), color=clr2, curvature = -.3, arrow = arrow(length = unit(0.03, "npc"))) +
  scale_y_log10() +
  scale_color_identity() +
  coord_flip(xlim = c(1, 21)) +
  labs(x="", y="", title="US Government Grant Opportunities in 2023", 
       subtitle="Data: grants.gov via TidyTuesday | Packages: {tidyverse,geomtextpath,ggtext} | Visualization: @c_borstell") +
  theme_void(base_family="LexendDeca-Light") +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
        plot.background = element_rect(color="transparent", fill=clr1),
        plot.title = element_textbox(color=clr4, size=rel(2.2), width = 1, family="LexendDeca-Medium", halign = .3),
        plot.subtitle = element_text(color=clr5, size=rel(.38), hjust=.55, vjust=10),
        plot.margin = margin(5, 5, 8, 0, "pt"))
# Save plot
ggsave("./us_grants.jpg", width=4.5, height=4.8, units="in", dpi=600)
