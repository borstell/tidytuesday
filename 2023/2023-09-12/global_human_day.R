
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggtext)



# Read data ---------------------------------------------------------------

all_countries <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/all_countries.csv")
country_regions <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/country_regions.csv")
global_human_day <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/global_human_day.csv")
global_economic_activity <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/global_economic_activity.csv")



# Data processing ---------------------------------------------------------

# Non-work type activities
life <- c("Active recreation", 
          "Hygiene & grooming",
          "Interactive",
          "Meals",
          "Passive",
          "Religious practice",
          "Sleep & bedrest",
          "Social")

# Categorize activities
activities <- tibble(Subcategory = global_human_day$Subcategory) |> 
  mutate(work_life = case_when(
    Subcategory %in% life ~ "life",
    .default = "work"
  ))

# Join activity and country data, remove sleep
no_sleep <- all_countries |> 
  left_join(country_regions, by = join_by(country_iso3)) |> 
  inner_join(activities, by = join_by(Subcategory)) |> 
  filter(Subcategory!="Sleep & bedrest") |> 
  mutate(hrs = hoursPerDayCombined)

# Subset European countries and calculate z scored balance between work and life
europe_activities <- no_sleep |> 
  drop_na(country_name) |> 
  filter(str_ends(region_name, "Europe") | country_name %in% c("Turkey", "Cyprus")) |> 
  summarize(hrs = sum(hrs), .by = c(country_name, country_iso3, region_name, work_life)) |> 
  pivot_wider(names_from = work_life, values_from = hrs) |> 
  mutate(balance = life-work) |> 
  mutate(country_name = gsub(" of Great.*|n Fed.*|Republic of ", "", country_name)) |> 
  mutate(region = gsub(" Europe", "", region_name)) |> 
  mutate(region = factor(region, levels=c("Northern", "Eastern", "Western", "Southern"))) |> 
  mutate(z_balance = (balance-mean(balance))/sd(balance)) |> 
  mutate(direction = z_balance>0)

# Custom labels and ordering for countries
euro_countries <- c("Iceland", "Norway", "Sweden", "Denmark", "Finland", "Estonia", "Latvia", 
                    "Ireland", "UK", "Netherlands", "Germany", "Poland", "Lithuania", "Russia",
                    "Belgium", "Luxembourg", "Czechia", "Austria", "Slovakia", "Hungary", "Belarus", 
                    "France", "Switzerland",  "Slovenia", "Romania", "Bulgaria", "Moldova", "Ukraine",
                    "Portugal", "Spain", "Italy", "Croatia", "Bosnia &\nHerzegovina", "Serbia", "Montenegro",  
                    "Malta", "Albania", "North\nMacedonia", "Greece", "Cyprus", "Turkey")



# Plot data ---------------------------------------------------------------

# Plot z scored data as scales
europe_activities |> 
  mutate(z_work = 0+z_balance/2,
         z_life = 0-z_balance/2,
         .by = country_name) |> 
  mutate(work_hrs = round(work),
         life_hrs = round(life)) |> 
  select(country_name, country_iso3, work_hrs, life_hrs, z_work, z_life) |> 
  mutate(country_name = gsub(" and ", " &\n", country_name)) |> 
  mutate(country_name = gsub("North ", "North\n", country_name)) |> 
  mutate(country_name = gsub("United Kingdom", "UK", country_name)) |> 
  mutate(country_name = factor(country_name, levels=euro_countries)) |> 
  
  # Plotting
  ggplot() +
  
  # Pivoted hanger for scales
  geom_point(aes(x=2, y=2)) +
  geom_segment(aes(x=2, xend=2, y=2, yend=3), linewidth=.8) +
  
  geom_segment(aes(x=1, xend=3, y=z_work+2, yend=z_life+2), linewidth=.8) +
  
  # Weights for scales
  geom_rect(aes(xmin=.8, xmax=1.2, ymin=z_work, ymax=z_work+log10(work_hrs)), fill="grey70") +
  geom_rect(aes(xmin=2.8, xmax=3.2, ymin=z_life, ymax=z_life+log10(life_hrs)), fill="skyblue") +
  
  # Dishes/cups for scales
  geom_segment(aes(x=.5, xend=1.5, y=z_work, yend=z_work), linewidth=1) +
  geom_segment(aes(x=2.5, xend=3.5, y=z_life, yend=z_life), linewidth=1) +
  
  # Chains for dishes/cups
  geom_segment(aes(x=1, xend=.5, y=z_work+2, yend=z_work), linetype=3) +
  geom_segment(aes(x=1, xend=1.5, y=z_work+2, yend=z_work), linetype=3) +
  geom_segment(aes(x=3, xend=2.5, y=z_life+2, yend=z_life), linetype=3) +
  geom_segment(aes(x=3, xend=3.5, y=z_life+2, yend=z_life), linetype=3) +
  
  # Customize
  labs(title = "<span style='color:grey70'>Work</span>~<span style='color:skyblue'>Life</span> balance in Europe",
       subtitle = "Hours of activity (z scored), excluding sleep<br>",
       caption = "Visualization:       \n@c_borstell          \n\nPackages:            \n{tidyverse,ggtext} \n\nData: The Human \nChronome Project") +
  
  facet_wrap(~country_name, ncol=7) +
  theme_void(base_family="Atkinson Hyperlegible Bold",
             base_size=20) +
  theme(plot.title = element_markdown(size=rel(2), color="grey30"),
        plot.subtitle = element_markdown(size=rel(.8), color="grey60", family="Atkinson Hyperlegible", hjust=.65),
        plot.caption = element_text(size=rel(.35), color="grey60", family="Atkinson Hyperlegible", vjust=60),
        strip.text = element_text(color="grey40"),
        plot.background = element_rect(color="transparent", fill="#FAFAFA"),
        panel.background = element_rect(color="transparent", fill="#FAFAFA"),
        plot.margin = margin(10, 10, -15, 10, unit="mm"),
        strip.clip="off")

# Save plot
ggsave("global_human_day.jpg", width=10, height=10, units="in", dpi=200)

# Alt-text
# A graphic showing "Work~Life balance in Europe: Hours of activity (z scored), excluding sleep". Each country is represented as scales, with either work or life weighing heavier.
