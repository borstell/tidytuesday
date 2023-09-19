
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(countrycode)
library(ggrepel)
library(scales)



# Read data ---------------------------------------------------------------

cran_20230905 <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-19/cran_20230905.csv")
package_authors <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-19/package_authors.csv")



# Pre-processing ----------------------------------------------------------

# Get email domains per maintainer
#maintainers <- cran_20230905 |> 
#  select(Maintainer) |> 
#  mutate(domain = tolower(gsub(">", "", str_extract(Maintainer, "\\....?>$")))) |> 
#  drop_na(domain)

# Get email domains per author
authors <- cran_20230905 |> 
  rename("author" = `Authors@R`) |> 
  select(author) |> 
  mutate(email = str_extract_all(author, "@.*\\....?")) |> 
  unnest(email) |>
  mutate(email = gsub('"', '', gsub("'", "", email))) |> 
  mutate(domain = str_extract(email, "\\....?$")) |> 
  drop_na(domain) |> 
  select(-author)
  
# Add country code data from {countrycode}
domains <- authors |> 
  count(domain, sort = T) |> 
  mutate(country = countrycode(domain, origin = "cctld", destination = "country.name")) |> 
  mutate(country = case_when(
    domain %in% c(".gov", ".edu") ~ "United States",
    .default = country
  ))

# Join with population data from the World Bank via {tidyr}
pop <- world_bank_pop |> 
  filter(indicator == "SP.POP.TOTL") |> 
  select(country, `2017`) |> 
  rename(population = `2017`) |> 
  mutate(iso3 = country) |> 
  mutate(country = countrycode(country, origin="iso3c", destination = "country.name")) |> 
  drop_na(country)

pop_domain <- domains |> 
  inner_join(pop, by = join_by(country))
  

pop_domain |> 
  mutate(total_n = sum(n), .by = country) |> 
  mutate(domain2 = paste0(domain, collapse = "\n"), .by = country) |> 
  slice(1, .by = country) |> 
  mutate(n = total_n) |> 
  filter(n>1) |> 
  ggplot() +
  geom_text_repel(aes(x=n, y=population, label=domain), 
                  min.segment.length = 100, family="Monaco", color="limegreen") +
  scale_x_log10(labels=comma_format()) +
  scale_y_log10(labels=comma_format()) +
  labs(x="authors", y="population",
       title="CRAN R package authors by country-affiliated email",
       subtitle="Frequency of authoR email domains compared to population of affiliated countries",
       caption='# Caption:\nvisualization ← "@c_borstell"\ndata ← "David Schoch via TidyTuesday"\npackages ← c("tidyverse", "countrycode", "ggrepel", "scales")') +
  theme_minimal(base_family="Menlo Bold") +
  theme(plot.background = element_rect(fill="grey5", color="transparent"),
        panel.background = element_rect(fill="grey5", color="transparent"),
        panel.grid = element_line(color="darkgreen", linetype=3),
        axis.text = element_text(color="darkgreen", family="Monaco", size=rel(1)),
        axis.title = element_text(color="limegreen", family="Monaco", size=rel(1)),
        plot.title = element_text(color="limegreen", family="Menlo-Bold", hjust=1, size=rel(1.7)),
        plot.subtitle = element_text(color="darkgreen", family="Menlo", hjust=1, size=rel(1)),
        plot.caption = element_text(color="darkgreen", family="Menlo", hjust=0, size=rel(.5)))

# Save plot
#ggsave("cran_authors.jpg", width=8, height=7, units="in", dpi=600)

# Alt-text: A scatterplot in neon green colors on a dark background, resembling a computer console. Title says "CRAN R package authors by country-affiliated email: Frequency of authoR email domains compared to population of affiliated countries". Shows the number of R package authors by country-affiliated email addresses (x axis) to the total population of the affiliated country (y axis). US (.edu/.gov/.us) at the top right corner.