# Load packages
library(tidyverse)
library(lubridate)

# Read data
flights <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv")

# Group by country and month, count total flights and plot
flights %>% 
  unite("time", YEAR, MONTH_NUM, sep = "-") %>% 
  mutate(country = STATE_NAME) %>% 
  group_by(country, time) %>% 
  tally(FLT_TOT_1) %>% 
  ggplot(aes(x=as_datetime(ym(time)), y=n, group=country)) +
  geom_line(aes(color=n), show.legend = F) +
  #geom_vline(xintercept=as_datetime("2020-03-01")) +
  scale_color_gradient(high="red", low="lightblue") +
  scale_x_datetime(date_breaks = "6 month", date_labels = "%b '%y") +
  scale_y_continuous(labels = scales::comma_format(big.mark = " ")) +
  labs(x="",y="", title="Flights per country and month (Europe)") +
  theme_minimal(base_size=15, base_family="Futura-Medium")

# Save plot
ggsave(width=12, height=5, units="in", dpi=600)

