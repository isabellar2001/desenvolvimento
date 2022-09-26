library(tidyverse)
library(ggplot2)
library(readxl)

data <- read_xlsx("mpd2020.xlsx", sheet = 3)

exercise1 <- c("Brazil", "Mexico", "Republic of Korea", "China", 
               "Viet Nam", "India", "Russian Federation", "MERCOSUL")

data_countries <- data %>% filter(country %in% exercise1)

data_countries <- data_countries %>% 
  filter(year >= 1800) %>% select(year,country,gdppc) %>% 
  mutate(gdppc_log = log(gdppc))

data_countries %>% ggplot(aes(x = year, y = gdppc_log, group = country) +
  geom_line(aes(colour = country)) +
  xlab("Ano") +
  ylab("PIB per capita") +
  labs(color = "Países") + 
  ggtitle("PIB per capita entre 1800 e 2018")