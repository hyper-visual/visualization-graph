library(tidyverse)
library(ggplot)
library(ggplot2)
library(dpylr)
library(stringr)
library(ggthemes)
library(lubridate)
library(plotly)


aave_user = read.csv("./data/aave_users.csv")
compound_user = read.csv("./data/compound_users.csv")
maker_dao_user = read.csv("./data/maker_dao_users.csv")

aave_user = aave_user %>%
  transform(date = ymd(str_split(date, pattern=fixed(" "),n=2,simplify=TRUE)[,1]))

compound_user = compound_user %>% 
  transform(date = ymd(str_split(date, pattern=fixed(" "),n=2,simplify=TRUE)[,1]))

maker_dao_user = maker_dao_user %>% 
  transform(date = ymd(str_split(date, pattern=fixed(" "),n=2,simplify=TRUE)[,1]))

total_user = data.frame(df, stringsAsFactors = TRUE) %>%
  
# 유저수 그래프
aave <- ggplot(aave_user, aes(x = date, y=users, group=1)) +
  geom_line() +
  ggtitle("Aave users") +
  theme_solarized() + 
  scale_colour_solarized('blue')

ggplotly(aave)

compound <- ggplot(compound_user, aes(x = date, y=users, group=1)) +
  geom_line() +
  ggtitle("Compound users") +
  theme_solarized() + 
  scale_colour_solarized('blue')

ggplotly(compound)

maker <- ggplot(maker_dao_user, aes(x = date, y=users, group=1)) +
  geom_line() +
  ggtitle("MakerDAO users") +
  theme_solarized() + 
  scale_colour_solarized('blue')

ggplotly(maker)