library(tidyverse)
library(ggplot)
library(ggplot2)
library(dpylr)
library(stringr)
library(ggthemes)
library(lubridate)
library(plotly)

yellow <-"#b58900"
orange <-"#cb4b16"
red <-"#dc322f"
magenta <- "#d33682"
violet <- "#6c71c4"
blue <- "#268bd2"
cyan <- "#2aa198"
green <- "#859900"

aave_tvl = read.csv("./data/aave.csv") %>% 
  mutate(datetime = as.POSIXct(timestamp, origin="1970-01-01")) %>%
  select(datetime, tvlUSD, tvlETH) %>%
  transform(tvlETH = strtoi(str_split(tvlETH, pattern=fixed("."),n=2,simplify=TRUE)[,1]))

compound_tvl = read.csv("./data/compound.csv") %>% 
  mutate(datetime = as.POSIXct(timestamp, origin="1970-01-01")) %>%
  select(datetime, tvlUSD, tvlETH) %>%
  transform(tvlETH = strtoi(str_split(tvlETH, pattern=fixed("."),n=2,simplify=TRUE)[,1])) %>%
  gather("currency", "value", 2:3)

maker_tvl = read.csv("./data/maker.csv") %>% 
  mutate(datetime = as.POSIXct(timestamp, origin="1970-01-01")) %>%
  select(datetime, tvlUSD, tvlETH) %>%
  transform(tvlETH = strtoi(str_split(tvlETH, pattern=fixed("."),n=2,simplify=TRUE)[,1])) %>%
  gather("currency", "value", 2:3)

aave_user = aave_user %>%
  transform(date = ymd(str_split(date, pattern=fixed(" "),n=2,simplify=TRUE)[,1])) 

colnames(aave_user)[2]<-"aave"

compound_user = compound_user %>% 
  transform(date = ymd(str_split(date, pattern=fixed(" "),n=2,simplify=TRUE)[,1])) 

colnames(compound_user)[2]<-"compound"


maker_dao_user = maker_dao_user %>% 
  transform(date = ymd(str_split(date, pattern=fixed(" "),n=2,simplify=TRUE)[,1]))

colnames(maker_dao_user)[2]<-"makerDAO"


total_tvl = left_join(maker_tvl, compound_tvl, by="date") %>%
  left_join(aave_tvl, by="date") %>%
  gather("protocol", "users", 2:4)


# aave 유저수 그래프
aave <- ggplot(aave_user, aes(x = date, y=users, group=1)) +
  geom_line() +
  ggtitle("Aave users") +
  theme_solarized() + 
  scale_colour_solarized('blue')

# compound 유저수 그래프
compound <- ggplot(compound_user, aes(x = date, y=users, group=1)) +
  geom_line() +
  ggtitle("Compound users") +
  theme_solarized() + 
  scale_colour_solarized('blue')

# makerDAO 유저수 그래프
maker <- ggplot(maker_dao_user, aes(x = date, y=users, group=1)) +
  geom_line() +
  ggtitle("MakerDAO users") +
  theme_solarized() + 
  scale_colour_solarized('blue')

# 세 프로토콜 유저 수 stack 그래프
total <- ggplot(total_user, aes(x = date, y=users, col=protocol)) +
  geom_line(aes(col=protocol)) +
  ggtitle("Total users") +
  theme_solarized_2() + 
  scale_color_manual(values=c(violet,cyan,yellow)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# 세 프로토콜 유저 수 비율 그래프
total_proportion <- ggplot(total_user, aes(x = date, y=users, fill=protocol)) +
  geom_bar(position="fill", stat="identity") +
  ggtitle("Total users proportion") +
  theme_solarized() + 
  scale_fill_manual(values=c(violet,cyan,yellow)) +
  scale_y_continuous(labels = scales::percent_format())

ggplotly(total)
ggplotly(total_proportion)