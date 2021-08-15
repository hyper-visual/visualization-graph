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

aave_user = read.csv("./data/aave_users.csv")
compound_user = read.csv("./data/compound_users.csv")
maker_dao_user = read.csv("./data/maker_dao_users.csv")

aave_user = aave_user %>%
  transform(date = ymd(str_split(date, pattern=fixed(" "),n=2,simplify=TRUE)[,1])) 

colnames(aave_user)[2]<-"aave"

compound_user = compound_user %>% 
  transform(date = ymd(str_split(date, pattern=fixed(" "),n=2,simplify=TRUE)[,1])) 

colnames(compound_user)[2]<-"compound"


maker_dao_user = maker_dao_user %>% 
  transform(date = ymd(str_split(date, pattern=fixed(" "),n=2,simplify=TRUE)[,1]))

colnames(maker_dao_user)[2]<-"makerDAO"


total_user = left_join(maker_dao_user, compound_user, by="date") %>%
  left_join(aave_user, by="date") %>%
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
total <- ggplot(total_user, aes(x = date, y=users, fill=protocol)) +
  geom_col(position="stack") +
  ggtitle("Total users") +
  theme_solarized_2() + 
  scale_fill_manual(values=c(violet,cyan,yellow)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# 세 프로토콜 유저 수 비율 그래프
total_proportion <- ggplot(total_user, aes(x = date, y=users, fill=protocol)) +
  geom_bar(position="fill", stat="identity",) +
  ggtitle("Total users proportion") +
  theme_solarized() + 
  scale_fill_manual(values=c(violet,cyan,yellow)) +
  scale_y_continuous(labels = scales::percent_format())

ggplotly(total)
ggplotly(total_proportion)