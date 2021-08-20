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

aave_data = read.csv("./data/transactions_aave.csv") %>%
  mutate(date = ymd(f0_)) %>%
  select(date, count)

compound_data = read.csv("./data/transactions_compound.csv") %>%
  mutate(date = ymd(f0_)) %>%
  select(date, count)

makerdao_data = read.csv("./data/transactions_makerdao.csv") %>%
  mutate(date = ymd(f0_)) %>%
  select(date, count)

ethereum_data = read.csv("./data/transactions_ethereum.csv")%>%
  mutate(date = ymd(f0_)) %>%
  select(date, count)

aave_trx <- ggplot(aave_data, aes(x=date, y=count)) +
  geom_line(colour = violet) +
  ggtitle("Aave Transactions") +
  ylab("Transaction Count") +
  xlab("Date") +
  theme_solarized_2() + 
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_colour_solarized('violet') 

compound_trx <- ggplot(compound_data, aes(x=date, y=count)) +
  geom_line(colour = cyan) +
  ggtitle("Compound Transactions") +
  ylab("Transaction Count") +
  xlab("Date") +
  theme_solarized_2() + 
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_colour_solarized('cyan') 

makerdao_trx <- ggplot(makerdao_data, aes(x=date, y=count)) +
  geom_line(colour = yellow) +
  ggtitle("MakerDAO Transactions") +
  ylab("Transaction Count") +
  xlab("Date") +
  theme_solarized_2() + 
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) +
  scale_colour_solarized('yellow') 

ethereum_trx <- ggplot(ethereum_data, aes(x=date, y=count)) +
  geom_line(colour = blue) +
  ggtitle("Ethereum Transactions") +
  ylab("Transaction Count") +
  xlab("Date") +
  theme_solarized_2() + 
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  scale_colour_solarized('blue') 

ggplotly(aave_trx)
ggplotly(compound_trx)
ggplotly(makerdao_trx)
ggplotly(ethereum_trx)