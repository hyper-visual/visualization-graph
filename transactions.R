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

aave_trx <- ggplot(aave_data, aes(x=date, y=transaction_count)) +
  geom_line() +
  ggtitle("Aave Transactions") +
  scale_y_log10(breaks = 10^(0:10)) +
  theme_solarized() + 
  scale_colour_manual(values=c(violet))

ggplotly(aave_trx)