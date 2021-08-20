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

aave_loan_data = read.csv("./data/deposit-loan_aave.csv") %>%
  transform(date = ymd(str_split(date, pattern=fixed(" "),n=2,simplify=TRUE)[,1])) %>%
  gather("division", "value", 2:3)

compound_loan_data = read.csv("./data/deposit-loan_compound.csv") %>%
  transform(date = ymd(str_split(date, pattern=fixed(" "),n=2,simplify=TRUE)[,1])) %>%
  gather("division", "value", 2:3)

maker_loan_data = read.csv("./data/deposit-loan_makerdao.csv") %>%
  select(date, loan, deposit) %>%
  transform(date = ymd(str_split(date, pattern=fixed(" "),n=2,simplify=TRUE)[,1])) %>%
  gather("division", "value", 2:3)


aave_loan <- ggplot(aave_loan_data, aes(x=date, y=value, col=division)) +
  geom_line(aes(col=division)) +
  ggtitle("Aave Deposits & Loans in USD") +
  ylab("Value in USD") +
  xlab("Date") +
  theme_solarized() + 
  scale_colour_solarized('violet')  +
  scale_y_continuous(labels = label_number(suffix = " B", scale = 1e-9))

compound_loan <- ggplot(compound_loan_data, aes(x=date, y=value, col=division)) +
  geom_line(aes(col=division)) +
  ggtitle("Compound Deposits & Loans in USD") +
  ylab("Value in USD") +
  xlab("Date") +
  theme_solarized() + 
  scale_colour_solarized('cyan') +
  scale_y_continuous(labels = label_number(suffix = " B", scale = 1e-9))

maker_loan <- ggplot(maker_loan_data, aes(x=date, y=value, col=division)) +
  geom_line(aes(col=division)) +
  ggtitle("MakerDAO Deposits & Loans in USD") +
  ylab("Value in USD") +
  xlab("Date") +
  theme_solarized() + 
  scale_colour_solarized('yellow')  +
  scale_y_continuous(labels = label_number(suffix = " B", scale = 1e-9))


ggplotly(aave_loan)
ggplotly(compound_loan)
ggplotly(maker_loan)


  
  