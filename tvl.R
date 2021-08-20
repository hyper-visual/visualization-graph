library(tidyverse)
library(ggplot)
library(ggplot2)
library(dpylr)
library(stringr)
library(ggthemes)
library(lubridate)
library(plotly)
library(scales)

yellow <-"#b58900"
orange <-"#cb4b16"
red <-"#dc322f"
magenta <- "#d33682"
violet <- "#6c71c4"
blue <- "#268bd2"
cyan <- "#2aa198"
green <- "#859900"

aave_data = read.csv("./data/aave.csv") %>% 
  mutate(date = as.POSIXct(timestamp, origin="1970-01-01")) %>%
  select(date, tvlUSD, tvlETH) %>%
  transform(tvlETH = strtoi(str_split(tvlETH, pattern=fixed("."),n=2,simplify=TRUE)[,1]))

aave_tvl_usd = aave_data %>%
  select(date, tvlUSD)
colnames(aave_tvl_usd)[2]<-"Aave"

aave_tvl_eth = aave_data %>%
  select(date, tvlETH)
colnames(aave_tvl_eth)[2]<-"Aave"

compound_data = read.csv("./data/compound.csv") %>% 
  mutate(date = as.POSIXct(timestamp, origin="1970-01-01")) %>%
  select(date, tvlUSD, tvlETH) %>%
  transform(tvlETH = strtoi(str_split(tvlETH, pattern=fixed("."),n=2,simplify=TRUE)[,1]))

compound_tvl_usd = compound_data %>%
  select(date, tvlUSD)
colnames(compound_tvl_usd)[2]<-"Compound"

compound_tvl_eth = compound_data %>%
  select(date, tvlETH)
colnames(compound_tvl_eth)[2]<-"Compound"

maker_data = read.csv("./data/maker.csv") %>% 
  mutate(date = as.POSIXct(timestamp, origin="1970-01-01")) %>%
  select(date, tvlUSD, tvlETH) %>%
  transform(tvlETH = strtoi(str_split(tvlETH, pattern=fixed("."),n=2,simplify=TRUE)[,1]))

maker_tvl_usd = maker_data %>%
  select(date, tvlUSD)
colnames(maker_tvl_usd)[2]<-"MakerDAO"

maker_tvl_eth = maker_data %>%
  select(date, tvlETH)
colnames(maker_tvl_eth)[2]<-"MakerDAO"

total_tvl_usd = left_join(maker_tvl_usd, compound_tvl_usd, by="date") %>%
  left_join(aave_tvl_usd, by="date") %>%
  gather("protocol", "tvl", 2:4)

total_tvl_eth = left_join(maker_tvl_eth, compound_tvl_eth, by="date") %>%
  left_join(aave_tvl_eth, by="date") %>%
  gather("protocol", "tvl", 2:4)


# usd 기준 tvl 그래프
usd <- ggplot(total_tvl_usd, aes(x=date, y=tvl, col=protocol)) +
  geom_line(aes(color=protocol)) +
  ggtitle("Total Value Locked (USD)") +
  ylab("Total Value Locked in USD") +
  xlab("Date") +
  scale_y_continuous(labels = label_number(suffix = " B", scale = 1e-9)) +
  theme_solarized_2() + 
  scale_color_manual(values=c(violet,cyan,yellow))
  
# eth 기준 tvl 그래프
eth <- ggplot(total_tvl_eth, aes(x=date, y=tvl, col=protocol)) +
  geom_line(aes(color=protocol)) +
  ggtitle("Total Value Locked (ETH)") +
  ylab("Total Value Locked in ETH") +
  xlab("Date") +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  theme_solarized_2() + 
  scale_color_manual(values=c(violet,cyan,yellow))
  
ggplotly(usd)
ggplotly(eth)