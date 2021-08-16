library(tidyverse)
library(ggplot)
library(ggplot2)
library(dpylr)
library(stringr)
library(ggthemes)
library(htmlwidgets)
library(plotly)
library(scales)

compound_data = read.csv("./data/compound.csv")

compound_realtime = compound_data %>% 
  mutate(datetime = as.POSIXct(timestamp, origin="1970-01-01"))

# tvl data 전처리
compound_tvl = compound_realtime %>%
  select(datetime, tvlUSD, tvlETH) %>%
  transform(tvlETH = strtoi(str_split(tvlETH, pattern=fixed("."),n=2,simplify=TRUE)[,1])) %>%
  gather("currency", "value", 2:3)

# asset data 전처리
compound_asset = compound_realtime %>% 
  select(datetime, wBTC, wETH, DAI) %>%
  gather("ticker", "value", 2:4)

options(scipen=999)

# tvl 그래프 (로그스케일, solarized 테마)
tvl <- ggplot(compound_tvl, aes(x=datetime, y=value, col=currency)) +
  geom_line(aes(color=currency)) +
  ggtitle("Compound TVL") +
  scale_y_log10(breaks = 10^(0:10)) +
  theme_solarized() + 
  scale_colour_solarized('blue') 

# 예치된 asset 그래프 (로그스케일, solarized 테마)
asset <- ggplot(compound_asset, aes(x = datetime, y=value, col=ticker)) +
  geom_line(aes(col=ticker)) +
  ggtitle("Assets deposited in Compound") +
  theme_solarized() + 
  scale_colour_solarized('blue') +
  scale_y_log10(breaks = 10^(0:9)) 

ggplotly(tvl)
ggplotly(asset)
