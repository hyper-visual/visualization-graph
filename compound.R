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
  select(datetime, USD, ETH) %>%
  transform(ETH = strtoi(str_split(ETH, pattern=fixed("."),n=2,simplify=TRUE)[,1])) %>%
  gather("currency", "value", 2:3)

# asset data 전처리
compound_asset = compound_realtime %>% 
  select(datetime, wBTC, wETH, DAI) %>%
  gather("ticker", "value", 2:4)

options(scipen=999)

# tvl 그래프 (로그스케일, solarized 테마)
ggplot(compound_tvl, aes(x=datetime, y=value, col=currency)) +
  geom_line(aes(color=currency)) +
  ggtitle("Compound TVL") +
#  ylab("Value") +
  ylab("Value in log scale") +
  xlab("Date") +
  scale_y_log10(breaks = 10^(0:11), labels = trans_format("log10", math_format(10^.x))) +
#  scale_y_continuous(labels = label_number(suffix = " B", scale = 1e-9)) +
  theme_solarized_2() + 
  scale_colour_solarized('blue') 

# 예치된 asset 그래프 (로그스케일, solarized 테마)
ggplot(compound_asset, aes(x = datetime, y=value, col=ticker)) +
  geom_line(aes(col=ticker)) +
  ggtitle("Assets deposited in Compound") +
  ylab("Value in log scale") +
#  ylab("Value") +
  xlab("Date") +
  theme_solarized_2() + 
  scale_color_manual(values=c(cyan,yellow,blue)) +
#  scale_y_continuous(labels = label_number(suffix = " B", scale = 1e-9))
  scale_y_log10(breaks = 10^(-9:9), labels = trans_format("log10", math_format(10^.x))) 

#ggplotly(tvl)
#ggplotly(asset)
