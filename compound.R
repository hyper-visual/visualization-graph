library(tidyverse)
library(ggplot)
library(ggplot2)
library(dpylr)
library(stringr)
library(ggthemes)

aave_data = read.csv("./data/compound.csv")

realtime = compound_data %>% 
  mutate(datetime = as.POSIXct(timestamp, origin="1970-01-01"))

# tvl data 전처리
compound_tvl = realtime %>%
  select(datetime, tvlUSD, tvlETH) %>%
  transform(tvlETH = strtoi(str_split(tvlETH, pattern=fixed("."),n=2,simplify=TRUE)[,1])) %>%
  gather("currency", "value", 2:3)

# asset data 전처리
compound_asset = realtime %>% 
  select(datetime, BTC, ETH, DAI) %>%
  gather("ticker", "value", 2:4)

# tvl 그래프 (로그스케일, economist 테마)
ggplot(compound_tvl, aes(x=datetime, y=value, group=currency)) +
  geom_line(aes(color=currency)) +
  scale_y_log10() +
  ggtitle("Compound TVL") +
  theme_economist(dkpanel=TRUE) + 
  scale_colour_economist()

# tvl 그래프 (로그스케일, solarized 테마)
ggplot(compound_tvl, aes(x=datetime, y=value, group=currency)) +
  geom_line(aes(color=currency)) +
  scale_y_log10() +
  ggtitle("Compound TVL") +
  theme_solarized() + 
  scale_colour_solarized('blue')

# 예치된 asset 그래프 (로그스케일, economist 테마)
ggplot(compound_asset, aes(x = datetime, y=value, group=ticker)) +
  geom_line(aes(col=ticker)) +
  scale_y_log10() +
  ggtitle("Assets deposited in Compound") +
  theme_economist(dkpanel=TRUE) + 
  scale_colour_economist()

# 예치된 asset 그래프 (로그스케일, solarized 테마)
ggplot(compound_asset, aes(x = datetime, y=value, group=ticker)) +
  geom_line(aes(col=ticker)) +
  scale_y_log10() +
  ggtitle("Assets deposited in Compound") +
  theme_solarized(light=FALSE) + 
  scale_colour_solarized('blue')
