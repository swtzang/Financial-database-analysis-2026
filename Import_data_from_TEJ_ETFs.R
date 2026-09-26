# Import data from TEJ
# 0050, 0052, 0056
rm(list=ls())
library(pacman)
p_load(tidyverse, lubridate, readxl, highcharter, tidyquant, 
       timetk, tibbletime, scales)
p_load(quantmod, PerformanceAnalytics) # these two packages cannot be installed!
p_load(reshape2)
p_load(TTR)

# import data
# Make sure to convert to UTF-8
etf_raw <- read.table("/2026F_金融資料庫管理與應用/ETF50_52_56/20260922072401.txt", 
                      header = TRUE,
                      sep          = "\t",
                      fileEncoding = "UTF-16LE",
                      stringsAsFactors = FALSE)
## Standardize column names (this file has 3 cols: code, date, close)
names(etf_raw) <- c("CO_ID", "Date", "Close")

library(xts)

etf_raw$Date <- as.Date(as.character(etf_raw$Date), format = "%Y%m%d")
etf_raw <- etf_raw[!is.na(etf_raw$Date), ]
etf_raw <- etf_raw[order(etf_raw$CO_ID, etf_raw$Date), ]

str(etf_raw)
table(etf_raw$CO_ID)         # how many rows per ETF
head(etf_raw)
tail(etf_raw)
## ---- 3a. Long-format data frame is etf_raw itself; also make it a
##          convenient short ticker column (e.g. "0050") if useful

## sub() replaces whatever the whole pattern matched with the 
## replacement "\\1", which refers back to capture group 1 
## (\\1 in R's escaping is just \1 — "the first parenthesized group"). 
## Since the pattern matches the entire string (digits followed by .* which 
## consumes the rest), the entire string gets replaced by just 
## that first group — i.e., the digits get kept and everything 
## else gets thrown away.

etf_raw$Ticker <- sub("^([0-9]+).*", "\\1", etf_raw$CO_ID)

# "0050 元大台灣50" → "0050"
# "0052 富邦科技"    → "0052"
# "0056 元大高股息"  → "0056"

## ---- 3b. One xts object per ETF, in a named list ---------------------------
## split(etf_raw, etf_raw$CO_ID) breaks the single data frame etf_raw 
## apart into multiple smaller data frames, one per distinct value 
## of CO_ID — grouping rows the same way GROUP BY would in SQL.

## The result is a list, where each element is a subset of etf_raw 
## containing only the rows for one ticker, and the list is named 
## after the CO_ID values:

## etf_list$`0050 元大台灣50`   # data frame: just the 0050 rows
## etf_list$`0052 富邦科技`      # data frame: just the 0052 rows
## etf_list$`0056 元大高股息`    # data frame: just the 0056 rows

etf_list <- split(etf_raw, etf_raw$CO_ID)

etf_xts_list <- lapply(etf_list, function(d) {
  xts(d[, "Close", drop = FALSE], order.by = d$Date)
})

names(etf_xts_list) <- names(etf_list)

lapply(etf_xts_list, head)

## Import data in csv format
etf3 <- read_csv("/2026F_金融資料庫管理與應用/ETF50_52_56/20260922100133_csv.csv", col_names = TRUE)
str(etf3)
colnames(etf3) <- c('id', 'date', 'close')

# Convert from long to wide format
etf3.wide <- dcast(etf3, date~id)
str(etf3.wide)
head(etf3.wide)

# check to see if there is any NA value
apply(is.na(etf3.wide), 2, sum)

# convert into xts format
etf3.xts <- xts(etf3.wide[, -1], order.by = as.Date(as.character(etf3.wide$date), 
                                                    format = '%Y%m%d'))
head(etf3.xts)

# compute returns
etf3.ret.log <- Return.calculate(etf3.xts, method = 'log') %>% na.omit()
head(etf3.ret.log)
etf3.ret.discrete <- ROC(etf3.xts, type = "discrete") %>% na.omit()
head(etf3.ret.discrete)

# ============================================================
# Convert daily closing prices into weekly and monthly prices
## etf3.w <- to.weekly(etf3.xts, indexAt = "lastof", OHLC = FALSE) 
# this is not working
# ============================================================
# Approach 1: 
etf3.w1 <- etf3.xts %>%
  tk_tbl(preserve_index = TRUE, rename_index ="date") %>%
  # this is the the tibbletime function
  as_tbl_time(index = date) %>%
  as_period(period = "week", side ="end")

head(etf3.w1)

# Approach 2:  
# Libraries
library(tidyr)

# First value in each month
etf3$date <- as.Date(as.character(etf3$date), format = '%Y%m%d')

etf3.w2 <- etf3 %>% group_by(id) %>%
  condense_period(date, .period = "1 week") %>% 
  ungroup() %>% 
  spread(key   = id, 
         value = close) 

head(etf3.w2)
# ====================================

# ====================================
# Convert into monthly prices
etf3.m <- to.monthly(etf3.xts, indexAt = "lastof", OHLC = FALSE)
head(etf3.m)

# Compute monthly returns
# Approach 1:
etf3.ret.m1 <- Return.calculate(etf3.m, method = 'log') %>% 
  na.omit()
head(etf3.ret.m1)

# Approach 2: 
# converting daily prices to monthly returns in tidyverse
# options(digits = 4)
etf3.ret.m2 <- etf3.xts %>%
  to.monthly(indexAt = 'lastof', OHLC = FALSE) %>%
  data.frame(date = index(.)) %>%
  remove_rownames() %>%
  gather(key = asset, value = prices, -date) %>%
  group_by(asset) %>%
  mutate(returns = ROC(prices, type = "discrete") ) %>%
  select(-prices) %>%
  spread(key = asset, value = returns) %>%
  ungroup() %>% 
  na.omit()

head(etf3.ret.m2)

# Approach 3: 
etf3.ret.m3 <- etf3.xts %>%
  tk_tbl(preserve_index = TRUE, rename_index ="date") %>%
  gather(asset, prices, -date) %>%
  group_by(asset) %>%
  tq_transmute(mutate_fun = periodReturn,
               period     = "monthly",
               type       = "arithmetic", 
               leading    = FALSE) %>%
  spread(asset, monthly.returns) %>% 
  head()
#
etf3.ret.m3
#

# ==============================================
p_load(fPortfolio)
p_load(tbl2xts)

#
summary(etf3.ret.m3)

# Convert tibble to xts
etf3.ret.m3.xts <- etf3.ret.m3 %>% tk_xts(silent = TRUE) 
basicStats(etf3.ret.m3.xts)

# 
etf3.ret.m2 %>% gather(asset, returns, -date) %>% 
  ggplot(aes(x = returns, fill = asset)) +
  geom_histogram(alpha = 0.45, binwidth = .005) +
  ggtitle("Monthly Returns Since 2008")

#
etf3.ret.m2 %>% gather(asset, returns, -date) %>% 
  ggplot(aes(x = returns, fill = asset)) +
  geom_histogram(alpha = 0.45, binwidth = .01) +
  facet_wrap(~asset) +
  ggtitle("Monthly Returns Since 2008") +
  theme_update(plot.title = element_text(hjust = 0.5))
#
etf3.ret.m2 %>% gather(asset, returns, -date) %>% 
  ggplot(aes(x = returns, colour = asset)) +
  geom_density(alpha = 1) +
  ggtitle("Monthly Returns Density Since 2008") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))
#
etf3.ret.m2 %>% gather(asset, returns, -date) %>% 
  ggplot(aes(x = returns)) +
  geom_density(aes(color = asset), alpha = 1) +
  geom_histogram(aes(fill = asset), alpha = 0.45, binwidth = .01) +
  # guides(fill = "none") +
  facet_wrap(~asset) +
  ggtitle("Monthly Returns Since 2008") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme(plot.title = element_text(hjust = 0.5))

