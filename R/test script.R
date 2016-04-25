"library(knitr)
library(ws.data)
library(dplyr)
library(lubridate)
library(zoo)
library(RcppRoll)
library(ggplot2)
library(reshape)
#Dont want to use reshape long term
library(tidyr)



#get_data <- function () {
data(secref)
data(yearly)
data(daily.1998)
data(daily.1999)
data(daily.2000)
data(daily.2001)
data(daily.2002)
data(daily.2003)
data(daily.2004)
data(daily.2005)
data(daily.2006)
data(daily.2007)

all_daily <-
  rbind(
    daily.1998,
    daily.1999,
    daily.2000,
    daily.2000,
    daily.2001,
    daily.2002,
    daily.2003,
    daily.2004,
    daily.2005,
    daily.2006,
    daily.2007) %>%
  mutate(year = as.numeric(format(v.date,'%Y')))

daily_yearly <- left_join(all_daily, yearly, by = c("id", "year", "symbol"))

all_ws.data <- left_join(daily_yearly, secref, by = c("id", "symbol"))

cleaned_data <- all_ws.data %>%
  filter(m.ind != "REITS")
# Need to filter out the stocks with mins less than $1
# in order to be fair, must do it w knowledge at the time
# put filter in later

monthly_returns1<- cleaned_data %>%
  mutate(monthYear = as.Date(as.yearmon(v.date))) %>%
  group_by(monthYear, symbol) %>%
  summarize(monthly_returns = 100 *(prod(1 + tret) -1))

monthly_TV1 <- cleaned_data %>%
  mutate(monthYear = as.Date(as.yearmon(v.date))) %>%
  mutate(outstanding_shares = cap.usd/price.unadj) %>%
  mutate(daily_turnover = volume / outstanding_shares) %>%
  group_by(monthYear, symbol) %>%
  summarize_each(funs(mean), daily_turnover, cap.usd) %>%
  rename(c(daily_turnover = "monthly_TV"))

##revise the outstanding shares

left_join(monthly_TV1, monthly_returns1, by = c("symbol", "monthYear")) -> x
x <- tbl_df(x)
#}

y <- x %>%
  filter(symbol == "AAPL"| symbol =="MSFT"| symbol =="GOOG")
### UNdo this

prior_returns <- function(y, months){
  y %>%
    group_by(symbol) %>%
    mutate(returns = roll_mean(monthly_returns, months, fill = NA, align = "right")) %>%
    rename(c(returns = paste0(months, "_month_prior_returns")))
}
for(i in c(1:36)) {prior_returns(y, i) -> y}
y <- y%>%
  gather(key = j.ret, value = prev.returns, 6:ncol(y))
y$j.ret  <- extract_numeric(y$j.ret)

prior_returns2 <- function(y, months){
  y %>%
    group_by(symbol, j.ret) %>%
    mutate(mean_TV = roll_mean(monthly_TV, months, fill = NA, align = "right")) %>%
    rename(c(mean_TV = paste0(months, "_month_prior_volume")))
}

for(i in c(1:36)) {prior_returns2(y, i) -> y}
y <- y%>%
  gather(key = j.vol, value = prev.vol, 8:ncol(y))
y$j.vol  <- extract_numeric(y$j.vol)

prior_returns3 <- function(y, months){
  y %>%
    group_by(symbol, j.ret, j.vol) %>%
    mutate(returns2 = roll_mean(monthly_returns, months, fill = NA, align = "left")) %>%
    rename(c(returns2 = paste0(months, "_month_future_returns")))
}

for(i in c(1:36)) {prior_returns3(y, i) -> y}
y <- y %>%
  gather(key = k.ret, value = future.returns, 10:ncol(y))
y$k.ret  <- extract_numeric(y$k.ret)

# There needs to be some sort of lag
# 3-1 the j=3 is from 1-1 to 3-31, while k = 3 is 3-1 to 5-30

z <- y %>%
  filter(j.ret == j.vol) %>%
  group_by(j.ret, j.vol, k.ret) %>%
  mutate(prev.ret.rank = ntile(prev.returns, 10)) %>%
  group_by(j.ret, j.vol, k.ret) %>%
  mutate(prev.vol.rank = ntile(prev.vol, 3)) %>%
  group_by(j.ret, j.vol, k.ret) %>%
  mutate(sz.rank = ntile(cap.usd, 10))

w <- z %>%
  group_by(j.ret, k.ret, prev.ret.rank, prev.vol.rank) %>%
  summarize_each(funs(mean), prev.returns, prev.vol, future.returns, cap.usd, sz.rank) %>%
  filter(prev.ret.rank == 1| prev.ret.rank == 5| prev.ret.rank == 10)"

