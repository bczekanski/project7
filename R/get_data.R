get_data <- function () {
  library(ws.data)
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

  left_join(monthly_TV1, monthly_returns1, by = c("symbol", "monthYear")) -> x
  x <- tbl_df(x)
}
