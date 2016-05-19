get_monthly <- function(x) {

  monthly_returns1<- x %>%
    mutate(monthYear = as.Date(as.yearmon(v.date))) %>%
    group_by(monthYear, symbol) %>%
    summarize(monthly_returns = 100*(prod(tret + 1) -1))

  monthly_TV1 <- x %>%
    mutate(monthYear = as.Date(as.yearmon(v.date)))%>%
    mutate(outstanding_shares = cap.usd/price.unadj) %>%
    mutate(daily_turnover = volume / outstanding_shares) %>%
    group_by(monthYear, symbol) %>%
    summarize_each(funs(mean), daily_turnover, cap.usd, price) %>%
    rename(c(daily_turnover = "monthly_TV"))

  x <- left_join(monthly_TV1, monthly_returns1, by = c("symbol", "monthYear")) %>%
    #mutate(month = month(monthYear)) %>%
    group_by(symbol) %>%
    filter(price >= 1 , monthly_TV <= .5)  %>%
    tbl_df()

  }
