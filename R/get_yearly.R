get_year <- function(x) {

  yearly_returns1<- x %>%
    mutate(year = year(v.date)) %>%
    group_by(year, symbol) %>%
    summarize(yearly_returns = 100*(prod(tret + 1) -1))

  yearly_TV1 <- x %>%
    mutate(year = year(v.date))%>%
    mutate(outstanding_shares = cap.usd/price.unadj) %>%
    mutate(daily_turnover = volume / outstanding_shares) %>%
    group_by(year, symbol) %>%
    summarize_each(funs(mean), daily_turnover, cap.usd, price) %>%
    rename(c(daily_turnover = "yearly_TV"))

  x <- left_join(yearly_TV1, yearly_returns1, by = c("symbol", "year")) %>%
    #mutate(month = month(monthYear)) %>%
    group_by(symbol) %>%
    filter(price >= 1 , yearly_TV <= 1)  %>%
    tbl_df()
}
