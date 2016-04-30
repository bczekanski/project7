compute_ret <- function(y) {
  prior_returns <- function(y, months){
    y %>%
      group_by(symbol) %>%
      mutate(returns = roll_mean(monthly_returns, months, fill = NA, align = "right")) %>%
      rename(c(returns = paste0(months, "_month_prior_returns")))
  }
  for(i in c(1:36)) {prior_returns(y, i) -> y}
  y <- y%>%
    gather(key = j.ret, value = prev.returns, 7:ncol(y))
  y$j.ret  <- extract_numeric(y$j.ret)

  prior_returns2 <- function(y, months){
    y %>%
      group_by(symbol) %>%
      mutate(mean_TV = roll_mean(monthly_TV, months, fill = NA, align = "right")) %>%
      rename(c(mean_TV = paste0(months, "_month_prior_volume")))
  }

  for(i in c(1:36)) {prior_returns2(y, i) -> y}
  y <- y%>%
    gather(key = j.vol, value = prev.vol, 9:ncol(y))
  y$j.vol  <- extract_numeric(y$j.vol)

  prior_returns3 <- function(y, months){
    y %>%
      group_by(symbol) %>%
      mutate(returns2 = roll_mean(monthly_returns, months, fill = NA, align = "left")) %>%
      rename(c(returns2 = paste0(months, "_month_future_returns")))
  }

  for(i in c(1:36)) {prior_returns3(y, i) -> y}
  y <- y %>%
    gather(key = k.ret, value = future.returns, 11:ncol(y))
  y$k.ret  <- extract_numeric(y$k.ret)

  # There needs to be some sort of lag
  # 3-1 the j=3 is from 1-1 to 3-31, while k = 3 is 3-1 to 5-30

  z <- y %>%
    mutate(month = month(monthYear)) %>%
    filter(j.ret == j.vol, month == 1)

}
