comp_ret1 <- function(y) {
  library(zoo)

  prior_returns <- function(y, months){
    y %>%
      group_by(symbol) %>%
      mutate(returns = roll_mean(lag(monthly_returns, n = 1), months, fill = NA, align = "right")) %>%
      rename(c(returns = paste0(months, "_month_prior_returns")))
  }

  for(i in c(1, 2, 3, 6, 9, 12, 15, 18, 21, 24, 30, 36, 48, 60)) {prior_returns(y, i) -> y}
  y <- y%>%
    gather(key = j.ret, value = prev.returns, 7:ncol(y))
  y$j.ret  <- extract_numeric(y$j.ret)

  prior_returns2 <- function(y, months){
    y %>%
      group_by(symbol) %>%
      mutate(mean_TV = roll_mean(lag(monthly_TV, n = 1), months, fill = NA, align = "right")) %>%
      rename(c(mean_TV = paste0(months, "_month_prior_volume")))
  }

  for(i in c(1, 2, 3, 6, 9, 12, 15, 18, 21, 24, 30, 36, 48, 60)) {prior_returns2(y, i) -> y}
  y <- y%>%
    gather(key = j.vol, value = prev.vol, 9:ncol(y))
  y$j.vol  <- extract_numeric(y$j.vol)

  z <- y %>%
    filter(j.ret == j.vol)# %>%
    #select(-"j.vol")

return(z)
}
