comp_ret2 <- function(y) {
library(zoo)
prior_returns3 <- function(y, months){
  y %>%
    group_by(symbol) %>%
    arrange(monthYear) %>%
    mutate(returns2 = roll_mean(monthly_returns,  months, fill = NA, align = "left")) %>%
    rename(c(returns2 = paste0(months, "_month_future_returns")))
}

for(i in c(1:12)) {prior_returns3(y, i) -> y}
y <- y %>%
  gather(key = k.ret, value = future.returns, 7:ncol(y))
y$k.ret  <- extract_numeric(y$k.ret)

prior_returns4 <- function(y, months){
  y %>%
    group_by(symbol) %>%
    arrange(monthYear) %>%
    mutate(vol2 = roll_mean(monthly_TV,  months, fill = NA, align = "left")) %>%
    rename(c(vol2 = paste0(months, "_month_future_volume")))
}

for(i in c(1:12)) {prior_returns4(y, i) -> y}
y <- y %>%
  gather(key = k.vol, value = future.vol, 9:ncol(y))
y$k.vol  <- extract_numeric(y$k.vol)

z <- y %>%
  filter(k.ret == k.vol) #%>%
  #select(-"k.vol")
return(z)
}
