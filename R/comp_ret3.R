comp_ret3 <- function(x, y) {
library(zoo)

y <- left_join(x, y, by = c("symbol", "monthYear", "cap.usd", "monthly_TV", "price"))

# z <- y %>%
#   mutate(month = month(monthYear)) %>%
#   filter(month == 1) %>%
#   #group_by(symbol) %>%
#   #filter(monthly_returns <= 200) %>%
#   na.omit
}
