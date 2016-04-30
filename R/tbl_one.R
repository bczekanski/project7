tbl_one <- function(x){
  library(htmlTable)
  y <- select_data(x, c(3, 6, 9, 12), c(3, 6, 9, 12, 24, 36, 48, 60)) %>%
    filter(prev.ret.rank == 1| prev.ret.rank == 5| prev.ret.rank == 10) %>%
    group_by(j.ret, k.ret, prev.ret.rank, monthYear) %>%
    summarize_each(funs(mean), prev.returns, prev.vol, sz.rank, price, future.returns)
  z <- y %>%
    select(j.ret, k.ret, prev.ret.rank, future.returns) %>%
    group_by(j.ret, k.ret, prev.ret.rank) %>%
    summarize(future.returns = mean(future.returns)) %>%
    spread(key = prev.ret.rank, value = future.returns) %>%
    mutate(`0` = `10` - `1`) %>%
    #select(j.ret, k.ret, diff) %>%
    group_by(j.ret, k.ret) %>%
    gather(key = prev.ret.rank, value = future.returns, `0`, `1`, `5`, `10`) %>%
    mutate(prev.ret.rank  = as.integer(prev.ret.rank))
   y <- y %>%
    group_by(j.ret, k.ret, prev.ret.rank) %>%
    summarize_each(funs(mean), prev.returns, prev.vol, sz.rank, price, future.returns)
  w <- left_join(z,y, by = c("j.ret", "k.ret", "prev.ret.rank", "future.returns"))
  # Spread ret rank out and calc diff then gather it back

  v <- w %>%
    #unite(c(7:12)) %>%
    spread(key = k.ret, value = future.returns) %>%
    #na.omit() %>%
    #summarize_each(funs(mean), prev.returns, prev.vol, sz.rank, price, c(7:12))

 return(v)
}
