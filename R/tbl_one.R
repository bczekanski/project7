tbl_one <- function(w){

  v <- select_data(w, c(3, 6, 9, 12), c(3, 6, 9, 12, 24, 36, 48, 60)) %>%
    group_by(j.ret, prev.ret.rank) %>%
    mutate_each(funs(mean), prev.returns, prev.vol, sz.rank, price) %>%
    group_by(j.ret, k.ret, prev.ret.rank) %>%
    summarize_each(funs(mean), prev.returns, prev.vol, sz.rank, price, future.returns) %>%
    filter(prev.ret.rank == 1| prev.ret.rank == 5| prev.ret.rank == 10)

  s <- v %>%
    select(j.ret, k.ret, prev.ret.rank, future.returns) %>%
    group_by(j.ret, k.ret, prev.ret.rank) %>%
    summarize(future.returns = mean(future.returns)) %>%
    spread(key = prev.ret.rank, value = future.returns) %>%
    mutate(`11` = `10` - `1`) %>%
    #select(j.ret, k.ret, diff) %>%
    group_by(j.ret, k.ret) %>%
    gather(key = prev.ret.rank, value = future.returns, `11`, `1`, `5`, `10`) %>%
    mutate(prev.ret.rank  = as.integer(prev.ret.rank))

  d <- v %>%
    group_by(j.ret, k.ret, prev.ret.rank) %>%
    summarize_each(funs(mean), prev.returns, prev.vol, sz.rank, price, future.returns)

  r <- left_join(s,d, by = c("j.ret", "k.ret", "prev.ret.rank", "future.returns"))
  # Spread ret rank out and calc diff then gather it back

  t <- r %>%
    #unite(c(7:12)) %>%
    spread(key = k.ret, value = future.returns) %>%
    #na.pass() %>%
    group_by(j.ret, prev.ret.rank) %>%
    summarize_each(funs(mean), prev.returns, prev.vol, sz.rank, price, c(7:12))
 return(t)
}
