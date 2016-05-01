tbl_two <- function(w) {
  t <- select_data(w, c(3, 6, 9, 12), c(3, 6, 9, 12, 24, 36, 48, 60)) %>%
    #group_by(j.ret, prev.ret.rank) %>%
    #mutate_each(funs(mean), prev.returns, prev.vol, sz.rank, price) %>%
    group_by(j.ret, k.ret, prev.ret.rank, prev.vol.rank) %>%
    summarize(future.returns = mean(future.returns)) %>%
    filter(prev.ret.rank == 1| prev.ret.rank == 5| prev.ret.rank == 10)

  s <- t %>%
    group_by(j.ret, k.ret, prev.ret.rank, prev.vol.rank) %>%
    summarize(future.returns = mean(future.returns)) %>%
    spread(key = prev.ret.rank, value = future.returns) %>%
    mutate(`0` = `10` - `1`) %>%
    #select(j.ret, k.ret, diff) %>%
    group_by(j.ret, k.ret, prev.vol.rank) %>%
    gather(key = prev.ret.rank, value = future.returns, `0`, `1`, `5`, `10`) %>%
    mutate(prev.ret.rank  = as.integer(prev.ret.rank))
  f <- s %>%
    group_by(j.ret, k.ret, prev.ret.rank, prev.vol.rank) %>%
    summarize(future.returns = mean(future.returns)) %>%
    spread(key = prev.vol.rank, value = future.returns) %>%
    mutate(`0` = `3` - `1`) %>%
    tbl_df
  k3 <- f %>%
    filter(k.ret == 3)
  k6 <- f %>%
    filter(k.ret == 6)
  k9 <- f %>%
    filter(k.ret == 9)
  k12 <- f %>%
    filter(k.ret == 12)


  k3.6 <- left_join(k3, k6, by = c("j.ret", "prev.ret.rank"))
  k9.12 <- left_join(k9, k12, by = c("j.ret", "prev.ret.rank"))
  k.all <- left_join(k3.6, k9.12, by = c("j.ret", "prev.ret.rank"))

  all <- k.all %>%
    select (- contains ("k.ret")) %>%
    select (- contains ("j.ret"))


  return(all)
}
