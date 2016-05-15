tbl_three_b <- function(w) {
  t <- select_data(w, c(3, 6, 9, 12), c(3, 6, 9, 12, 24, 36, 48, 60)) %>%
    group_by(j.ret,k.ret, prev.vol.rank, prev.ret.rank) %>%
    mutate_each(funs(mean), prev.returns, prev.vol, sz.rank, price) %>%
    group_by(j.ret, k.ret, prev.ret.rank, prev.vol.rank) %>%
    summarize(future.returns = mean(future.returns)) %>%
    #ungroup %>%
    filter(prev.ret.rank == 1| prev.ret.rank == 3| prev.ret.rank == 5)

  s <- t %>%
    #group_by(j.ret, k.ret, prev.vol.rank) %>%
    #summarize(future.returns = mean(future.returns)) %>%
    spread(key = prev.ret.rank, value = future.returns) %>%
    mutate(`0` = `5` - `1`) %>%
    #select(j.ret, k.ret, diff) %>%
    group_by(j.ret, k.ret, prev.vol.rank) %>%
    gather(key = prev.ret.rank, value = future.returns, `0`, `1`, `3`, `5`) %>%
    mutate(prev.ret.rank  = as.integer(prev.ret.rank))
  f <- s %>%
    group_by(j.ret, k.ret, prev.ret.rank, prev.vol.rank) %>%
    summarize(future.returns = mean(future.returns)) %>%
    spread(key = prev.vol.rank, value = future.returns) %>%
    mutate(`0` = `5` - `1`) %>%
    select(- c(`2`,  `4`)) %>%
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
    filter(j.ret == 6) %>%
    select (- contains ("k.ret")) %>%
    select (- contains ("j.ret"))


  return(all)
  }
