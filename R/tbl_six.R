tbl_six <- function(w) {
  t <- w %>%
    #group_by(k.ret, prev.vol.rank, prev.ret.rank) %>%
    #mutate_each(funs(mean), prev.returns, prev.vol, sz.rank, price) %>%
    group_by(k.ret, prev.ret.rank, prev.vol.rank) %>%
    summarize(future.returns = mean(yearly_returns)) %>%
    #ungroup %>%
    filter(prev.ret.rank == 1| prev.ret.rank == 5| prev.ret.rank == 10)

  s <- t %>%
    #group_by(j.ret, k.ret, prev.vol.rank) %>%
    #summarize(future.returns = mean(future.returns)) %>%
    spread(key = prev.ret.rank, value = future.returns) %>%
    mutate(`0` = `10` - `1`) %>%
    #select(j.ret, k.ret, diff) %>%
    group_by(k.ret, prev.vol.rank) %>%
    gather(key = prev.ret.rank, value = future.returns, `0`, `1`, `5`, `10`) %>%
    mutate(prev.ret.rank  = as.integer(prev.ret.rank))
  f <- s %>%
    group_by(k.ret, prev.ret.rank, prev.vol.rank) %>%
    summarize(future.returns = mean(future.returns)) %>%
    spread(key = prev.vol.rank, value = future.returns) %>%
    mutate(`0` = `3` - `1`) %>%
    group_by(k.ret, prev.ret.rank) %>%
    gather(key = prev.vol.rank, value = future.returns, `0`, `1`, `2`, `3`) %>%
    #select(- c(`2`)) %>%
    spread(key = k.ret, value = future.returns) %>%
    arrange(prev.vol.rank) %>%
    tbl_df
  # v1 <- f %>%
  #   filter(prev.vol.rank == 1)
  # v2 <- f %>%
  #   filter(prev.vol.rank == 2)
  # v3 <- f %>%
  #   filter(prev.vol.rank == 3)
  #
  # e <- cbind(v1, v2)
  # g <- cbind(e, v3) %>%
  #   select (- contains ("j.ret"))

  return(f)
}
