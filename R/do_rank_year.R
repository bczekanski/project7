do_rank_year <- function(x, prev.ret.tile, prev.vol.tile, cap.tile) {
  y <- x %>%
    #na.omit() %>%
    group_by(j.ret, year) %>%
    mutate(prev.ret.rank = ntile(prev.returns, prev.ret.tile)) %>%
    group_by(j.ret, prev.ret.rank, year) %>%
    mutate(prev.vol.rank = ntile(prev.vol, prev.vol.tile)) %>%
    group_by(j.ret, prev.ret.rank, prev.vol.rank, year) %>%
    mutate(sz.rank = ntile(cap.usd, cap.tile)) %>%
    ungroup
  return(y)
}
