do_rank <- function(x, prev.ret.tile, prev.vol.tile, cap.tile) {
  y <- x %>%
    #na.omit() %>%
    group_by(j.ret, k.ret, monthYear) %>%
    mutate(prev.ret.rank = ntile(prev.returns, prev.ret.tile)) %>%
    group_by(j.ret, k.ret, prev.ret.rank, monthYear) %>%
    mutate(prev.vol.rank = ntile(prev.vol, prev.vol.tile)) %>%
    group_by(j.ret, k.ret, prev.ret.rank, prev.vol.rank, monthYear) %>%
    mutate(sz.rank = ntile(cap.usd, cap.tile)) %>%
    ungroup
  return(y)
}
