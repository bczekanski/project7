do_rank <- function(x) {
  y <- x %>%
    #na.omit() %>%
    group_by(j.ret, k.ret, monthYear) %>%
    mutate(prev.ret.rank = ntile(prev.returns, 10)) %>%
    group_by(j.ret, k.ret, prev.ret.rank, monthYear) %>%
    mutate(prev.vol.rank = ntile(prev.vol, 3)) %>%
    group_by(j.ret, k.ret, prev.ret.rank, prev.vol.rank, monthYear) %>%
    mutate(sz.rank = ntile(cap.usd, 10)) %>%
    ungroup
  return(y)
}
