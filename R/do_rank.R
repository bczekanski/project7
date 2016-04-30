do_rank <- function(x) {
  y <- x %>%
    #na.omit() %>%
    group_by(j.ret,monthYear) %>%
    mutate(prev.ret.rank = ntile(prev.returns, 10)) %>%
    group_by(j.ret,monthYear) %>%
    mutate(prev.vol.rank = ntile(prev.vol, 3)) %>%
    group_by(k.ret, monthYear) %>%
    mutate(sz.rank = ntile(cap.usd, 10)) %>%
    ungroup
  return(y)
}
