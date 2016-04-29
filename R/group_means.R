group_means <- function(x) {
  y <- x %>%
    group_by(prev.vol.rank) %>%
    spread(key = k.ret, value = future.returns)
    group_by(j.ret, k.ret, prev.ret.rank, prev.vol.rank) %>%
    summarize_each(funs(mean), prev.returns, prev.vol, future.returns, cap.usd, sz.rank) %>%
    filter(prev.ret.rank == 1| prev.ret.rank == 5| prev.ret.rank == 10)
  return(y)
}
