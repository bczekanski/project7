tbl_four <- function(w) {
  t <- select_data(w2, c(3, 6, 9, 12), c(3, 6, 9, 12, 24, 36, 48, 60)) %>%
    group_by(j.ret, prev.vol.rank, prev.ret.rank) %>%
    summarize_each(funs(mean), prev.returns, prev.vol, sz.rank, price) %>%
    filter(prev.vol.rank == 1| prev.vol.rank == 5| prev.vol.rank == 10,
           j.ret == 6,
           prev.ret.rank == 1 | prev.ret.rank == 2 | prev.ret.rank == 3
           ) #%>%
    v1 <- t %>%
      filter(prev.vol.rank == 1)
    v2 <- t %>%
      filter(prev.vol.rank == 5)
    v3 <- t %>%
      filter(prev.vol.rank == 10)
    #spread(key = prev.ret.rank, value = prev.returns)

    z <- cbind(v1, v2)
    w <- cbind(z, v3) %>%
      select (- contains ("j.ret"))

    ## Needs to be renamd

  return(w)
}
