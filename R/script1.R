
make_table1 <- function(x) {
  x %>%
    na.omit() %>%
    group_by(j.ret, k.ret)
    #summarize_each(funs(mean), prev.returns, prev.vol, future.returns) %>%
    #filter(prev.ret.rank == 1 | prev.ret.rank == 5| prev.ret.rank == 10)
}

make_table2 <- function(x) {
  x %>%
    na.omit() %>%
    group_by(j.ret, j.vol, k.ret) %>%
    mutate(prev.ret.rank = ntile(prev.returns, 10)) %>%
    mutate(prev.vol.rank = ntile(prev.vol, 3)) %>%
    group_by(j.ret, j.vol, k.ret, prev.ret.rank, prev.vol.rank) %>%
    summarize_each(funs(mean), prev.returns, prev.vol, future.returns) %>%
    filter(prev.ret.rank == 1| prev.ret.rank == 5| prev.ret.rank == 10)
}

make_table3 <- function(x) {
  x %>%
    na.omit() %>%
    group_by(j.ret, j.vol, k.ret) %>%
    mutate(prev.ret.rank = ntile(prev.returns, 10)) %>%
    mutate(prev.vol.rank = ntile(prev.vol, 3)) %>%
    group_by(j.ret, j.vol, k.ret, prev.vol.rank, prev.ret.rank) %>%
    #summarize_each(funs(mean), prev.returns, prev.vol) %>%
    filter(prev.ret.rank == 1| prev.ret.rank == 10) %>%
    spread(key = prev.ret.rank, value = future.returns)

}

make_table6 <- function(x) {
  x %>%
    na.omit() %>%
    select(-k.ret, -future.returns) %>%
    group_by(j.ret, j.vol ) %>%
    mutate(prev.ret.rank = ntile(prev.returns, 10)) %>%
    mutate(size.rank = ntile(cap.usd, 10)) %>%
    mutate(prev.vol.rank = ntile(prev.vol, 3)) %>%
    group_by(j.ret, j.vol, prev.ret.rank, prev.vol.rank) %>%
    summarize_each(funs(mean), prev.returns, prev.vol, size.rank, cap.usd) %>%
    filter(prev.ret.rank == 1| prev.ret.rank == 5| prev.ret.rank == 10)
}



