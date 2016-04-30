tbl_one <- function(x){
  library(htmlTable)
  y <- select_data(x, c(3, 6, 9, 12), c(3, 6, 9, 12, 24, 36, 48, 60))
  z <- y %>%
    spread(key = k.ret, value = future.returns) %>%
    na.omit() %>%
    group_by(j.ret, prev.ret.rank) %>%
    summarize_each(funs(mean), prev.returns, prev.vol, sz.rank, price, c(15:20)) %>%
    filter(prev.ret.rank == 1| prev.ret.rank == 5| prev.ret.rank == 10) %>%

 return(z)
}
