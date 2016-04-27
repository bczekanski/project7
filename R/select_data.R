select_data <- function(x, j, k) {
  y <- x %>%
    filter(j.ret %in% j)
  z <- y %>%
    filter(k.ret %in% k)
  return(z)
}
