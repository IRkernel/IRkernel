#' @importFrom utils head tail
skip_repeated <- function(vec) {
    enc <- rle(vec)
    i <- which.max(enc$lengths)
    l <- enc$lengths[[i]]
    if (l <= 3) {
        vec
    } else {
        v <- enc$values[[i]]
        e <- repr:::ellip.h
        enc$lengths <- c(head(enc$lengths, i - 1), 1, 1, 1, tail(enc$lengths, -i))
        enc$values  <- c(head(enc$values,  i - 1), v, e, v, tail(enc$values,  -i))
        inverse.rle(enc)
    }
}
