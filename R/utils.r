ellip_h <- repr:::.char_fallback('\u22EF', '...')

#' @importFrom utils head tail
skip_repeated <- function(vec) {
    if (length(vec) == 0L)
        return(vec)
    enc <- rle(vec)
    i <- which.max(enc$lengths)
    l <- enc$lengths[[i]]
    if (l <= 3) {
        vec
    } else {
        v <- enc$values[[i]]
        enc$lengths <- c(head(enc$lengths, i - 1), 1, 1,       1, tail(enc$lengths, -i))
        enc$values  <- c(head(enc$values,  i - 1), v, ellip_h, v, tail(enc$values,  -i))
        inverse.rle(enc)
    }
}
