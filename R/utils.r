ellip_h <- repr:::.char_fallback('\u22EF', '...')

#' @importFrom utils head tail
skip_repeated <- function(vec) {
    if (length(vec) == 0L)
        return(vec)
    
    if (is.language(vec[[1]])) {  # rle does not work on language items
        ctb <- as.character(vec)
        enc <- rle(ctb)
        enc$values <- match(enc$values, ctb)
    } else {
        enc <- rle(vec)
    }
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

fromRawJSON <- function(r) {
    s <- rawToChar(r)
    Encoding(s) <- 'UTF-8'
    fromJSON(s)
}

set_last_value <- function(obj) {
    # access via namespace so R CMD check does not complain
    .BaseNamespaceEnv$unlockBinding(".Last.value", .BaseNamespaceEnv)
    assign(".Last.value", obj, .BaseNamespaceEnv)
    lockBinding(".Last.value", .BaseNamespaceEnv)
}

get_os <- function() switch(.Platform$OS.type,
    windows = 'win',
    unix = if (identical(Sys.info()[['sysname']], 'Darwin')) 'osx' else 'unix')
