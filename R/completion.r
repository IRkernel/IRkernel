completions <- function(code, cursor_pos = nchar(code)) {
    # Find which line we're on and position within that line
    lines <- strsplit(code, '\n', fixed = TRUE)[[1]]
    chars_before_line <- 0L
    for (line in lines) {
        new_cursor_pos <- cursor_pos - nchar(line) - 1L # -1 for the newline
        if (new_cursor_pos < 0L) {
            break
        }
        cursor_pos <- new_cursor_pos
        chars_before_line <- chars_before_line + nchar(line) + 1L
    }
    
    # the completion docs say:
    # > they are unexported because they are not meant to be called directly by users
    # And we are no users, so we just have to trick the overeager R CMD check by not using :::
    utils_ns <- asNamespace('utils')
    
    get('.assignLinebuffer', utils_ns)(line)
    get('.assignEnd', utils_ns)(cursor_pos)
    # .guessTokenFromLine, like most other functions here usually sets variables in .CompletionEnv.
    # When specifying update = FALSE, it instead returns a list(token = ..., start = ...)
    c.info <- get('.guessTokenFromLine', utils_ns)(update = FALSE)
    get('.guessTokenFromLine', utils_ns)()
    get('.completeToken', utils_ns)()

    comps <- get('.retrieveCompletions', utils_ns)()
    
    # https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Identifiers
    # TODO: only do this if we are not in a string or so
    comps <- gsub('^([\\w\\d._]+(?:\\$|::))?(?!.*::)([_.].*?|.*?[^\\w\\d._].*?|.*?[.]\\d)(=)?(?<!::)$', '\\1`\\2`\\3', comps, perl = TRUE)
    
    # good coding style for completions
    comps <- gsub('`[.][.][.]`=$', '...', comps)
    comps <- gsub('=$', ' = ', comps)
    
    start_position <- chars_before_line + c.info$start
    list(
        comps = comps,
        start = start_position,
        end = start_position
    )
}
