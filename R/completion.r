completions <- function(code, cursor_pos = nchar(code), fixup = TRUE) {
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

    # guard from errors when completion is invoked in empty cells 
    if (is.null(line)) {
        line <- ''
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

    start_position <- chars_before_line + c.info$start
    in_string <- substr(code, start_position, start_position) %in% c("'", '"')

    comps <- get('.retrieveCompletions', utils_ns)()
    if (fixup && !in_string) comps <- fixup_comps(comps)
    list(
        comps = comps,
        start = start_position,
        end = start_position + nchar(c.info$token)
    )
}

fixup_comps <- function(comps) {
    # TODO: only do this if we are not in a string or so
    re_trail <- '=|::'
    re_lead <- '[\\w\\d._]+(?:\\$|@|:::?)'  # TODO: allow foo$`_bar`$baz<tab>
    
    # split off leading and trailing parts
    trailing <- gsub(sprintf('^.*?(%s)?$', re_trail), '\\1', comps, perl = TRUE)
    comps <- gsub(sprintf('(%s)$', re_trail), '', comps, perl = TRUE)
    leading <- gsub(sprintf('^((%s)*).*?$', re_lead), '\\1', comps, perl = TRUE)
    comps <- gsub(sprintf('^(%s)+', re_lead), '', comps, perl = TRUE)
    
    # wrap non-identifiers with ``
    # https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Identifiers
    comps <- gsub('^(_.*?|[.]{2,}.*?|.*?[^\\w\\d._].*?|.*?[.]\\d)$', '`\\1`', comps, perl = TRUE)
    
    # good coding style for completions
    trailing <- gsub('=', ' = ', trailing)
    comps <- paste0(leading, comps, trailing)
    gsub('^`[.][.][.]` = $', '...', comps)
}
