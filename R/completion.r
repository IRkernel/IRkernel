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
    if (fixup && !in_string && length(comps)) comps <- fixup_comps(comps)
    list(
        comps = comps,
        start = start_position,
        end = start_position + nchar(c.info$token)
    )
}

fixup_comps <- function(comps) {
    # TODO: only do this if we are not in a string or so
    re_trail <- '=|::'

    # split off leading and trailing parts
    trailing <- gsub(sprintf('^.*?(%s)?$', re_trail), '\\1', comps, perl = TRUE)
    comps <- gsub(sprintf('(%s)$', re_trail), '', comps, perl = TRUE)

    # split off everything before the last special operator (a la utils:::specialOpLocs)
    # NB: use look-behind so that we can use the output directly without worrying about
    #   match.length. Separate look-behind conditions because each one must have a
    #   fixed length.
    lead_matches <- gregexpr("(?<=[$@])|(?<=[^:]::)|(?<=:::)", comps, perl = TRUE)
    last_match <- vapply(lead_matches, tail, n = 1L, integer(1L))
    has_match <- last_match > 0L
    leading <- rep("", length(comps))
    comps_with_leading <- comps[has_match]
    leading[has_match] <- substr(comps_with_leading, 1L, last_match - 1L)
    comps[has_match] <- substr(comps_with_leading, last_match, nchar(comps_with_leading))
    
    # wrap non-identifiers with ``; h/t the related r-devel thread:
    #   https://stat.ethz.ch/pipermail/r-devel/2023-March/082388.html
    non_empty <- nzchar(comps)
    comps[non_empty] <- vapply(
        comps[non_empty],
        # TODO(R>=4.0.0) use deparse1() for brevity
        function(nm) paste(deparse(as.name(nm), backtick = TRUE), collapse = " "),
        character(1L)
    )

    # good coding style for completions
    trailing <- gsub('=', ' = ', trailing, fixed = TRUE)
    comps <- paste0(leading, comps, trailing)
    comps[comps == "... = "] <- "..."
    comps
}
