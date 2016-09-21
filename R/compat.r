UNICODE_WARNING <-
'Your code contains a unicode char which cannot be displayed in your
current locale and R will silently convert it to an escaped form when the
R kernel executes this code. This can lead to subtle errors if you use
such chars to do comparisons. For more information, please see
https://github.com/IRkernel/repr/wiki/Problems-with-unicode-on-windows'

#' @importFrom utils capture.output
warn_unicode_on_windows <- function(code, warn) {
    # Workaround to warn user when code contains potential problematic code
    # https://github.com/IRkernel/repr/issues/28#issuecomment-208810772
    # See https://github.com/hadley/evaluate/issues/66
    if (.Platform$OS.type == 'windows') {
        # strip whitespace, because trailing newlines would trip the test...
        code <- gsub('^\\s+|\\s+$', '', code)
        real_len <- nchar(code)
        r_len <- nchar(paste(capture.output(cat(code)), collapse = '\n'))
        if (real_len != r_len) warn(UNICODE_WARNING)
    }
}
