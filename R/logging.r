# The primitive jupyter logging system...
# Per default, only error messages are shown
# levels: 3 = DEBUG, 2 = INFO/MSG, 1 = ERROR

# Handle for stderr to even log to the console when the stderr() points to a
# sink'ed connection...
stderror <- stderr()

log_debug <- function(...) {
    if (getOption('jupyter.log_level') >= 3L) {
        cat('DEBUG: ', sprintf(...), '\n', sep = '', file = stderror)
    }
}

log_info <- function(...) {
    if (getOption('jupyter.log_level') >= 2L) {
        cat('INFO: ', sprintf(...), '\n', sep = '', file = stderror)
    }
}

log_error <- function(...) {
    if (getOption('jupyter.log_level') >= 1L) {
        cat('INFO: ', sprintf(...), '\n', sep = '', file = stderror)
    }
}
