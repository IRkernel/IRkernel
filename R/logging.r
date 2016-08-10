# The primitive jupyter logging system...
# Per default, only error messages are shown
# levels: 3 = DEBUG, 2 = INFO/MSG, 1 = ERROR

#' Kernel logging functions
#'
#' A set of exported logging utilities that have the capability to be used in upstream projects.
#' Log level and log file can be set via R package options e.g. \code{options(jupyter.log_level = 2L)}
#' or from the environment variables JUPYTER_LOG_LEVEL and JUPYTER_LOGFILE.
#'
#' @name log_*
#' @param ...  message to log
#' @export
log_debug <- function(...) {
    if (isTRUE(getOption('jupyter.log_level') >= 3L)) {
        log_msg('DEBUG', sprintf(...))
    }
}

#' @name log_*
#' @export
log_info <- function(...) {
    if (isTRUE(getOption('jupyter.log_level') >= 2L)) {
        log_msg('INFO', sprintf(...))
    }
}

#' @name log_*
#' @export
log_error <- function(...) {
    if (isTRUE(getOption('jupyter.log_level') >= 1L)) {
        log_msg('ERROR', sprintf(...))
    }
}

log_msg <- function(lvl, msg) {
    log_msg_stderror(lvl, msg)
    log_msg_logfile(lvl, msg)
}


# Handle for stderr to even log to the console when the stderr() points to a
# sink'ed connection...
.stderror <- stderr()

log_msg_stderror <- function(lvl, msg) {
    cat(sprintf('%s: %s\n', log_color(lvl), msg) , file = .stderror)
}

#' @importFrom crayon blue
#' @importFrom crayon green
#' @importFrom crayon red
log_color <- function(lvl) {
    color <- switch(lvl,
        DEBUG = green,
        INFO  = blue,
        ERROR = red,
        stop('unknown level: ', lvl))
    
    color(lvl)
}

.is_changed_logfile <- local({
    old_logfile <- ''
    function(logfile) {
        if (old_logfile != logfile) {
            old_logfile <<- logfile
            TRUE
        } else {
            FALSE
        }
    }
})

log_msg_logfile <- function(lvl, msg) {
    cur_logfile <- getOption('jupyter.logfile')
    if (!is.na(cur_logfile)) {
        if (.is_changed_logfile(cur_logfile)) {
            log_msg_stderror('INFO', sprintf('Logging to %s', cur_logfile))
        }
        log_con <- file(cur_logfile, open = 'ab')
        writeBin(charToRaw(sprintf('%s %s: %s\n', format(Sys.time()), lvl, msg)), log_con, endian = 'little')
        close(log_con)
    }
}
