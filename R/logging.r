# The primitive jupyter logging system...
# Per default, only error messages are shown
# levels: 3 = DEBUG, 2 = INFO/MSG, 1 = ERROR

log_debug <- function(...) {
    if (isTRUE(getOption('jupyter.log_level') >= 3L)) {
        log_msg('DEBUG', sprintf(...))
    }
}

log_info <- function(...) {
    if (isTRUE(getOption('jupyter.log_level') >= 2L)) {
        log_msg('INFO', sprintf(...))
    }
}

log_error <- function(...) {
    if (isTRUE(getOption('jupyter.log_level') >= 1L)) {
        log_msg('ERROR', sprintf(...))
    }
}

log_msg <- function(lvl, msg){
        log_msg_stderror(lvl, msg)
        log_msg_logfile(lvl, msg)
}


# Handle for stderr to even log to the console when the stderr() points to a
# sink'ed connection...
.stderror <- stderr()

log_msg_stderror <- function(lvl, msg){
    cat(sprintf('%s: %s\n', lvl, msg) , file = .stderror)
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