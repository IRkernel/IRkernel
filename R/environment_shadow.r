# Everthing related to the environment which takes functions which shadow base R functions.
# This is needed to build in our own needs, like properly shutting down the kernel
# when `quit()` is called.

add_to_user_searchpath <- function(name, FN, pkg = NULL) {
    pkg_avail <- !is.null(pkg) && requireNamespace(pkg, quietly = TRUE)
    if (pkg_avail) {
        replace_in_package(pkg, name, FN)
    } else {
        assign(name, FN, 'jupyter:irkernel')
    }
}

replace_in_package <- function(pkg, name, FN) {
    env_name <- paste0('package:', pkg)
    if (env_name %in% search())
        replace_in_env(name, FN, as.environment(env_name))
    replace_in_env(name, FN, getNamespace(pkg))
}

replace_in_env <- function(name, FN, env) {
    .BaseNamespaceEnv$unlockBinding(name, env)
    assign(name, FN, env)
    .BaseNamespaceEnv$lockBinding(name, env)
}

get_shadowenv <- function() {
    as.environment('jupyter:irkernel')
}

# save functions that are later replaced (called in .onLoad)
backup_env <- new.env()
# Circumvent windows build bug, see issue #530
backup_env$utils_flush_console <- function(...) {}
# Circumvent devtools bug
backup_env$base_flush_connection <- function(...) {}

init_backup_env <- function() {
    if (!identical(environment(utils::flush.console), environment(utils::read.delim))) {
        tb <- .traceback(2)
        warning(
            'init_backup_env called a second time after init_shadowenv:\n',
            paste(capture.output(traceback(tb)), collapse = '\n')
        )
        return()
    }
    backup_env$base_flush_connection <- base::flush.connection
    backup_env$utils_flush_console <- utils::flush.console
    backup_env$base_quit <- base::quit
}

# Adds functions which do not need any access to the executer into the users searchpath
#' @importFrom utils getFromNamespace getS3method
#' @importFrom evaluate flush_console
init_shadowenv <- function() {
    # add the accessors to the shadow env itself, so they are actually accessable
    # from everywhere...
    add_to_user_searchpath('.irk.get_shadowenv', get_shadowenv)
    add_to_user_searchpath('.irk.add_to_user_searchpath', add_to_user_searchpath)

    # For the rest of the functions, please explain why the workaround is needed
    # (=the problem) and link to the issue describing the problem.

    # workaround for problems with vignette(xxx) not bringing up the vignette
    # content in the browser: https://github.com/IRkernel/IRkernel/issues/267
    add_to_user_searchpath('print.vignette', function(x, ...) {
        # R CMD check does not like us using :::
        getS3method('print', 'vignette')(x, ...)
        # returning immediately will run into trouble with zmq and its polling
        # preventing the vignette server to startup. So wait a little to let
        # it startup...
        # 0.1 is too little, so add some margin...
        Sys.sleep(0.5)
    })

    add_to_user_searchpath('View', function(x, title) {
        if (!missing(title)) IRdisplay::display_text(title)
        IRdisplay::display(x)
        invisible(x)  # the manpage says it returns NULL, but this is useful for piping
    })
    # we simply have currently no way to edit dfs:
    # https://github.com/IRkernel/IRkernel/issues/280
    add_to_user_searchpath('edit', function(...) {
        stop(sQuote('edit()'), ' not yet supported in the Jupyter R kernel')
    })
    
    # stream output in loops:
    # https://github.com/IRkernel/IRkernel/issues/3
    replace_in_package('base', 'flush.connection', function(con) {
        backup_env$base_flush_connection(con)
        flush_console()
    })
    replace_in_package('utils', 'flush.console', function() {
        backup_env$utils_flush_console()
        flush_console()
    })
}


init_cran_repo <- function() {
    r <- getOption('repos')
    is_unuseable_mirror <- identical(r, c(CRAN = '@CRAN@'))
    if (is_unuseable_mirror) {
        # the default repo according to https://cran.R-project.org/mirrors.html
        # uses geo-redirects
        r[['CRAN']] <- 'https://cran.r-project.org'
        # attribute indicating the repos was set by us...
        attr(r, 'irkernel') <- TRUE
        options(repos = r)
    }
}

init_session <- function() {
    init_cran_repo()
    # We support color even if isatty(stdout()) is FALSE
    options(crayon.enabled = TRUE)
}


#' @importFrom grDevices pdf png
init_null_device <- function() {
    # if possible, use a device that
    # 1. prints no warnings for unicode (unlike pdf/postscript)
    # 2. can handle /dev/null (unlike OSX devices)
    # since there is nothing like that on OSX AFAIK, use pdf there (accepting warnings).
    
    os <- get_os()
    
    ok_device     <- switch(os, win = png,   osx = pdf,  unix = png)
    null_filename <- switch(os, win = 'NUL', osx = NULL, unix = '/dev/null')
    
    null_device <- function(filename = null_filename, ...) ok_device(filename, ...)
    
    if (identical(getOption('device'), pdf)) {
        options(device = null_device)
    }
}
