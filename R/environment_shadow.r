# Everthing related to the environment which takes functions which shadow base R functions.
# This is needed to build in our own needs, like properly shutting down the kernel
# when `quit()` is called.

add_to_user_searchpath <- function(name, FN) {
    assign(name, FN, 'jupyter:irkernel')
}

replace_in_base_namespace <- function(name, FN) {
    .BaseNamespaceEnv$unlockBinding(name, baseenv())
    assign(name, FN, baseenv())
}

get_shadowenv <- function() {
    as.environment('jupyter:irkernel')
}

# Adds functions which do not need any access to the executer into the users searchpath
#' @importFrom utils getFromNamespace
init_shadowenv <- function() {
    # add the accessors to the shadow env itself, so they are actually accessable
    # from everywhere...
    add_to_user_searchpath('.irk.get_shadowenv', get_shadowenv)
    add_to_user_searchpath('.irk.add_to_user_searchpath', add_to_user_searchpath)

    # For the rest of the functions, please explain why the workaround is needed
    # (=the problem) and link to the issue describing the problem.

    # workaround for problems with vignette(xxx) not bringing up the vignette
    # content in the browser: https://github.com/IRkernel/IRkernel/issues/267
    add_to_user_searchpath('print.vignette', function(...) {
        # utils:::print.vignette is private and R CMD check does not like us
        # using it directly, so work around the check by using getFromNamespace
        utils_print_vignette <- getFromNamespace('print.vignette', 'utils')
        utils_print_vignette(...)
        # returning immediately will run into trouble with zmq and its polling
        # preventing the vignette server to startup. So wait a little to let
        # it startup...
        # 0.1 is too little, so add some margin...
        Sys.sleep(0.5)
    })

    # we simply have currently no way to view or edit dfs:
    # https://github.com/IRkernel/IRkernel/issues/280
    add_to_user_searchpath('View', function(...) {
        stop(sQuote('View()'), ' not yet supported in the Jupyter R kernel')
    })
    add_to_user_searchpath('edit', function(...) {
        stop(sQuote('edit()'), ' not yet supported in the Jupyter R kernel')
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
