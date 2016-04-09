getenv_default <- function(varname, default) {
    value <- Sys.getenv(varname)
    if (identical(value, '')) default else value
}

opt.defaults <- list(
    jupyter.log_level = as.integer(getenv_default('JUPYTER_LOG_LEVEL', 1L)),
    jupyter.plot_mimetypes = c(
        'text/plain',
        'image/png',
        'image/svg+xml'),
    jupyter.in_kernel = FALSE)

.onLoad <- function(libname = NULL, pkgname = NULL) {
    for (option in names(opt.defaults)) {
        if (is.null(getOption(option))) {
            do.call(options, opt.defaults[option])  # single []: preserve name
        }
    }
}
