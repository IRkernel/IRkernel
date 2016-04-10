getenv_default <- function(varname, default) {
    value <- Sys.getenv(varname)
    if (identical(value, '')) default else value
}

#' @usage \code{
#' options(jupyter.* = ...)
#' getOption('jupyter.*')
#' jupyter_option_defaults$jupyter.*
#' }
#' @name IRkernel
#' @export
jupyter_option_defaults <- list(
    jupyter.log_level = as.integer(getenv_default('JUPYTER_LOG_LEVEL', 1L)),
    jupyter.log_to_file = FALSE,
    jupyter.logfile = getenv_default('JUPYTER_LOGFILE', 'irkernel.log'),
    jupyter.pager_classes = c(
        'help_files_with_topic'),
    jupyter.plot_mimetypes = c(
        'text/plain',
        'image/png',
        'image/svg+xml'),
    jupyter.in_kernel = FALSE)

.onLoad <- function(libname = NULL, pkgname = NULL) {
    for (opt_name in names(jupyter_option_defaults)) {
        if (is.null(getOption(opt_name))) {
            do.call(options, jupyter_option_defaults[opt_name])  # single []: preserve name
        }
    }
}
