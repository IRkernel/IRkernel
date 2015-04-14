#' @export
set_plot_options <- function(...) {
    .Deprecated('options', msg = 'use the `repr.plot.*` options from the repr package instead')
    opts <- list(...)
    names(opts) <- paste0('repr.plot.', names(opts))
    do.call(options, opts)
}

#' @export
get_plot_options <- function() {
    .Deprecated('getOption', msg = 'use the `repr.plot.*` options from the repr package instead')
    all.opts <- options()
    plot.opt.idx <- grep('^repr.plot', names(all.opts))
    plot.opts <- all.opts[plot.opt.idx]
    names(plot.opts) <- gsub('^repr\\.plot\\.', '', names(plot.opts))
    return(as.list(plot.opts))
}

# all plot mime types. default value for the jupyter.plot_mimetypes option
plot_mimetypes <- c(
    'image/png',
    'application/pdf',
    'image/svg+xml')

.onLoad = function(libname = NULL, pkgname = NULL) {
    if (is.null(getOption('jupyter.plot_mimetypes')))
        options(jupyter.plot_mimetypes = plot_mimetypes)
}