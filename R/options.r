#' @usage
#' jupyter_option_defaults
#' 
#' @rdname IRkernel-package
#' @export
jupyter_option_defaults <- list(
    jupyter.rich_display = TRUE, # moved from IRdisplay
    jupyter.log_level = 1L,
    jupyter.logfile = NA,
    jupyter.pager_classes = c(
        'packageIQR',
        'help_files_with_topic'),
    jupyter.plot_mimetypes = c(
        'text/plain',
        'image/png'),
    jupyter.plot_scale = 2,
    jupyter.in_kernel = FALSE)

from_env <- list(
    JUPYTER_LOG_LEVEL = as.integer,
    JUPYTER_LOGFILE = function(f) if (nchar(f) == 0) NA else f)

# converts e.g. jupyter.log_level to JUPYTER_LOG_LEVEL
opt_to_env <- function(nms) gsub('.', '_', toupper(nms), fixed = TRUE)

# called in .onLoad
init_options <- function() {
    for (opt_name in names(jupyter_option_defaults)) {
        # skip option if it is already set, e.g. in the Rprofile
        if (is.null(getOption(opt_name))) {
            # prepare `options` call from the default
            call_arg <- jupyter_option_defaults[opt_name]  # single [] preserve names
            
            # if an env var is set, get value from it.
            env_name <- opt_to_env(opt_name)
            convert <- from_env[[env_name]]
            env_val <- Sys.getenv(env_name, unset = NA)
            if (!is.null(convert) && !is.na(env_val))
                call_arg[[opt_name]] <- convert(env_val)
            
            do.call(options, call_arg)
        }
    }
}
