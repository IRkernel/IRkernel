#' Install the kernelspec to tell Jupyter (or IPython ≥ 3) about IRkernel.
#'
#' Will use jupyter and its config directory if available, but fall back to ipython if not.
#'
#' This can be called multiple times for different R interpreter, but you have to give a
#' different name (and displayname to see a difference in the notebook UI). If the same
#' name is give, it will overwrite older versions of the kernel spec with that name!
#'
#' @param user Install into user directory (~/.jupyter or ~/.ipython) or globally?
#' @param name The name of the kernel (default "ir")
#' @param displayname The name which is displayed in the notebook (default: "R")
#'
#' @export
installspec <- function(user = TRUE, name = 'ir', displayname = 'R') {
    found_binary <- FALSE
    for (binary in c('jupyter', 'ipython', 'ipython3', 'ipython2')) {
        version <- tryCatch(system2(binary, '--version', TRUE, FALSE), error = function(e) '0.0.0')
        if (compareVersion(version, '3.0.0') >= 0) {
            found_binary <- TRUE
            break
        }
    }

    if (!found_binary)
        stop(paste0('Jupyter or IPython 3.0 has to be installed but could neither run ', qQuote('jupyter'),
                    ' nor ', qQuote('ipython'), ', ', qQuote('ipython2'), ' or ', qQuote('ipython3'), '.\n',
                    '(Note that ', qQuote('ipython2'), ' is just IPython for Python 2, but still may be IPython 3.0)'))

    # make a kernelspec with the current interpreter's absolute path
    srcdir <- system.file('kernelspec', package = 'IRkernel')
    tmp_name <- tempfile()
    dir.create(tmp_name)
    file.copy(srcdir, tmp_name, recursive = TRUE)
    spec_path <- file.path(tmp_name, 'kernelspec', 'kernel.json')
    spec <- fromJSON(spec_path)
    spec$argv[[1]] <- file.path(R.home('bin'), 'R')
    spec$display_name <- displayname
    write(toJSON(spec, pretty = TRUE, auto_unbox = TRUE), file = spec_path)

    user_flag <- if (user) '--user' else character(0)
    args <- c('kernelspec', 'install', '--replace', '--name', name, user_flag, file.path(tmp_name, 'kernelspec'))
    system2(binary, args, wait = TRUE)

    unlink(tmp_name, recursive = TRUE)
}
