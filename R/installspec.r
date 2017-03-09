#' Install the kernelspec to tell Jupyter about IRkernel.
#'
#' This can be called multiple times for different R interpreter, but you have to give a
#' different name (and displayname to see a difference in the notebook UI). If the same
#' name is give, it will overwrite older versions of the kernel spec with that name!
#'
#' @param user         Install into user directory (\href{https://specifications.freedesktop.org/basedir-spec/latest/ar01s03.html}{\code{$XDG_DATA_HOME}}\code{/jupyter/kernels}) or globally?
#' @param name         The name of the kernel (default "ir")
#' @param displayname  The name which is displayed in the notebook (default: "R")
#' 
#' @return Exit code of the \code{jupyter kernelspec install} call.
#' 
#' @export
installspec <- function(user = TRUE, name = 'ir', displayname = 'R') {
    exit_code <- system2('jupyter', c('kernelspec', '--version'), FALSE, FALSE)
    if (exit_code != 0)
        stop('jupyter-client has to be installed but ', dQuote('jupyter kernelspec --version'), ' exited with code ', exit_code, '.\n')
    
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
    exit_code <- system2('jupyter', args)
    
    unlink(tmp_name, recursive = TRUE)
    
    invisible(exit_code)
}
