#' Initialise and run the kernel
#'
#' @param connection_file  The path to the Jupyter connection file, written by the frontend
#' 
#' @export 
main <- function(connection_file = '') {
    if (connection_file == '') {
        # On Windows, passing the connection file in as a string literal fails,
        # because the \U in C:\Users looks like a unicode escape. So, we have to
        # pass it as a separate command line argument.
        connection_file <- commandArgs(TRUE)[[1]]
    }
    log_debug('Starting the R kernel...')
    kernel <- Kernel$new(connection_file = connection_file)
    kernel$run()
}
