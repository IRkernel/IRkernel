# This file contains an environment to store runtime variables independent from the kernel

runtime_env <- new.env()

#' Get global CommManager instance
#'
#' @return \link{CommManager} instance if a kernel is running, else NULL
#' @export
comm_manager <- function() runtime_env$comm_manager
