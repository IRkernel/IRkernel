# This file contains an environment to store runtime variables independent from the kernel

comm_manager_env <- new.env()

#' Get global Comm_Manager instance
#'
#' @return \link{Comm_Manager} instance if a kernel is running, else NULL
#' @export
comm_manager <- function() comm_manager_env$comm_manager
