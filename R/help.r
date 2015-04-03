#'An R kernel for IPython.
#'
#'IPython's modern interfaces, including the IPython Notebook, speak a JSON+ZMQ
#'protocol to a 'kernel' which is responsible for executing code. This protocol
#'is language agnostic, so other languages can take advantage of IPython's rich
#'UI by implementing a kernel. This package is a kernel for the R language.
#' @export main
#' @import methods
#' @import rzmq
#' @import uuid
#' @import digest
#' @import evaluate
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom base64enc base64encode
#' @importFrom IRdisplay display display_png display_multi
#' @docType package
#' @name IRkernel
#' @aliases IRkernel IRkernel-package
NULL
