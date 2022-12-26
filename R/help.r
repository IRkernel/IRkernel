#' An R kernel for Jupyter.
#' 
#' Jupyter speaks a JSON+ZMQ protocol to a 'kernel' which is responsible for executing code.
#' This package is a kernel for the R language.
#' 
#' @section Options:
#' 
#' The following can be set/read via \code{options(opt.name = ...)} / \code{getOption('opt.name')}
#' 
#' \describe{
#'   \item{\code{jupyter.log_level}}{1L (errors), 2L (warnings), or 3L (debug). 1L is the default.}
#'   \item{\code{jupyter.pager_classes}}{Classes to use the pager for instead of displaying them inline. Default: help pages}
#'   \item{\code{jupyter.in_kernel}}{\code{TRUE} if this code is executed in a running kernel. Set to pretend being/not being in a kernel}
#'   \item{\code{jupyter.rich_display}}{Use more than just text display}
#'   \item{\code{jupyter.display_mimetypes}}{
#'      The formats emitted when any return value is to be displayed
#'      (default: all mimetypes listed \href{http://ipython.org/ipython-doc/stable/api/generated/IPython.core.formatters.html#IPython.core.formatters.format_display_data}{here}) 
#'   }
#'   \item{\code{jupyter.plot_mimetypes}}{
#'      The plot formats emitted to the frontend when a plot is displayed.
#'      (default: image/png and application/pdf)
#'   }
#'   \item{\code{jupyter.plot_scale}}{
#'      The ratio (notebook PPI / \code{repr.plot.res}).
#'      E.g.: With the defaults \code{repr.plot.res}=120 px/in (PPI) and \code{jupyter.plot_scale}=2, 
#'      a 1in\eqn{\times}1in image will be displayed as a 0.5in\eqn{\times}0.5in, 240 PPI image.
#'      (default: 2, fit for retina displays)
#'   }
#' }
#'
#' @export main
#' 
#' @import methods
#' @import uuid
#' @import digest
#' @importFrom pbdZMQ zmq.ctx.new zmq.socket zmq.bind zmq.getsockopt zmq.setsockopt
#' @importFrom pbdZMQ zmq.send zmq.recv zmq.msg.send zmq.msg.recv zmq.send.multipart zmq.recv.multipart
#' @importFrom pbdZMQ zmq.poll zmq.poll.get.revents ZMQ.MC ZMQ.PO ZMQ.ST ZMQ.SO
#' @importFrom evaluate evaluate new_output_handler parse_all
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom IRdisplay publish_mimebundle prepare_mimebundle
#' @importFrom repr mime2repr repr_option_defaults
#' 
#' @docType package
#' @seealso \link{installspec}
#' @name IRkernel-package
#' @aliases IRkernel IRkernel-package IRkernel-options
NULL
