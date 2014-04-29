Kernel <- setRefClass("Kernel",
                fields = c("connection_info", "zmqctx", "sockets", "executor"),
                methods= list(

hb_reply = function() {
    data <- receive.socket(sockets$hb, unserialize = FALSE)
    send.socket(sockets$hb, data, serialize = FALSE)
},

#'<brief desc>
#'
#'<full description>
#' @param msg_lst <what param does>
#' @export
sign_msg = function(msg_lst) {
    concat <- paste(msg_lst, collapse = "")
    return(hmac(connection_info$key, concat, "sha256"))
},
#'<brief desc>
#'
#'<full description>
#' @param parts <what param does>
#' @import rjson
#' @export
wire_to_msg = function(parts) {
    i <- 1
    # print(parts)
    while (parts[i] != "<IDS|MSG>") {
        i <- i + 1
    }
    signature <- parts[i + 1]
    expected_signature <- sign_msg(parts[(i + 2):(i + 5)])
    stopifnot(identical(signature, expected_signature))
    header <- fromJSON(parts[i + 2])
    parent_header <- fromJSON(parts[i + 3])
    metadata <- fromJSON(parts[i + 4])
    content <- fromJSON(parts[i + 5])
    if (i > 1) {
        identities <- parts[1:(i - 1)]
    } else {
        identities <- NULL
    }
    return(list(header = header, parent_header = parent_header, metadata = metadata, 
        content = content, identities = identities))
},
#'<brief desc>
#'
#'<full description>
#' @param msg <what param does>
#' @export
msg_to_wire = function(msg) {
    bodyparts <- c(toJSON(msg$header, auto_unbox=TRUE), toJSON(msg$parent_header, auto_unbox=TRUE), 
        toJSON(msg$metadata, auto_unbox=TRUE), toJSON(msg$content, auto_unbox=TRUE))
    signature <- sign_msg(bodyparts)
    # print(msg$identities)
    return(c(msg$identities, "<IDS|MSG>", signature, bodyparts))
},
#'<brief desc>
#'
#'<full description>
#' @param msg_type <what param does>
#' @param  parent_msg <what param does>
#' @export
new_reply = function(msg_type, parent_msg) {
    header <- list(msg_id = UUIDgenerate(), username = parent_msg$header$username, 
        session = parent_msg$header$session, msg_type = msg_type)
    return(list(header = header, parent_header = parent_msg$header, identities = parent_msg$identities, 
        metadata = dict()  # Ensure this is {} in JSON, not []
        ))
},
#'<brief desc>
#'
#'<full description>
#' @param msg_type <what param does>
#' @param  parent_msg <what param does>
#' @param  socket <what param does>
#' @param  content <what param does>
#' @export
send_response = function(msg_type, parent_msg, socket_name, content) {
    msg <- new_reply(msg_type, parent_msg)
    msg$content <- content
    socket <- sockets[socket_name][[1]]
    send.multipart(socket, msg_to_wire(msg))
},
#'<brief desc>
#'
#'<full description>
#' @param  <what param does>
#' @export
handle_shell = function() {
    parts <- receive.multipart(sockets$shell)
    msg <- wire_to_msg(parts)
    if (msg$header$msg_type == "execute_request") {
        executor$execute(msg)
    } else if (msg$header$msg_type == "kernel_info_request") {
        kernel_info(msg)
    } else if (msg$header$msg_type == "history_request") {
        history(msg)
    } else {
        print(c("Got unhandled msg_type:", msg$header$msg_type))
    }
},

history = function(request) {
  send_response("history_reply", request, 'shell', list(history=list()))
},

kernel_info = function(request) {
  send_response("kernel_info_reply", request, 'shell',
                list(protocol_version=c(4, 0), language="R"))
},

handle_control = function() {
  parts = receive.multipart(sockets$control)
  msg = wire_to_msg(parts)
  if (msg$header$msg_type == "shutdown_request") {
    shutdown(msg)
  } else {
    print(c("Unhandled control message, msg_type:", msg$header$msg_type))
  }
},

shutdown = function(request) {
  send_response('shutdown_reply', request, 'control',
                list(restart=request$content$restart))
  stop("Shut down by frontend.")
},

initialize = function(connection_file) {
    connection_info <<- fromJSON(connection_file)
    print(connection_info)
    url <- paste(connection_info$transport, "://", connection_info$ip, sep = "")
    url_with_port <- function(port_name) {
        return(paste(url, ":", connection_info[port_name], sep = ""))
    }

    # ZMQ Socket setup
    zmqctx <<- init.context()
    sockets <<- list(
        hb = init.socket(zmqctx, "ZMQ_REP"),
        iopub = init.socket(zmqctx, "ZMQ_PUB"),
        control = init.socket(zmqctx, "ZMQ_ROUTER"),
        stdin = init.socket(zmqctx, "ZMQ_ROUTER"),
        shell = init.socket(zmqctx, "ZMQ_ROUTER")
    )
    bind.socket(sockets$hb, url_with_port("hb_port"))
    bind.socket(sockets$iopub, url_with_port("iopub_port"))
    bind.socket(sockets$control, url_with_port("control_port"))
    bind.socket(sockets$stdin, url_with_port("stdin_port"))
    bind.socket(sockets$shell, url_with_port("shell_port"))

    executor <<- Executor$new(kernel=.self)
},

run = function() {
    while (1) {
        events <- poll.socket(list(sockets$hb, sockets$shell, sockets$control),
                              list("read", "read", "read"), timeout = -1L)
        if (events[[1]]$read) {
            # heartbeat
            hb_reply()
        }
        if (events[[2]]$read) {
            # Shell socket
            handle_shell()
        }

        if (events[[3]]$read) {  # Control socket
            handle_control()
        }
    }
})
)

#'Initialise and run the kernel
#'
#'@param connection_file The path to the IPython connection file, written by the frontend
#'@export 
main <- function(connection_file) {
    kernel <- Kernel$new(connection_file=connection_file)
    kernel$run()
}
