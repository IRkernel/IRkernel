#'<brief desc>
#'
#'<full description>
#' @export
#' @import rzmq
#' @import uuid
#' @import digest
#' @importFrom rjson fromJSON toJSON
hb_reply <- function() {
    data <- receive.socket(hb_socket, unserialize = FALSE)
    send.socket(hb_socket, data, serialize = FALSE)
}

#'<brief desc>
#'
#'<full description>
#' @param msg_lst <what param does>
#' @export
sign_msg <- function(msg_lst) {
    concat <- paste(msg_lst, collapse = "")
    return(hmac(connection_info$key, concat, "sha256"))
}
#'<brief desc>
#'
#'<full description>
#' @param socket <what param does>
#' @export
recv_multipart <- function(socket) {
    parts <- rawToChar(receive.socket(socket, unserialize = FALSE))
    while (get.rcvmore(socket)) {
        parts <- append(parts, rawToChar(receive.socket(socket, unserialize = FALSE)))
    }
    return(parts)
}
#'<brief desc>
#'
#'<full description>
#' @param socket <what param does>
#' @param  parts <what param does>
#' @export
send_multipart <- function(socket, parts) {
    for (part in parts[1:(length(parts) - 1)]) {
        send.raw.string(socket, part, send.more = TRUE)
    }
    send.raw.string(socket, parts[length(parts)], send.more = FALSE)
}
#'<brief desc>
#'
#'<full description>
#' @param parts <what param does>
#' @import rjson
#' @export
wire_to_msg <- function(parts) {
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
}
#'<brief desc>
#'
#'<full description>
#' @param msg <what param does>
#' @export
msg_to_wire <- function(msg) {
    bodyparts <- c(toJSON(msg$header), toJSON(msg$parent_header), toJSON(msg$metadata), 
        toJSON(msg$content))
    # Hack: an empty R list becomes [], not {}, which is what we want
    if (length(msg$metadata) == 0) {
        bodyparts[3] <- "{}"
    }
    signature <- sign_msg(bodyparts)
    # print(msg$identities)
    return(c(msg$identities, "<IDS|MSG>", signature, bodyparts))
}
#'<brief desc>
#'
#'<full description>
#' @param msg_type <what param does>
#' @param  parent_msg <what param does>
#' @export
new_reply <- function(msg_type, parent_msg) {
    header <- list(msg_id = UUIDgenerate(), username = parent_msg$header$username, 
        session = parent_msg$header$session, msg_type = msg_type)
    return(list(header = header, parent_header = parent_msg$header, identities = parent_msg$identities, 
        metadata = list()))
}
#'<brief desc>
#'
#'<full description>
#' @param msg_type <what param does>
#' @param  parent_msg <what param does>
#' @param  socket <what param does>
#' @param  content <what param does>
#' @export
send_response <- function(msg_type, parent_msg, socket, content) {
    msg <- new_reply(msg_type, parent_msg)
    msg$content <- content
    send_multipart(socket, msg_to_wire(msg))
}
#'<brief desc>
#'
#'<full description>
#' @param  <what param does>
#' @export
handle_shell <- function() {
    print("Shell msg")
    parts <- recv_multipart(shell_socket)
    msg <- wire_to_msg(parts)
    if (msg$header$msg_type == "execute_request") {
        execute(msg)
    } else if (msg$header$msg_type == "kernel_info_request") {
        kernel_info(msg)
    } else {
        print(c("Got unhandled msg_type:", msg$header$msg_type))
    }
}
execution_count <- 1
userenv <- new.env()
execute <- function(request) {
    send_response("status", request, iopub_socket, list(execution_state = "busy"))
    send_response("pyin", request, iopub_socket, list(code = request$code, execution_count = execution_count))
    code <- sprintf("withVisible({%s})", request$content$code)
    print(code)
    expr <- parse(text = code)
    result <- eval(expr, envir = userenv)
    print(userenv)
    print(result)
    if (result$visible) {
        data <- list()
        data["text/plain"] <- toString(result$value)
        send_response("pyout", request, iopub_socket, list(data = data, metadata = list(), 
            execution_count = execution_count))
    }
    send_response("status", request, iopub_socket, list(execution_state = "idle"))
    reply_content <- list(status = "ok", execution_count = execution_count, payload = list(), 
        user_variables = list(), user_expressions = list())
    send_response("execute_reply", request, shell_socket, reply_content)
    assign("execution_count", execution_count + 1, envir = .GlobalEnv)
}
kernel_info <- function(request) {
    send_response("kernel_info_reply", request, shell_socket, list(protocol_version = c(4, 
        0), language = "R"))
}
argv <- commandArgs(trailingOnly = TRUE)
connection_info <- fromJSON(file = argv[1])
print(connection_info)
url <- paste(connection_info$transport, "://", connection_info$ip, sep = "")
url_with_port <- function(port_name) {
    return(paste(url, ":", connection_info[port_name], sep = ""))
}
# ZMQ Socket setup
zmqctx <- init.context()
hb_socket <- init.socket(zmqctx, "ZMQ_REP")
bind.socket(hb_socket, url_with_port("hb_port"))
iopub_socket <- init.socket(zmqctx, "ZMQ_DEALER")
bind.socket(iopub_socket, url_with_port("iopub_port"))
control_socket <- init.socket(zmqctx, "ZMQ_DEALER")
bind.socket(control_socket, url_with_port("control_port"))
stdin_socket <- init.socket(zmqctx, "ZMQ_DEALER")
bind.socket(stdin_socket, url_with_port("stdin_port"))
shell_socket <- init.socket(zmqctx, "ZMQ_DEALER")
bind.socket(shell_socket, url_with_port("shell_port"))
while (1) {
    events <- poll.socket(list(hb_socket, shell_socket, control_socket), list("read", 
        "read", "read"), timeout = -1L)
    if (events[[1]]$read) {
        # heartbeat
        hb_reply()
    }
    if (events[[2]]$read) {
        # Shell socket
        handle_shell()
    }
} 
