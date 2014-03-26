#'Receive multipart ZMQ message
#'
#'Returns a string vector, one element per message part.
#'This will hopefully become part of rzmq.
#'
#' @param socket The ZMQ socket from which to receive data
recv_multipart <- function(socket) {
    parts <- rawToChar(receive.socket(socket, unserialize = FALSE))
    while (get.rcvmore(socket)) {
        parts <- append(parts, rawToChar(receive.socket(socket, unserialize = FALSE)))
    }
    return(parts)
}
#'Send multipart ZMQ message.
#'
#'This will hopefully become part of rzmq.
#' @param socket The ZMQ socket on which to send data
#' @param parts A string vector; each element will be sent as one part of the message
send_multipart <- function(socket, parts) {
    for (part in parts[1:(length(parts) - 1)]) {
        send.raw.string(socket, part, send.more = TRUE)
    }
    send.raw.string(socket, parts[length(parts)], send.more = FALSE)
}
