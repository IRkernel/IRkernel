#' @include execution.r help.r comm_manager.r logging.r
NULL

fromRawJSON <- function(r) {
    s <- rawToChar(r)
    Encoding(s) <- 'UTF-8'
    fromJSON(s)
}


#' The kernel
#' 
#' Has methods able to connect and talk to a Jupyter server.
#' 
#' @export
Kernel <- setRefClass(
    'Kernel',
    fields = list(
        connection_info = 'list',
        zmqctx          = 'externalptr',
        sockets         = 'list',
        executor        = 'Executor',
        comm_manager    = 'Comm_Manager'),
    methods = list(

hb_reply = function() {
    data <- zmq.msg.recv(sockets$hb, unserialize = FALSE)
    zmq.msg.send(data, sockets$hb, serialize = FALSE)
},

sign_msg = function(msg_lst) {
    "Sign messages"
    
    concat <- unlist(msg_lst)
    hmac(connection_info$key, concat, 'sha256')
},

wire_to_msg = function(parts) {
    "Deserialize a message"
    
    i <- 1
    while (!identical(parts[[i]], charToRaw('<IDS|MSG>'))) {
        i <- i + 1
    }
    if (!identical(connection_info$key, '')) {
        signature <- rawToChar(parts[[i + 1]])
        expected_signature <- sign_msg(parts[(i + 2):(i + 5)])
        stopifnot(identical(signature, expected_signature))
    }

    # Convert the four key parts of the message to strings and parse the JSON
    header        <- fromRawJSON(parts[[i + 2]])
    parent_header <- fromRawJSON(parts[[i + 3]])
    metadata      <- fromRawJSON(parts[[i + 4]])
    content       <- fromRawJSON(parts[[i + 5]])

    # ZMQ routing bits
    if (i > 1) {
        identities <- parts[1:(i - 1)]
    } else {
        identities <- NULL
    }

    list(
        header        = header,
        parent_header = parent_header,
        metadata      = metadata,
        content       = content,
        identities    = identities)
},

msg_to_wire = function(msg) {
    "Serialize a message"
    
    bodyparts <- list(
        charToRaw(toJSON(msg$header,        auto_unbox = TRUE)),
        charToRaw(toJSON(msg$parent_header, auto_unbox = TRUE)),
        charToRaw(toJSON(msg$metadata,      auto_unbox = TRUE)),
        charToRaw(toJSON(msg$content,       auto_unbox = TRUE)))
    
    signature <- sign_msg(bodyparts)
    c(
        msg$identities,
        list(charToRaw('<IDS|MSG>')),
        list(charToRaw(signature)),
        bodyparts)
},

new_reply = function(msg_type, parent_msg) {
    "Prepare a reply"
    
    header <- list(
        msg_id   = UUIDgenerate(),
        username = parent_msg$header$username,
        session  = parent_msg$header$session,
        msg_type = msg_type,
        version  = '5.0')
    
    list(
        header        = header,
        parent_header = parent_msg$header,
        identities    = parent_msg$identities,
        # Ensure this is {} in JSON, not []
        metadata      = namedlist())
},

send_response = function(msg_type, parent_msg, socket_name, content) {
    "Send a response"
    
    msg <- new_reply(msg_type, parent_msg)
    msg$content <- content
    socket <- sockets[[socket_name]]
    zmq.send.multipart(socket, msg_to_wire(msg), serialize = FALSE)
    log_debug('Sending msg %s', msg$header$msg_type)
},

handle_shell = function() {
    "React to a shell message coming in"
    
    parts <- zmq.recv.multipart(sockets$shell, unserialize = FALSE)
    msg <- wire_to_msg(parts)
    switch(
        msg$header$msg_type,
        comm_info_request   = comm_manager$on_comm_info_request(msg),
        comm_open           = comm_manager$on_comm_open(msg),
        comm_msg            = comm_manager$on_comm_msg(msg),
        comm_close          = comm_manager$on_comm_close(msg),
        execute_request     = executor$execute(msg),
        kernel_info_request = kernel_info(msg),
        history_request     = history(msg),
        complete_request    = complete(msg),
        is_complete_request = is_complete(msg),
        shutdown_request    = shutdown(msg),
        print(c('Got unhandled msg_type:', msg$header$msg_type)))
},

abort_shell_msg = function() {
    "Send an abort message for an incoming shell request"
    # See https://github.com/ipython/ipykernel/blob/1d97cb2a04149387a0d2dbea1b3d0af691d8df6c/ipykernel/kernelbase.py#L623

    parts <- zmq.recv.multipart(sockets$shell, unserialize = FALSE)
    msg <- wire_to_msg(parts)
    log_debug('Aborting msg of type %s', msg$header$msg_type)
    reply_type <- paste0(unlist(strsplit(msg$header$msg_type, '_'))[1], '_reply')
    reply_content <- list(status = 'aborted')
    send_response(reply_type, msg, 'shell', reply_content)
    log_debug('Aborted msg')
},

abort_queued_messages = function() {
    "Abort all already queued shell messages after an error"

    log_debug('abort loop: aborted all outstanding msg')
    while (TRUE) {
        log_debug('abort loop: before poll')
        ret <- zmq.poll(
            c(sockets$shell), # only shell channel
            c(.pbd_env$ZMQ.PO$POLLIN), # type
            0) # zero timeout, only what's already there
        log_debug('abort loop: after poll')
        if(bitwAnd(zmq.poll.get.revents(1), .pbd_env$ZMQ.PO$POLLIN)) {
            log_debug('abort loop: found msg')
            abort_shell_msg()
        } else {
            # no more messages...
            log_debug('abort loop: breaking')
            break
        }
    }
    log_debug('abort loop: end')

},

is_complete = function(request) {
    "Checks whether the code in the rest is complete"
    
    code <- request$content$code
    message <- tryCatch({
        parse_all(code)
        # the code compiles, so we are complete (either no code at all / only
        # comments or syntactically correct code)
        'complete'
    }, error = function(e) e$message)
    
    # One of 'complete', 'incomplete', 'invalid', 'unknown'
    status <- if (message == 'complete') {
        # syntactical complete code
        'complete'
    } else if (grepl('unexpected end of input', message)) {
        # missing closing parenthesis
        'incomplete'
    } else if (grepl('unexpected INCOMPLETE_STRING', message)) {
        # missing closing quotes
        'incomplete'
    } else {
        # all else
        'invalid'
    }
    
    content <- list(status = status)
    if (status == 'incomplete') {
        # we don't try to guess the indention level and just return zero indention
        # That's fine because R has braces... :-)
        # TODO: do some guessing?
        content <- c(content, indent = '')
    }
    
    send_response('is_complete_reply', request, 'shell', content)
},

complete = function(request) {
    # 5.0 protocol:
    code <- request$content$code
    cursor_pos <- request$content$cursor_pos
    
    # Find which line we're on and position within that line
    lines <- strsplit(code, '\n', fixed = TRUE)[[1]]
    chars_before_line <- 0L
    for (line in lines) {
        new_cursor_pos <- cursor_pos - nchar(line) - 1L # -1 for the newline
        if (new_cursor_pos < 0L) {
            break
        }
        cursor_pos <- new_cursor_pos
        chars_before_line <- chars_before_line + nchar(line) + 1L
    }
    
    utils:::.assignLinebuffer(line)
    utils:::.assignEnd(cursor_pos)
    utils:::.guessTokenFromLine()
    utils:::.completeToken()
        
    # .guessTokenFromLine, like most other functions here usually sets variables in .CompletionEnv.
    # When specifying update = FALSE, it instead returns a list(token = ..., start = ...)
    c.info <- c(
        list(comps = utils:::.retrieveCompletions()),
        utils:::.guessTokenFromLine(update = FALSE))
    
    # good coding style for completions
    comps <- gsub('=$', ' = ', c.info$comps)
    
    start_position <- chars_before_line + c.info$start
    send_response('complete_reply', request, 'shell', list(
        matches = as.list(comps),  # make single strings not explode into letters
        metadata = namedlist(),
        status = 'ok',
        cursor_start = start_position,
        cursor_end = start_position + nchar(c.info$token)))
},

history = function(request) {
    send_response('history_reply', request, 'shell', list(history = list()))
},

kernel_info = function(request) {
    rversion <- paste0(version$major, '.', version$minor)
    send_response('kernel_info_reply', request, 'shell', list(
        protocol_version       = '5.0',
        implementation         = 'IRkernel',
        implementation_version = as.character(packageVersion('IRkernel')),
        language_info = list(
            name = 'R',
            codemirror_mode = 'r',
            pygments_lexer = 'r',
            mimetype = 'text/x-r-source',
            file_extension = '.r',
            version = rversion),
        banner = version$version.string))
},

handle_control = function() {
    log_debug('Control: beginning')
    parts <- zmq.recv.multipart(sockets$control, unserialize = FALSE)
    msg <- wire_to_msg(parts)
    log_debug('Control: recv msg of type %s', msg$header$msg_type)
    if (msg$header$msg_type == 'shutdown_request') {
        log_debug('Control: shutdown...')
        shutdown(msg)
    } else {
        print(paste('Unhandled control message, msg_type:', msg$header$msg_type))
    }
},

shutdown = function(request) {
    send_response('shutdown_reply', request, 'control', list(
        restart = request$content$restart))
    quit('no')
},

initialize = function(connection_file) {
    connection_info <<- fromJSON(connection_file)
    
    url <- paste0(connection_info$transport, '://', connection_info$ip)
    url_with_port <- function(port_name) {
        paste0(url, ':', connection_info[[port_name]])
    }
    
    # ZMQ Socket setup
    zmqctx <<- zmq.ctx.new()
    sockets <<- list(
        hb      = zmq.socket(zmqctx, .pbd_env$ZMQ.ST$REP),
        iopub   = zmq.socket(zmqctx, .pbd_env$ZMQ.ST$PUB),
        control = zmq.socket(zmqctx, .pbd_env$ZMQ.ST$ROUTER),
        stdin   = zmq.socket(zmqctx, .pbd_env$ZMQ.ST$ROUTER),
        shell   = zmq.socket(zmqctx, .pbd_env$ZMQ.ST$ROUTER))
    
    zmq.bind(sockets$hb,      url_with_port('hb_port'))
    zmq.bind(sockets$iopub,   url_with_port('iopub_port'))
    zmq.bind(sockets$control, url_with_port('control_port'))
    zmq.bind(sockets$stdin,   url_with_port('stdin_port'))
    zmq.bind(sockets$shell,   url_with_port('shell_port'))

    executor <<- Executor$new(send_response = .self$send_response,
        abort_queued_messages = .self$abort_queued_messages)
    comm_manager <<- Comm_Manager$new(send_response = .self$send_response)
    comm_manager_env$comm_manager <- comm_manager
},

run = function() {
    options(jupyter.in_kernel = TRUE)
    while (TRUE) {
        log_debug('main loop: beginning')
        zmq.poll(
            c(sockets$hb, sockets$shell, sockets$control),
            rep(.pbd_env$ZMQ.PO$POLLIN, 3))
        log_debug('main loop: after poll')

        # It's important that these messages are handler one  by one in each
        # look. The problem is that during the handler, a new zmq.poll could be
        # done (and is done in case of errors in a execution request) and this
        # invalidates the zmq.poll.get.revents call leading to "funny" results
        # with found control message even if there are no control messages. So
        # the easiest seems to be to handle this in a big if .. else if .. else
        # clause...
        # https://github.com/IRkernel/IRkernel/pull/266
        if(bitwAnd(zmq.poll.get.revents(1), .pbd_env$ZMQ.PO$POLLIN)) {
            log_debug('main loop: hb')
            hb_reply()
        } else if(bitwAnd(zmq.poll.get.revents(2), .pbd_env$ZMQ.PO$POLLIN)) {
            log_debug('main loop: shell')
            handle_shell()
        } else if(bitwAnd(zmq.poll.get.revents(3), .pbd_env$ZMQ.PO$POLLIN)) {
            log_debug('main loop: control')
            handle_control()
        } else {
            # else shouldn't be possible
            log_debug('main loop: zmq.poll returned but no message found?')
        }
    }
    log_debug('main loop: end')
})
)

comm_manager_env <- new.env()
#' Get global CommManager instance
#'
#' @return \link{CommManager} instance if a kernel is running, else NULL
#' @export
comm_manager <- function() comm_manager_env$comm_manager

#' Initialise and run the kernel
#'
#' @param connection_file The path to the Jupyter connection file, written by the frontend
#' 
#' @export 
main <- function(connection_file = '') {
    if (connection_file == '') {
        # On Windows, passing the connection file in as a string literal fails,
        # because the \U in C:\Users looks like a unicode escape. So, we have to
        # pass it as a separate command line argument.
        connection_file <- commandArgs(TRUE)[[1]]
    }
    kernel <- Kernel$new(connection_file = connection_file)
    kernel$run()
}
