#' @include execution.r help.r comm_manager.r logging.r utils.r
PROTOCOL_VER = '5.3'

Kernel <- setRefClass(
    'Kernel',
    fields = list(
        connection_info = 'list',
        zmqctx          = 'externalptr',
        sockets         = 'list',
        executor        = 'Executor',
        comm_manager    = 'CommManager'),
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
        session  = parent_msg$header$session,
        username = parent_msg$header$username,
        # ISO 8601, 6 is the maximum number decimal digits supported by R
        date = strftime(as.POSIXlt(Sys.time(), 'UTC'), '%Y-%m-%dT%H:%M:%OS6Z'),
        msg_type = msg_type,
        version  = PROTOCOL_VER)
    
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
    if (grepl('_reply$', msg_type) && is.null(content$status)) {
        content$status <- 'ok'
    }
    msg$content <- content
    socket <- sockets[[socket_name]]
    zmq.send.multipart(socket, msg_to_wire(msg), serialize = FALSE)
    log_debug('Sending msg %s.%s', socket_name, msg$header$msg_type)
},

handle_shell = function() {
    "React to a shell message coming in"
    
    parts <- zmq.recv.multipart(sockets$shell, unserialize = FALSE)
    msg <- wire_to_msg(parts)
    
    # protocol 5.0: send busy/idle around all of these
    send_response('status', msg, 'iopub', list(
        execution_state = 'busy'))
    
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
        inspect_request     = inspect(msg),
        shutdown_request    = shutdown(msg),
        log_debug(c('Got unhandled msg_type:', msg$header$msg_type)))
        
    send_response('status', msg, 'iopub', list(
        execution_state = 'idle'))
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
            c(ZMQ.PO()$POLLIN), # type
            0) # zero timeout, only what's already there
        log_debug('abort loop: after poll')
        if (bitwAnd(zmq.poll.get.revents(1), ZMQ.PO()$POLLIN)) {
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

handle_stdin = function() {
    "React to a stdin message coming in"
    
    # wait for 'input_reply' response message
    while (TRUE) {
        log_debug('stdin loop: beginning')
        zmq.poll(c(sockets$stdin),          # only stdin channel
                 c(ZMQ.PO()$POLLIN)) # type
        
        if (bitwAnd(zmq.poll.get.revents(1), ZMQ.PO()$POLLIN)) {
            log_debug('stdin loop: found msg')
            parts <- zmq.recv.multipart(sockets$stdin, unserialize = FALSE)
            msg <- wire_to_msg(parts)
            return(msg$content$value)
        } else {
            # else shouldn't be possible
            log_error('stdin loop: zmq.poll returned but no message found?')
        }
    }
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
    } else if (grepl(gettext('unexpected end of input', domain = 'R'), message, fixed = TRUE)) {
        # missing closing parenthesis
        'incomplete'
    } else if (grepl(gettextf('unexpected %s', 'INCOMPLETE_STRING', domain = 'R'), message, fixed = TRUE)) {
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
    
    comps <- completions(code, cursor_pos)
    send_response('complete_reply', request, 'shell', list(
        matches = as.list(comps$comps),  # make single strings not explode into letters
        metadata = namedlist(),
        cursor_start = comps$start,
        cursor_end = comps$end))
},

inspect = function(request) {
    # 5.0 protocol:
    code <- request$content$code
    cursor_pos <- request$content$cursor_pos

    title_templates <- list(
        'text/plain' = '# %s:\n',
        'text/html' = '<h1>%s:</h1>\n')
    # Function to add a section to content.
    add_new_section <- function(data, section_name, new_data) {
        for (mime in names(title_templates)) {
            new_content <- new_data[[mime]]
            if (is.null(new_content)) next
            title <- sprintf(title_templates[[mime]], section_name)
            # use paste0 since sprintf cannot deal with format strings > 8192 bytes
            data[[mime]] <- paste0(data[[mime]], title, new_content, '\n', sep = '\n')
        }
        return(data)
    }

    # Get token under the `cursor_pos`.
    # Since `.guessTokenFromLine()` does not check the characters after `cursor_pos`
    # check them by a loop. Use get since R CMD check does not like :::
    token <- ''
    for (i in seq(cursor_pos, nchar(code))) {
        token_candidate <- get('.guessTokenFromLine', asNamespace('utils'))(code, i)
        if (nchar(token_candidate) == 0) break
        token <- token_candidate
    }

    data <- namedlist()
    if (nchar(token) != 0) {
        # In many cases `get(token)` works, but it does not
        # in the cases such as `token` is a numeric constant or a reserved word.
        # Therefore `eval()` is used here.
        obj <- tryCatch(eval(parse(text = token), envir = .GlobalEnv), error = function(e) NULL)
        class_data <- if (!is.null(obj)) IRdisplay::prepare_mimebundle(class(obj))$data
        print_data <- if (!is.null(obj)) IRdisplay::prepare_mimebundle(obj)$data
        
        # `help(token)` is  not used here because it does not works
        # in the cases `token` is in `pkg::topic`or `pkg:::topic` form.
        help_data <- tryCatch({
            help_obj <- eval(parse(text = paste0('?', token)))
            IRdisplay::prepare_mimebundle(help_obj)$data
        }, error = function(e) NULL)
        
        # only show help if we have a function
        if ('function' %in% class(obj) && !is.null(help_data)) {
            data <- help_data
        } else {# any of those that are NULL are automatically skipped
            data <- add_new_section(data, 'Class attribute', class_data)
            data <- add_new_section(data, 'Printed form', print_data)
            data <- add_new_section(data, 'Help document', help_data)
        }
    }
    found <- length(data) != 0
    send_response('inspect_reply', request, 'shell', list(
        found = found,
        data = data,
        metadata = namedlist()))
},

history = function(request) {
    send_response('history_reply', request, 'shell', list(history = list()))
},

kernel_info = function(request) {
    rversion <- paste0(version$major, '.', version$minor)
    send_response('kernel_info_reply', request, 'shell', list(
        protocol_version       = PROTOCOL_VER,
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
        log_debug(paste('Unhandled control message, msg_type:', msg$header$msg_type))
    }
},

shutdown = function(request) {
    send_response('shutdown_reply', request, 'control', list(
        restart = request$content$restart))
    # Always call the base quit() during shutdown since execution shadows it.
    backup_env$base_quit('no')  # bound during startup in .onLoad
},

initialize = function(connection_file) {
    if (is.character(connection_file)) connection_file <- file(connection_file)
    connection_info <<- fromJSON(connection_file)
    stopifnot(connection_info$transport %in% c('tcp', 'ipc'))
    
    url <- paste0(connection_info$transport, '://', connection_info$ip)
    url_with_port <- function(port_name) {
        sep <- switch(connection_info$transport, tcp = ':', ipc = '-')
        paste0(url, sep, connection_info[[port_name]])
    }
    
    # ZMQ Socket setup
    zmqctx <<- zmq.ctx.new()
    sockets <<- list(
        hb      = zmq.socket(zmqctx, ZMQ.ST()$REP),
        iopub   = zmq.socket(zmqctx, ZMQ.ST()$PUB),
        control = zmq.socket(zmqctx, ZMQ.ST()$ROUTER),
        stdin   = zmq.socket(zmqctx, ZMQ.ST()$ROUTER),
        shell   = zmq.socket(zmqctx, ZMQ.ST()$ROUTER))
    
    # Enable handover: https://github.com/IRkernel/IRkernel/issues/508
    for (router in sockets[c('control', 'stdin', 'shell')]) {
        zmq.setsockopt(router, ZMQ.SO()$ROUTER_HANDOVER, 1L)
    }
    
    zmq.bind(sockets$hb,      url_with_port('hb_port'))
    zmq.bind(sockets$iopub,   url_with_port('iopub_port'))
    zmq.bind(sockets$control, url_with_port('control_port'))
    zmq.bind(sockets$stdin,   url_with_port('stdin_port'))
    zmq.bind(sockets$shell,   url_with_port('shell_port'))

    executor <<- Executor$new(
        send_response         = .self$send_response,
        handle_stdin          = .self$handle_stdin,
        abort_queued_messages = .self$abort_queued_messages)
    
    comm_manager <<- CommManager$new(send_response = .self$send_response)
    runtime_env$comm_manager <- comm_manager
},

run = function() {
    options(jupyter.in_kernel = TRUE)

    while (TRUE) {
        log_debug('main loop: beginning')
        r <- tryCatch(
            zmq.poll(
                c(sockets$hb, sockets$shell, sockets$control),
                rep(ZMQ.PO()$POLLIN, 3),
                MC = ZMQ.MC(check.eintr = TRUE)),
            interrupt = function(e) list(0L, 'SIGINT'))
        log_debug('main loop: after poll. ZMQ code: %s; Errno: %s', r[[1L]], r[[2L]])
        if (identical(r[[2L]], 'SIGINT')) {
            log_info('main loop: keyboard interrupt caught')
            next
        }

        # It's important that these messages are handled one by one in each
        # look. The problem is that during the handler, a new zmq.poll could be
        # done (and is done in case of errors in a execution request) and this
        # invalidates the zmq.poll.get.revents call leading to "funny" results
        # with found control message even if there are no control messages. So
        # the easiest seems to be to handle this in a big if .. else if .. else
        # clause...
        # https://github.com/IRkernel/IRkernel/pull/266
        if (bitwAnd(zmq.poll.get.revents(1), ZMQ.PO()$POLLIN)) {
            log_debug('main loop: hb')
            hb_reply()
        } else if (bitwAnd(zmq.poll.get.revents(2), ZMQ.PO()$POLLIN)) {
            log_debug('main loop: shell')
            handle_shell()
        } else if (bitwAnd(zmq.poll.get.revents(3), ZMQ.PO()$POLLIN)) {
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
