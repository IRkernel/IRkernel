#' @include options.r class_unions.r
NULL

# Create an empty named list
#' @importFrom stats setNames
namedlist <- function() setNames(list(), character(0))

plot_builds_upon <- function(prev, current) {
    if (is.null(prev)) {
        return(TRUE)
    }
    
    lprev <- length(prev[[1]])
    lcurrent <- length(current[[1]])
    
    lcurrent >= lprev && identical(current[[1]][1:lprev], prev[[1]][1:lprev])
}

ask <- function(prompt = '') {
    answer <- NA
    while (is.na(answer)) {
        answer <- switch(readline(prompt),
           y = , Y = TRUE,
           n = , N = FALSE,
           c = NULL,
           NA)
    }
    answer
}

format_stack <- function(calls) {
    line_refs <- rep('', length(calls))
    
    tb <- lapply(seq_along(calls), function(cl) {
        call <- calls[[cl]]
        
        # first_line, first_byte, last_line, last_byte, first_column, last_column, first_parsed, last_parsed
        ref <- attr(call, 'srcref')
        
        filename <- attr(ref, 'srcfile')$filename
        
        if (!is.null(ref)) {
            f <- ref[[1]]
            l <- ref[[3]]
            lines <- if (f == l) f else paste0(f, '-', l)
            line_refs[[cl]] <<- paste0('   # at line ', lines, ' of file ', filename)
        }
        
        white <- paste(rep(' ', nchar(format(cl))), collapse = '')
        
        f.call <- format(call)
        line.prefix <- c(cl, rep(white, length(f.call) - 1))
        paste(paste0(line.prefix, '. ', f.call), collapse = '\n')
    })
    
    paste0(tb, line_refs)
}


#' @importFrom utils capture.output
Executor <- setRefClass(
    Class = 'Executor',
    fields = list(
        send_response         = 'function',
        handle_stdin          = 'function',
        abort_queued_messages = 'function',
        execution_count       = 'integer',
        payload               = 'list',
        err                   = 'list',
        interrupted           = 'logical',
        last_recorded_plot    = 'recordedplotOrNULL',
        current_request       = 'listOrNULL',
        nframe                = 'integer'),
    methods = list(

is_silent = function() {
    current_request$content$silent
},

should_store_history = function() {
    sh <- current_request$content$store_history
    !is.null(sh) && sh
},

send_error_msg = function(msg) {
    if (!is_silent()) return()
    send_response('stream', current_request, 'iopub',
                  list(name = 'stderr', text = msg))

},

display_data = function(data, metadata = NULL) {
    if (is.null(metadata)) {
        metadata <- namedlist()
    }
    send_response('display_data', current_request, 'iopub', list(
        source = 'R display func',
        data = data,
        metadata = metadata))
    
    invisible(TRUE)
},

page = function(mimebundle) {
    payload <<- c(payload, list(c(source = 'page', mimebundle)))
},

# .Last doesn’t seem to work, so replicating behavior
quit = function(save = 'default', status = 0, runLast = TRUE) {
    save <- switch(save,
        default = , yes = TRUE,
        no = FALSE,
        ask = ask('Save workspace image? [y/n/c]: '),
        stop('unknown `save` value'))
    if (is.null(save)) return()  # cancel
    if (runLast) {
        if (!is.null(.GlobalEnv$.Last)) .GlobalEnv$.Last()
        if (!is.null(.GlobalEnv$.Last.sys)) .GlobalEnv$.Last.sys()
    }
    if (save) NULL  # TODO: actually save history
    payload <<- c(.self$payload, list(list(source = 'ask_exit', keepkernel = FALSE)))
},

# noninteractive
readline = function(prompt = '') {
    log_debug('entering custom readline')
    send_response('input_request', current_request, 'stdin',
        list(prompt = prompt, password = FALSE))
    # wait for 'input_reply' response message
    input <- handle_stdin()
},

# noninteractive 5.0 protocol:
get_pass = function(prompt = '') {
    log_debug('entering custom get_pass')
    send_response('input_request', current_request, 'stdin',
        list(prompt = prompt, password = TRUE))
    # wait for 'input_reply' response message
    log_debug('exiting custom get_pass')
    input <- handle_stdin()
},

handle_error = function(e) tryCatch({
    log_debug('Error output: %s', toString(e))
    calls <- head(sys.calls()[-seq_len(nframe + 1L)], -3)
    
    calls <- skip_repeated(calls)
    
    msg <- paste0(toString(e), 'Traceback:\n')
    stack_info <- format_stack(calls)

    err <<- list(ename = 'ERROR', evalue = toString(e), traceback = as.list(c(msg, stack_info)))
    if (!is_silent()) {
        send_response('error', current_request, 'iopub', c(err, list(
            execution_count = execution_count)))
    }
}, error = log_error),

send_plot = function(plotobj) {
    formats <- namedlist()
    metadata <- namedlist()
    for (mime in getOption('jupyter.plot_mimetypes')) {
        tryCatch({
            formats[[mime]] <- mime2repr[[mime]](plotobj, attr(plotobj, '.irkernel_width'), attr(plotobj, '.irkernel_height'))
        }, error = handle_error)
        # Isolating SVGs (putting them in an iframe) avoids strange
        # interactions with CSS on the page.
        if (identical(mime, 'image/svg+xml')) {
            metadata[[mime]] <- list(isolated = TRUE)
        }
    }
    publish_mimebundle(formats, metadata)
},

handle_display_error = function(e) {
    # This is used with withCallingHandler and only has two additional
    # calls at the end instead of the 3 for tryCatch... (-2 at the end)
    # we also remove the tryCatch and mime2repr stuff at the head of the callstack (+7)
    calls <- head(sys.calls()[-seq_len(nframe + 7L)], -2)
    stack_info <- format_stack(calls)
    msg <- sprintf('ERROR while rich displaying an object: %s\nTraceback:\n%s\n',
                   toString(e),
                   paste(stack_info, collapse = '\n'))
    log_debug(msg)
    send_error_msg(msg)
},

handle_value = function(obj, visible) {
    log_debug('Value output...')
    set_last_value(obj)
    if (visible) {
        mimebundle <- prepare_mimebundle_kernel(obj, .self$handle_display_error)
        if (length(intersect(class(obj), getOption('jupyter.pager_classes'))) > 0) {
            log_debug('Showing pager: %s', paste(capture.output(str(mimebundle$data)), collapse = '\n'))
            page(mimebundle)
        } else {
            log_debug('Sending display_data: %s', paste(capture.output(str(mimebundle$data)), collapse = '\n'))
            send_response('display_data', current_request, 'iopub', mimebundle)
        }
    }
},

stream = function(output, streamname) {
    log_debug('Stream output: %s', output)
    send_response('stream', current_request, 'iopub', list(
        name = streamname,
        text = paste(output, collapse = '\n')))
},

handle_graphics = function(plotobj) {
    log_debug('Graphics output...')
    if (!plot_builds_upon(last_recorded_plot, plotobj)) {
        log_debug('Sending plot...')
        send_plot(last_recorded_plot)
    }
    # need to be set here to capture the size and have it available when the plot is sent
    attr(plotobj, '.irkernel_width')  <- getOption('repr.plot.width',  repr_option_defaults$repr.plot.width)
    attr(plotobj, '.irkernel_height') <- getOption('repr.plot.height', repr_option_defaults$repr.plot.height)
    last_recorded_plot <<- plotobj
},

handle_message = function(o) {
    log_debug('Message output: %s', o)
    stream(paste(conditionMessage(o), collapse = ''), 'stderr')
},

handle_warning = function(o) {
    call <- conditionCall(o)
    call <- if (is.null(call)) '' else sprintf(' in %s', deparse(call)[[1]])
    msg <- sprintf('Warning message%s:\n%s', call, dQuote(conditionMessage(o)))
    log_debug('Warning output: %s', msg)
    stream(msg, 'stderr')
},

execute = function(request) {
    send_response('execute_input', request, 'iopub', list(
        code = request$content$code,
        execution_count = execution_count))
        
    # Make the current request available to other functions
    current_request <<- request
    # reset ...
    payload <<- list()
    err <<- list()
    
    # shade base::readline
    replace_in_base_namespace('readline', .self$readline)
    
    # shade getPass::getPass
    add_to_user_searchpath('getPass', .self$get_pass)
    
    # shade base::quit
    replace_in_base_namespace('quit', .self$quit)
    replace_in_base_namespace('q', .self$quit)

    # find out stack depth in notebook cell
    # TODO: maybe replace with a single call on first execute and rest reuse the value?
    tryCatch(evaluate(
        'stop()',
        stop_on_error = 1L,
        output_handler = new_output_handler(error = function(e) nframe <<- sys.nframe())))
    
    oh <- if (is_silent()) {
        new_output_handler(
            text = identity,
            graphics = identity,
            message = identity,
            warning = identity,
            error = identity,
            value = identity)
    } else {
        new_output_handler(
            text = function(o) stream(o, 'stdout'),
            graphics = .self$handle_graphics,
            message = .self$handle_message,
            warning = .self$handle_warning,
            error = .self$handle_error,
            value = .self$handle_value)
    }
    
    interrupted <<- FALSE
    last_recorded_plot <<- NULL
    log_debug('Executing code: %s', request$content$code)
    
    warn_unicode_on_windows(request$content$code, .self$send_error_msg)
    
    tryCatch(
        evaluate(
            request$content$code,
            envir = .GlobalEnv,
            output_handler = oh,
            stop_on_error = 1L),
        interrupt = function(cond) interrupted <<- TRUE,
        error = .self$handle_error) # evaluate does not catch errors in parsing
    
    if (!is_silent() && !is.null(last_recorded_plot)) {
        send_plot(last_recorded_plot)
    }
    
    if (interrupted) {
        reply_content <- list(
            status = 'abort')
    } else if (!is.null(err$ename)) {
        reply_content <- c(err, list(
            status = 'error',
            execution_count = execution_count))
    } else {
        reply_content <- list(
            status = 'ok',
            execution_count = execution_count,
            payload = payload,
            user_expressions = namedlist())
    }
    
    send_response('execute_reply', request, 'shell', reply_content)

    if (interrupted || !is.null(err$ename)) {
        # errors or interrupts should interrupt all currently queued messages,
        # not only the currently running one...
        abort_queued_messages()
    }

    if (!is_silent() && should_store_history()) {
        execution_count <<- execution_count + 1L
    }
},

initialize = function(...) {
    execution_count <<- 1L
    err <<- list()
    options(pager = function(files, header, title, delete.file) {
        text <- title
        for (path in files) {
            text <- c(text, header, readLines(path))
        }
        if (delete.file) file.remove(files)
        data <- list('text/plain' = paste(text, collapse = '\n'))
        page(list(data=data, metadata=namedlist()))
    })
    options(jupyter.base_display_func = .self$display_data)
    # Create the shadow env here and detach it finalize
    # so it's available for the whole lifetime of the kernel.
    .BaseNamespaceEnv$attach(NULL, name = 'jupyter:irkernel')

    # Add stuff to the user environment and configure a few options
    # in the current session
    init_shadowenv()
    init_session()
    init_null_device()

    callSuper(...)
},
finalize = function() {
    detach('jupyter:irkernel')
})
)
