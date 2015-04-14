#' @include options.R
NULL

displayenv = environment(display)

lappend <- function(lst, obj) {
    # I hope this isn't the best way to do this.
    lst[[length(lst)+1]] = obj
    return(lst)
}
namedlist <- function() {
    # create an empty named list
    return(setNames(list(), character(0)))
}

plot_builds_upon <- function(prev, current) {
    if (is.null(prev)) {
        return(TRUE)
    }
    lprev = length(prev[[1]])
    lcurrent = length(current[[1]])
    return((lcurrent >= lprev) && (identical(current[[1]][1:lprev], prev[[1]][1:lprev])))
}

# needed to easily encode reprs as json.
setOldClass('repr')
asJSON <- jsonlite:::asJSON
setMethod('asJSON', 'repr', function(x, ...) jsonlite:::asJSON(structure(x, class = NULL, repr.format = NULL), ...))

Executor = setRefClass("Executor",
            fields=c("execution_count", "payload", "err", "interrupted", "kernel",
                     "last_recorded_plot"),
            methods = list(

execute = function(request) {
  send_response = kernel$send_response
  send_response("status", request, 'iopub', list(execution_state="busy"))
  send_response("execute_input", request, 'iopub',
                list(code=request$code, execution_count=execution_count))

  silent = request$content$silent
  
  display  = function(data, metadata=NULL) {
    if (is.null(metadata)) {
        metadata = namedlist()
    }
    send_response("display_data", request, 'iopub',
            list(source='R display func', data=data, metadata=metadata)
        )
    invisible(T)
  }
  
  # Push the display function into the IRdisplay namespace
  # This looks awkward, but we do need to get a reference to the execution
  # state into a global environment.
  unlockBinding("base_display", displayenv)
  assign('base_display', display, pos=displayenv)
  
  payload <<- list()

  options(pager=function(files, header, title, delete.file) {
    text=title
    for (path in files) {
        text = c(text, header, readLines(path))
    }
    if (delete.file) file.remove(files)
    payload <<- lappend(payload, list(source='page', text=paste(text, collapse="\n")))
  })

  send_plot <- function(plotobj) {
      params <- list()
      for (mime in getOption('jupyter.plot_mimetypes')) {
          params[[mime]] <- mime2repr[[mime]](plotobj)
      }
      display(params)
  }

  err <<- list()
  
  
  handle_error = function(e) {
    err <<- list(ename="ERROR", evalue=toString(e), traceback=list(toString(e)))
    if (!silent) {
      send_response("error", request, 'iopub',
                    c(err, list(execution_count=execution_count)))
    }
  }
  if (silent) {
    stream = function(s, n) {}
    handle_value = identity
    handle_graphics = identity
    handle_message = identity
    handle_warning = identity
  } else {
    handle_value <- function (obj) {
        data <- list()
        if (getOption('jupyter.rich_display')) {
            for (mime in getOption('jupyter.result_mimetypes')) {
                r <- mime2repr[[mime]](obj)
                if (!is.null(r)) data[[mime]] <- r
            }
        } else {
            data[['text/plain']] <- repr_text(obj)
        } 
        send_response('execute_result', request, 'iopub', list(
            data = data,
            metadata = namedlist(),
            execution_count = execution_count))
    }
    stream = function(output, streamname) {
        send_response("stream", request, 'iopub',
                      list(name=streamname, text=paste(output, collapse="\n")))
    }
    handle_graphics = function(plotobj) {
        if (!plot_builds_upon(last_recorded_plot, plotobj)) {
            send_plot(last_recorded_plot)
        }
        last_recorded_plot <<- plotobj
    }
    handle_message = function(o){
      stream(paste(o$message, collapse = ''), 'stderr')
    }
    handle_warning = function(o){
      call = if (is.null(o$call)) '' else {
       call = deparse(o$call)[1]
       paste('In', call)
      }
    stream(sprintf('Warning message:\n%s: %s', call, o$message), 'stderr')
    }
  }
  
  oh = new_output_handler(text=function(o) {stream(o, 'stdout')},
                          graphics = handle_graphics,
                          message = handle_message,
                          warning = handle_warning,
                          error = handle_error,
                          value = handle_value
                          )

  interrupted <<- FALSE
  last_recorded_plot <<- NULL
  
  tryCatch(
    evaluate(request$content$code, envir=.GlobalEnv, output_handler=oh,
                stop_on_error=0),
        interrupt = function(cond) {interrupted <<- TRUE},
        error = handle_error  # evaluate does not catch errors in parsing
    )

  if ((!silent) & (!is.null(last_recorded_plot))) {
      send_plot(last_recorded_plot)
  }
  
  send_response("status", request, 'iopub', list(execution_state="idle"))
  
  if (interrupted) {
    reply_content = list(status='abort')
  } else if (!is.null(err$ename)) {
    reply_content = c(err, list(status='error', execution_count=execution_count))
  } else {
    reply_content = list(status='ok', execution_count=execution_count,
          payload=payload, user_expressions=namedlist())
  }
  send_response("execute_reply", request, 'shell', reply_content)
  
  if (!silent) {
    execution_count <<- execution_count + 1
  }
},

initialize = function(...) {
    execution_count <<- 1
    err <<- list()
    callSuper(...)
})
)
