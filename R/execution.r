displayenv = environment(display)

plot_options=new.env() # environment for storing plot-options

#'Set options for plotting
#'
#'IRkernel displays plots in the notebook with calls to png().
#'This function allows to set the variables that will be passed on to
#'png(), for example width or height, see help(png).
#' @param ... options that will be passed to  png()
#' @export
set_plot_options <- function(...){
    options <- list(...)
    for(opt in names(options)){
        assign( opt, options[[opt]], plot_options )
    }
}

#'Get options for plotting
#'
#'Use set_plot_options() for modifying.
#' @export
get_plot_options <- function(){
    return(as.list(plot_options))
}

lappend <- function(lst, obj) {
    # I hope this isn't the best way to do this.
    lst[[length(lst)+1]] = obj
    return(lst)
}
namedlist <- function() {
    # create an empty named list
    return(setNames(list(), character(0)))
}

Executor = setRefClass("Executor",
            fields=c("execution_count", "payload", "err", "interrupted", "kernel"),
            methods = list(

execute = function(request) {
  send_response = kernel$send_response
  send_response("status", request, 'iopub', list(execution_state="busy"))
  send_response("pyin", request, 'iopub',
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

  err <<- list()
  
  
  handle_error = function(e) {
    err <<- list(ename="ERROR", evalue=toString(e), traceback=list(toString(e)))
    if (!silent) {
      send_response("pyerr", request, 'iopub',
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
    handle_value = function (obj) {
        data = list()
        data['text/plain'] = paste(capture.output(print(obj)), collapse="\n")
        send_response("pyout", request, 'iopub',
                  list(data=data, metadata=namedlist(),
                  execution_count=execution_count))
    }
    stream = function(output, streamname) {
        send_response("stream", request, 'iopub',
                      list(name=streamname, data=paste(output, collapse="\n")))
    }
    handle_graphics = function(plotobj) {
        tf = tempfile(fileext='.png')
        do.call(png, c(list(filename=tf), get_plot_options()))
        replayPlot(plotobj)
        dev.off()
        display_png(filename=tf)
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
  
  tryCatch(
    evaluate(request$content$code, envir=.GlobalEnv, output_handler=oh,
                stop_on_error=0),
        interrupt = function(cond) {interrupted <<- TRUE},
        error = handle_error  # evaluate does not catch errors in parsing
    )

  
  send_response("status", request, 'iopub', list(execution_state="idle"))
  
  if (interrupted) {
    reply_content = list(status='abort')
  } else if (!is.null(err$ename)) {
    reply_content = c(err, list(status='error', execution_count=execution_count))
  } else {
    reply_content = list(status='ok', execution_count=execution_count,
          payload=payload, user_variables=namedlist(), user_expressions=namedlist())
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
