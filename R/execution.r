displayenv = environment(display)

lappend <- function(lst, obj) {
    # I hope this isn't the best way to do this.
    lst[[length(lst)+1]] = obj
    return(lst)
}

Executor = setRefClass("Executor",
            fields=c("execution_count", "userenv", "payload", "err", "interrupted", "kernel"),
            methods = list(

execute = function(request) {
  send_response = kernel$send_response
  send_response("status", request, 'iopub', list(execution_state="busy"))
  send_response("pyin", request, 'iopub',
                list(code=request$code, execution_count=execution_count))

  silent = request$content$silent
  
  display  = function(data, metadata=NULL) {
    if (is.null(metadata)) {
        metadata = setNames(list(), character(0))
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
  } else {
    handle_value = function (obj) {
        data = list()
        data['text/plain'] = paste(capture.output(print(obj)), collapse="\n")
        send_response("pyout", request, 'iopub',
                  list(data=data, metadata=setNames(list(), character(0)),
                  execution_count=execution_count))
    }
    stream = function(output, streamname) {
        send_response("stream", request, 'iopub',
                      list(name=streamname, data=paste(output, collapse="\n")))
    }
    handle_graphics = function(plotobj) {
        tf = tempfile(fileext='.png')
        png(tf)
        replayPlot(plotobj)
        dev.off()
        display_png(filename=tf)
    }
  }
  
  oh = new_output_handler(text=function(o) {stream(o, 'stdout')},
                          graphics=handle_graphics,
                          message=function(o) {stream(o, 'stderr')},
                          warning=function(o) {stream(o, 'stderr')},
                          error=handle_error,
                          value=handle_value
                          )

  interrupted <<- FALSE
  tryCatch(
    evaluate(request$content$code, envir=userenv, output_handler=oh,
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
                  payload=payload, user_variables=list(), user_expressions=list())
  }
  send_response("execute_reply", request, 'shell', reply_content)
  
  if (!silent) {
    execution_count <<- execution_count + 1
  }
},

initialize = function(...) {
    execution_count <<- 1
    userenv <<- new.env()
    err <<- list()
    callSuper(...)
})
)
