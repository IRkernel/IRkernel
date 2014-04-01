Executor = setRefClass("Executor",
            fields=c("execution_count", "userenv", "err", "kernel"),
            methods = list(

execute = function(request) {
  send_response = kernel$send_response
  send_response("status", request, 'iopub', list(execution_state="busy"))
  send_response("pyin", request, 'iopub',
                list(code=request$code, execution_count=execution_count))

  silent = request$content$silent
  
  display  = function(mimetype, content, metadata) {
    if (missing(metadata)) {
        metadata = setNames(list(), character(0))
    }
    data = list()
    data[mimetype] = content
    send_response("display_data", request, 'iopub',
            list(source='R display func', data=data, metadata=metadata)
        )
    invisible(T)
  }
  assign('display', display, pos=userenv)
  
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
  } else {
    handle_value = function (obj) {
        write("value", stderr())
        data = list()
        data['text/plain'] = paste(capture.output(print(obj)), collapse="\n")
        send_response("pyout", request, 'iopub',
                  list(data=data, metadata=setNames(list(), character(0)),
                  execution_count=execution_count))
      }
    stream = function(output, streamname) {
        write("stream", stderr())
        send_response("stream", request, 'iopub',
                      list(name=streamname, data=paste(output, collapse="\n")))
      }
  }
  
  oh = new_output_handler(text=function(o) {stream(o, 'stdout')},
                          message=function(o) {stream(o, 'stderr')},
                          warning=function(o) {stream(o, stderr)},
                          error=handle_error,
                          value=handle_value
                          )

  evaluate(request$content$code, envir=userenv, output_handler=oh,
            stop_on_error=0, new_device=FALSE)
  
  send_response("status", request, 'iopub', list(execution_state="idle"))
  
  if (!is.null(err$ename)) {
    reply_content = c(err, list(status='error', execution_count=execution_count))
  } else {
    reply_content = list(status='ok', execution_count=execution_count,
                  payload=list(), user_variables=list(), user_expressions=list())
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
