exec.special = new.env()
assign("counter", 1, envir=exec.special)
userenv = new.env()

execute <- function(request) {
  execution_count = get("counter", envir=exec.special)
  send_response("status", request, 'iopub', list(execution_state="busy"))
  send_response("pyin", request, 'iopub',
                list(code=request$code, execution_count=execution_count))

  silent = request$content$silent
  if (silent) {
    code = request$contents$code
  } else {
    code = sprintf("withVisible({%s})", request$content$code)
  }
  
  err = tryCatch({
    output_conn = textConnection("output", "w")
    sink(output_conn)
    expr = parse(text=code)
    result = eval(expr, envir=userenv)
    list(ename=NULL)  # Result of expression: error status
  }, error = function(e) {
    return(list(ename="ERROR", evalue=toString(e), traceback=list(toString(e))))
  }, finally = {
    sink()
    close(output_conn)
  })

  if (!silent) {
      if (!is.null(err$ename)) {
        send_response("pyerr", request, 'iopub',
                      c(err, list(execution_count=execution_count)))
      } else if (result$visible) {
        data = list()
        data['text/plain'] = capture.output(print(result$value))
        send_response("pyout", request, 'iopub',
                  list(data=data, metadata=list(), execution_count=execution_count))
      }

      if (length(output) > 0) {
        send_response("stream", request, 'iopub',
                      list(name="stdout", data=output))
      }
  }
  
  send_response("status", request, 'iopub', list(execution_state="idle"))
  
  if (!is.null(err$ename)) {
    reply_content = c(err, list(status='error', execution_count=execution_count))
  } else {
    reply_content = list(status='ok', execution_count=execution_count,
                  payload=list(), user_variables=list(), user_expressions=list())
  }
  send_response("execute_reply", request, 'shell', reply_content)
  
  if (!silent) {
    assign("counter", execution_count+1, envir=exec.special)
  }
}
