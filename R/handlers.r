prepare_mimebundle <- function(obj, handle_display_error = log_error) {
    data <- namedlist()
    metadata <- namedlist()
    
    # we always send text/plain, even if the user removed that from the option!
    data[['text/plain']] <- text_repr <- repr_text(obj)
    
    # Only send a response when there is regular console output
    if (!is.null(text_repr) && nchar(text_repr) > 0L) {
        if (getOption('jupyter.rich_display')) {
            for (mime in getOption('jupyter.display_mimetypes')) {
                if (mime == 'text/plain') {
                    # doing a print(obj) can be expensive and we don't want to do it twice!
                    next
                }
                # Use withCallingHandlers as that shows the inner stacktrace:
                # https://stackoverflow.com/questions/15282471/get-stack-trace-on-trycatched-error-in-r
                # the tryCatch is  still needed to prevent the error from showing
                # up outside further up the stack :-/
                tryCatch(withCallingHandlers({
                    r <- mime2repr[[mime]](obj)
                    if (!is.null(r)) {
                        data[[mime]] <- r
                    }
                }, error = handle_display_error),
                error = function(x) {})
            }
        }
    }
    
    list(data = data, metadata = metadata)
}
