UNICODE_WARNING <-
'Your code contains a unicode char which cannot be displayed in your
current locale and R will silently convert it to an escaped form when the
R kernel executes this code. This can lead to subtle errors if you use
such chars to do comparisons. For more information, please see
https://github.com/IRkernel/repr/wiki/Problems-with-unicode-on-windows'

#' @importFrom utils capture.output
warn_unicode_on_windows <- function(code, warn) {
    # Workaround to warn user when code contains potential problematic code
    # https://github.com/IRkernel/repr/issues/28#issuecomment-208810772
    # See https://github.com/hadley/evaluate/issues/66
    if (.Platform$OS.type == 'windows') {
        # strip whitespace, because trailing newlines would trip the test...
        code <- gsub('^\\s+|\\s+$', '', code)
        real_len <- nchar(code)
        r_len <- nchar(paste(capture.output(cat(code)), collapse = '\n'))
        if (real_len != r_len) warn(UNICODE_WARNING)
    }
}


DEVICE_ERR <-
"Your R installation is misconfigured:
getOption('bitmapType') == '%s', but capabilities('%s') == FALSE.
This means that the png() device is unable to work.
Please %s."

preflight_check <- function() {
    bitmap_type <- getOption('bitmapType')
    xlib  <- capabilities('X11')
    cairo <- capabilities('cairo')
    
    insane_xlib  <- bitmap_type == 'Xlib'  && !xlib
    insane_cairo <- bitmap_type == 'cairo' && !cairo
    if (insane_xlib || insane_cairo) {
        help_msg <-
            if (!xlib && !cairo) 'install an R that is configured to have either Xlib or cairo support'
            else sprintf("put `options(bitmapType='%s')` into your ~/.Rprofile", if (xlib) 'Xlib' else 'cairo')
        stop(sprintf(DEVICE_ERR, bitmap_type, if (insane_xlib) 'X11' else 'cairo', help_msg))
    }
}
