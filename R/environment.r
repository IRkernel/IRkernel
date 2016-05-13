# Everthing related to the environment which takes functions which shadow base R functions.
# This is needed to build in our own needs, like properly shutting down the kernel
# when `quit()` is called.

add_to_user_searchpath <- function(name, FN) {
    assign(name, FN, 'jupyter:irkernel')
}

get_shadowenv <- function() {
    as.environment('jupyter:irkernel')
}

# Adds functions which do not need any access to the executer into the users searchpath
init_shadowenv <- function() {
    # add the accessors to the shadow env itself, so they are actually accessable 
    # from everywhere...
    add_to_user_searchpath('.irk.get_shadowenv', get_shadowenv)
    add_to_user_searchpath('.irk.add_to_user_searchpath', add_to_user_searchpath)

    # workaround for problems with vignette(xxx) not bringing up the vignette
    # content in the browser: https://github.com/IRkernel/IRkernel/issues/267
    add_to_user_searchpath('print.vignette', function(...) {
        utils:::print.vignette(...)
        # returning immediately will run into trouble with zmq and its polling
        # preventing the vignette server to startup. So wait a little to let
        # it startup...
        # 0.1 is too little, so add some margin...
        Sys.sleep(0.5)
    })
}
