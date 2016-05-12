# Everthing related to the environment which takes functions which shadow base R functions.
# This is needed to build in our own needs, like properly shutting down the kernel
# when `quit()` is called.

# The real env is created and attach'ed in the main() via the call to init_shadowenv()!
init_shadowenv <- function(){
    .irk.shadowenv <- attach(NULL, name = "jupyter:irkernel")

    add_to_user_searchpath <- function(name, FN, attrs = list()) {
        assign(name, FN, .irk.shadowenv)
    }

    # add the accessors to the shadow env to that env, so they are actually accessable 
    # from everywhere...
    add_to_user_searchpath(".irk.get_shadowenv", function() {.irk.shadowenv})
    add_to_user_searchpath(".irk.add_to_user_searchpath", add_to_user_searchpath)
    
}
