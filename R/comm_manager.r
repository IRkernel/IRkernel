#' The CommManager
#'
#' Has methods able to register comms/targets and process comm messages
#'
#' @include logging.r class_unions.r
#' @export
CommManager <- setRefClass(
    'CommManager',
    fields = list(
        send_response = 'function',
        target_to_handler_map = 'list',
        commid_to_comm = 'list',
        parent_request = 'list'
    ),
    methods = list(
        new_comm = function(target_name, comm_id = UUIDgenerate()) {
            Comm$new(id = comm_id, target_name = target_name, comm_manager = .self)
        },
        register_target   = function(target_name, handler_func) target_to_handler_map[[target_name]] <<- handler_func,
        unregister_target = function(target_name, handler_func) target_to_handler_map[[target_name]] <<- NULL,
        register_comm   = function(comm) commid_to_comm[[comm$id]] <<- comm,
        unregister_comm = function(comm) commid_to_comm[[comm$id]] <<- NULL,
        is_comm_registered = function(comm) !is.null(commid_to_comm[[comm$id]]),
        send_open = function(comm_id, target_name, data, metadata = list()) {
            send_response('comm_open', parent_request, 'iopub', list(
                metadata = metadata,
                comm_id = comm_id,
                target_name = target_name,
                data = data
            ))
        },
        send_msg = function(comm_id, target_name, data, metadata = list()) {
            send_response('comm_msg', parent_request, 'iopub', list(
                metadata = metadata,
                comm_id = comm_id,
                target_name = target_name,
                data = data
            ))
        },
        send_close = function(comm_id, target_name, data, metadata = list()) {
            send_response('comm_close', parent_request, 'iopub', list(
                metadata = metadata,
                comm_id = comm_id,
                target_name = target_name,
                data = data
            ))
        },
        make_comm_list = function(comm_list) {
            all_comms <- list()
            for (the_comm in comm_list) {
                all_comms[[the_comm$id]] <- list(target_name = the_comm$target_name)
            }
            all_comms
        },
        #response:
        #content = {
        #    # A dictionary of the comms, indexed by uuids.
        #    'comms': {
        #        comm_id_1: {
        #           'target_name': str,
        #        },
        #        comm_id_2: {
        #           'target_name': str,
        #        },
        #    },
        #}
        on_comm_info_request = function(request) {
            #If request$content$target_name is provided return all commids registered with that target_name
            #Else target_name not in the request return all commids accross all targets
            reply_msg <- list()
            comms <- list()
            if ('target_name' %in% names(request$content)) {
                #reply with comms only for the specified target_name
                target_name_requested <- request$content$target_name
                filtered_comms <- Filter(function(x) x$target_name == target_name_requested, commid_to_comm)
                comms[['comms']] <- make_comm_list(filtered_comms)
            } else {
                comms[['comms']] <- make_comm_list(commid_to_comm)
            }
            reply_msg[['content']] <- comms
            send_response('comm_info_reply', request, 'shell', reply_msg)
        },
        #{
        #   'comm_id' : 'u-u-i-d',
        #   'target_name' : 'my_comm',
        #   'data' : {}
        # }
        on_comm_open = function(request) {
            parent_request <<- request
            target_name <- request$content$target_name
            comm_id <- request$content$comm_id
            
            if (target_name %in% names(target_to_handler_map)) {
                # create a comm object
                comm <- new_comm(target_name, comm_id)
                register_comm(comm)
                data <- request$content$data
                
                #invoke target handler
                tryCatch({
                    target_to_handler_map[[target_name]](comm, data)
                }, error = function(e) {
                    log_debug('error invoking the handler for target: %s', e)
                })
            } else {
                log_debug('target_name not found in comm_open')
                #reply with a comm_close message as target_name not found
                send_close(comm_id, target_name, list())
            }
        },
        #{
        #  'comm_id' : 'u-u-i-d',
        #  'data' : {}
        #}
        on_comm_msg = function(request) {
            parent_request <<- request
            comm_id <- request$content$comm_id
            
            if (comm_id %in% names(commid_to_comm)) {
                comm <- commid_to_comm[[comm_id]]
                
                data <- request$content$data
                tryCatch({
                    comm$handle_msg(data)
                }, error = function(e) {
                    log_debug('error invoking comm handle msg: %s', e)
                })
            } else {
                log_debug('comm_id not found in comm_msg')
            }
        },
        
        #{
        #  'comm_id' : 'u-u-i-d',
        #  'data' : {}
        #}
        on_comm_close = function(request) {
            parent_request <<- request
            comm_id <- request$content$comm_id
            
            if (comm_id %in% names(commid_to_comm)) {
                comm <- commid_to_comm[[comm_id]]
                tryCatch({
                    comm$handle_close()
                }, error = function(e) {
                    log_debug('error invoking comm handle close: %s', e)
                })
                unregister_comm(comm)
            } else {
                log_debug('comm_id not found in comm_msg')
            }
        },
        initialize = function(...) {
            callSuper(...)
        }
    )
)

#' The Comm
#'
#' Has methods able to register and handle message callbacks
#'
#' @export
Comm <- setRefClass(
    'Comm',
    fields = list(
        id = 'character',
        target_name = 'character',
        comm_manager = 'CommManager',
        msg_callback = 'functionOrNULL',
        close_callback = 'functionOrNULL'
    ),
    methods = list(
        open = function(msg = list()) {
            if (!comm_manager$is_comm_registered(.self)) {
                comm_manager$register_comm(.self)
                comm_manager$send_open(id, target_name, msg)
            } else {
                log_debug('Comm already opened!')
            }
        },
        send = function(msg = list()) {
            if (comm_manager$is_comm_registered(.self)) {
                comm_manager$send_msg(id, target_name, msg)
            } else {
                log_debug('Comm is not opened. Cannot send!')
            }
        },
        close = function(msg = list()) {
            if (comm_manager$is_comm_registered(.self)) {
                comm_manager$send_close(id, target_name, msg)
                comm_manager$unregister_comm(.self)
            } else {
                log_debug('Comm is already closed!')
            }
        },
        on_msg = function(a_msg_callback) {
            msg_callback <<- a_msg_callback
        },
        on_close = function(a_close_callback) {
            close_callback <<- a_close_callback
        },
        handle_msg = function(msg) {
            if (!is.null(msg_callback)) {
                msg_callback(msg)
            }
        },
        handle_close = function() {
            if (!is.null(close_callback)) {
                close_callback()
            }
        }
    )
)
