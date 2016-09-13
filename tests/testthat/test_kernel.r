context('kernel')

get_desc <- function(result) {
    if (!is.na(result$desc)) result$desc
    else sub('_', ' ', result$id)
}

result_to_test <- function(result) {
    emit_result <- switch(result$type,
        success            = succeed,
        expected_failure   = succeed,
        failure            = fail,
        error              = stop,
        unexpected_success = fail,
        skip               = skip,
        stop('Unknown test result type: ', result$type))
    
    msg <- if (!is.na(result$msg)) result$msg else '<no description>'
    
    test_that(get_desc(result), emit_result(msg))
}

spec_add_status <- installspec(name = 'testir', displayname = 'testir')
test_that('test kernel installed', expect_equal(spec_add_status, 0))

test_that('kernel tests pass', {
    con <- pipe('python3 -W ignore::DeprecationWarning -m ndjson_testrunner test_ir', 'rt')
    on.exit(close(con))
    
    jsonlite::stream_in(con, result_to_test, pagesize = 1L, verbose = FALSE)
})

spec_rm_status <- system2('jupyter', c('kernelspec', 'remove', '-f', 'testir'))
test_that('test kernel removed', expect_equal(spec_rm_status, 0))
