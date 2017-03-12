context('kernel')

get_desc <- function(result) {
    if (!is.na(result$desc)) result$desc
    else sub('_', ' ', result$id)
}

a_test_ran <- FALSE

result_to_test <- function(result) {
    a_test_ran <<- TRUE
    
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
    expect_true(file.exists('test_ir.py'), 'test_ir.py exists')
    
    Sys.setenv(PYTHONPATH = 'njr')
    con <- pipe('python3 -W ignore::DeprecationWarning -m ndjson_testrunner test_ir', 'rt')
    on.exit(expect_equal(close(con), 0L))
    
    jsonlite::stream_in(con, result_to_test, pagesize = 1L, verbose = FALSE)
    
    expect_true(a_test_ran, 'at least one python test ran')
})

spec_rm_status <- system2('jupyter', c('kernelspec', 'remove', '-f', 'testir'))
test_that('test kernel removed', expect_equal(spec_rm_status, 0))
