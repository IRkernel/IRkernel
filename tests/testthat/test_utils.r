context('utils')

test_that('skip_repeated works', {
    stack <- c('foo()', 'f()', 'f()', 'f()', 'f()', 'bar()')
    expect_equal(skip_repeated(stack), c('foo()', 'f()', ellip_h, 'f()', 'bar()'))
})

test_that('skip_repeated does not skip three or less consecutive items', {
    stack <- c('foo()', 'f()', 'f()', 'f()', 'bar()')
    expect_equal(skip_repeated(stack), stack)
})

test_that('skip_repeated works on tracebacks', {
    err <- evaluate::try_capture_stack(quote({
        f <- function(x) stop(x)
        f(1)
    }), new.env())
    skipped_stack <- skip_repeated(err$calls)
    expect_type(skipped_stack[[1]], 'language')
})
