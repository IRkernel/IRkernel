context('utils')

test_that('skip_repeated works', {
    stack <- c('foo()', 'f()', 'f()', 'f()', 'f()', 'bar()')
    expect_equal(skip_repeated(stack), c('foo()', 'f()', ellip_h, 'f()', 'bar()'))
})

test_that('skip_repeated does not skip three or less consecutive items', {
    stack <- c('foo()', 'f()', 'f()', 'f()', 'bar()')
    expect_equal(skip_repeated(stack), stack)
})
