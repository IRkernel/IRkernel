context('kernel')

status <- system2('python3', c('-W', 'ignore::DeprecationWarning', 'test_ir.py'),
                  wait=TRUE)
test_that('kernel tests pass', {
    expect_equal(status, 0)
})
