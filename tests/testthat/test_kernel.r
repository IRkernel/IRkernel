context('kernel')

test_that('kernel tests pass', {
    spec_add_status <- installspec(name = 'testir', displayname = 'testir')
    expect_equal(spec_add_status, 0)
    
    # adding the kernel succeeded, so delay all expect_* until after removing the spec.
    test_status    <- system2('python3', c('-W', 'ignore::DeprecationWarning', 'test_ir.py'), wait = TRUE)
    spec_rm_status <- system2('jupyter', c('kernelspec', 'remove', '-f', 'testir'))
    
    expect_equal(test_status,    0)
    expect_equal(spec_rm_status, 0)
})
