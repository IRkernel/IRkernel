context('kernel')

test_con <- pipe('python3 test_runner.py', 'rb', 'utf-8')
if (!isOpen(test_con)) open(test_con)

while(isOpen(test_con)) {
    test_result <- readBin(test_con, 'character', 1L)
    if (length(test_result) == 0) break 
    
    parts <- strsplit(test_result, '\n')[[1]]
    if (length(parts) < 2) {
        test_that('test output parsing', fail(sprintf('needs 2 parts, but only has one: %s', dQuote(parts))))
        next
    }
    
    name   <- parts[[1]]
    status <- parts[[2]]
    msg    <- paste(parts[-(1:2)], collapse = '\n')
    
    if (nchar(msg) == 0)
        msg <- status
    
    test_that(name, switch(
        status,
        ok =, 'expected failure' = succeed('Success!'),
        failure =, error =, 'unexpected success' = fail(msg),
        skipped = skip(msg),
        stop('unknown status: ', status)))
}
