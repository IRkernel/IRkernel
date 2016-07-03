context('default options')

test_that('default options are set', {
    expect_true(getOption('jupyter.rich_display'))
    expect_equal(getOption('jupyter.log_level'), 1L)
    expect_equal(getOption('jupyter.logfile'), NA)
    expect_equal(getOption('jupyter.pager_classes'), c(
        'packageIQR',
        'help_files_with_topic'))
    expect_equal(getOption('jupyter.plot_mimetypes'), c(
        'text/plain',
        'image/png'))
    # this is not a kernel itself, only the loaded package!
    expect_equal(getOption('jupyter.in_kernel'), FALSE)
})

