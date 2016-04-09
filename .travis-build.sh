#!/bin/bash
ANSI_YELLOW="\033[33;1m"

bootstrap_header() {
  echo -e "${ANSI_YELLOW}$1${ANSI_RESET}"
}

travis_fold start Building
 bootstrap_header 'Building package'
 R CMD build .
travis_fold end Building

travis_fold start Checking
 bootstrap_header 'Checking package' 
 export PKG_TARBALL=$(Rscript -e "with(as.list(read.dcf('DESCRIPTION')[1, ]), cat(sprintf('%s_%s.tar.gz', Package, Version)))")
 R CMD check "$PKG_TARBALL"
travis_fold end Checking

travis_fold start Installing
 bootstrap_header 'Installing package'
 R CMD INSTALL "$PKG_TARBALL"
travis_fold end Installing

travis_fold start Testing
 bootstrap_header 'Testing package'
 Rscript -e "IRkernel::installspec()"
 Rscript -e "IRkernel::installspec(name = 'testir', displayname = 'testir')"
 easy_install3 jupyter_kernel_test
 python3 test_ir.py
travis_fold end Testing