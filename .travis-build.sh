#!/bin/bash  
# error on first problem
set -e 
ANSI_YELLOW="\033[33;1m"

bootstrap_header() {
  echo -e "${ANSI_YELLOW}$1${ANSI_RESET}"
}

if [ "${DEPS}" == "github" ]; then
 travis_fold start github_dependencies
  bootstrap_header 'Installing additional github dependencies'
  Rscript -e "devtools::install_github('irkernel/repr')"
  Rscript -e "devtools::install_github('irkernel/IRdisplay')"
 travis_fold end github_dependencies
fi

travis_fold start Building
 bootstrap_header 'Building package'
 R CMD build .
travis_fold end Building

travis_fold start Checking
 bootstrap_header 'Checking package' 
 export PKG_TARBALL=$(Rscript -e "with(as.list(read.dcf('DESCRIPTION')[1, ]), cat(sprintf('%s_%s.tar.gz', Package, Version)))")
 R CMD check $PKG_TARBALL # could also use --as-cran
 export CHECK_LOG=/home/travis/build/IRkernel/IRkernel/IRkernel.Rcheck/00check.log
travis_fold end Checking

travis_fold start Installing
 bootstrap_header 'Installing package'
 R CMD INSTALL "$PKG_TARBALL"
travis_fold end Installing

travis_fold start Python_env
 bootstrap_header 'Installing Python Environment for testing'
 # http://conda.pydata.org/docs/travis.html
 wget https://repo.continuum.io/miniconda/Miniconda-latest-Linux-x86_64.sh -O miniconda.sh;
 bash miniconda.sh -b -p $HOME/miniconda
 export PATH="$HOME/miniconda/bin:$PATH"
 hash -r
 conda config --set always_yes yes --set changeps1 no
 conda update -q conda
 #Useful for debugging any issues with conda
 conda info -a
 # Replace dep1 dep2 ... with your dependencies
 conda create -q -n test-environment python=3.5 jupyter nose
 source activate test-environment
 pip install jupyter_kernel_test
travis_fold end Python_env

travis_fold start Testing
 bootstrap_header 'Testing package'
 Rscript -e "IRkernel::installspec()"
 Rscript -e "IRkernel::installspec(name = 'testir', displayname = 'testir')"
 python -W ignore::DeprecationWarning test_ir.py
travis_fold end Testing
set +e