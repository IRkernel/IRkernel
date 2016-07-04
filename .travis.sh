#!/bin/bash

# NOTE:
# This file uses some functions that have their bodies in subshells: (...)
# This way they can use set -e, so the whole function returns a fail value once a command fails
# Functions that don’t need this and/or export variables use the more common braces: {...}

headline() {
    echo -e "\033[33;1m$1\033[0m"
}

fold_wrap() {
    local header=$1; shift
    local cmd=$1; shift
    
    travis_fold start $cmd
    headline "$header"
    $cmd
    local result=$?
    travis_fold end $cmd
    return $result
}

### BEFORE_SCRIPT ###

install_r_packages() (
    set -ex
    
    # Cairo was missing on OSX, no idea why this wasn't installed :-( Needed for repr
    if [[ "$TRAVIS_OS_NAME" == osx ]]; then
        Rscript -e "install.packages('Cairo')"
    fi
    
    # Also test newer versions so that we catch errors earlier
    if [[ "$DEPS" == github ]]; then
        Rscript -e 'devtools::install_github(c("snoweye/pbdZMQ", "irkernel/repr", "irkernel/IRdisplay"))'
    fi
)

create_conda_environment() (
    set -ex
    
    # http://conda.pydata.org/docs/travis.html
    if [[ "$TRAVIS_OS_NAME" == osx ]]; then os=MacOSX; else os=Linux; fi
    wget "https://repo.continuum.io/miniconda/Miniconda3-latest-$os-x86_64.sh" -O miniconda.sh
    bash miniconda.sh -b -p "$HOME/miniconda"
    rm miniconda.sh # needed to prevent a error in the R CMD check part
    
    hash -r
    conda config --set always_yes yes --set changeps1 no
    conda update -q conda
    # Useful for debugging any issues with conda
    conda info -a
    conda create -q -n test-environment python=3.5 jupyter nose
)

### SCRIPT ###

export_variables() {
    export -p CHECK_LOG="$TRAVIS_BUILD_DIR/IRkernel.Rcheck/00check.log"
    export -p INSTALL_LOG="$TRAVIS_BUILD_DIR/IRkernel.Rcheck/00install.out"
    # Last command → return value. We can’t export from a subshell, so no (set -e; ...)
    export -p PKG_TARBALL=$(Rscript -e "with(as.list(read.dcf('DESCRIPTION')[1, ]), cat(sprintf('%s_%s.tar.gz', Package, Version)))")
}

build_package() {
    R CMD build .  # Last/Only command → return value
}

check_package() (
    set -ex
    
    R CMD check "$PKG_TARBALL" --as-cran
    ! grep -q 'WARNING' "$CHECK_LOG"
    # .. because ': ' was resulting in an replacement by travis and an error
    grep -q 'Status..1 NOTE' "$CHECK_LOG"
    # Code problems are only one note, so make sure that we catch new ones...
    # count the lines with real note sin it... remove leading whitespace because
    # wc -l on OSX has some there...
    LINES=$(grep -v '* .*$' "$CHECK_LOG" | wc -l | sed -e 's/^[[:space:]]*//')
    echo "Lines: $LINES"
    #  4 lines from the "attach" NOTE, 1 lines from the "Status" at the end
    if [[ "_$LINES" != _5 ]]; then grep -v '* .*$' "$CHECK_LOG"; false; fi
)

test_kernel() (
    set -ex
    
    R CMD INSTALL "$PKG_TARBALL"
    Rscript -e 'IRkernel::installspec()'
    Rscript -e 'IRkernel::installspec(name = "testir", displayname = "testir")'
    Rscript -e 'devtools::test()'
)
