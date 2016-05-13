#!/bin/bash -e
# https://github.com/travis-ci/travis-rubies

headline() {
  echo -e "\033[33;1m${1}\033[0m"
}

install_github_dependencies() {
    travis_fold start install_github
    headline "Install github dependencies"
    set -e
    Rscript -e "devtools::install_github('snoweye/pbdZMQ')"
    Rscript -e "devtools::install_github('irkernel/repr')"
    Rscript -e "devtools::install_github('irkernel/IRdisplay')"
    set +e
    travis_fold end install_github
}

install_and_activate_conda_env() {
    travis_fold start install_conda
    headline 'Installing Python Environment for testing'
    set -e
    # http://conda.pydata.org/docs/travis.html
    if [[ "$TRAVIS_OS_NAME" == "osx" ]] ; then os=MacOSX ; else os=Linux ; fi
    wget "https://repo.continuum.io/miniconda/Miniconda3-latest-$os-x86_64.sh" -O miniconda.sh
    bash miniconda.sh -b -p $HOME/miniconda
    export PATH="$HOME/miniconda/bin:$PATH"
    hash -r
    rm miniconda.sh
    conda config --set always_yes yes --set changeps1 no
    conda update -q conda
    #Useful for debugging any issues with conda
    conda info -a
    conda create -q -n test-environment python=3.5 jupyter nose
    source activate test-environment
    pip install jupyter_kernel_test
    set +e
    travis_fold end install_conda
}