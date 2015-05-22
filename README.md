# Native R kernel for IPython

This is still experimental. Your code should be safe,
since IPython handles saving and loading notebooks in another process, but
you'll lose all your variables if it crashes.

## Requirements

To run IPython with an R kernel, you need at least the following:

* [IPython](http://ipython.org/), version 3.0 or later. If you already have a 
  Python environment set up, install IPython using your preferred tools. If 
  not, installing [Anaconda](http://continuum.io/downloads) is the quickest 
  way to get everything you need. Earlier versions of IPython are now 
  unsupported.
* A current [R installation](http://www.r-project.org/).

Installing from source needs additionally header files (see below).

## Installing via supplied binary packages (Windows + Mac OS X)

We provide Windows and Mac OS X binary packages of all the needed packages. The packages 
and the kernel spec can be installed with the following lines in an R console: 

```r
install.packages(c('rzmq','repr','IRkernel','IRdisplay'),
                 repos = c('http://irkernel.github.io/', getOption('repos')))
IRkernel::installspec()
```

To update packages, you have to either add the repo to your default ones in your R startup 
file and update afterwards as normal:

```r
r <- getOption('repos')
r$IRkernel <- 'http://irkernel.github.io/'
options(repos = r)
```

Or use the `repo` option to `update.packages()` directly:

```r
update.packages(repos = c('http://irkernel.github.io/', getOption('repos')))
```

Please note that during the initial development, these packages can be updated
without changing the version number, which prevents updating via `update.packages()`. 
In that case, updated packages can be installed by re-running the above 
`install.packages()` line.

## Installing from source (Default on Linux, but possible on all platforms)

### ZMQ

You'll need zmq development headers to compile rzmq.

*   **Ubuntu/Debian**

    ```bash
    sudo apt-get install libzmq3-dev
    ```

*   **Homebrew**

    ```bash
    brew install zmq
    # or upgrade
    brew update
    brew upgrade zmq
    ```

*   **MacPorts**

    1.  make sure an [X server is installed](http://xquartz.macosforge.org/),
        open a terminal and do the following:

        ```bash
        sudo port install zmq
        ```

    2.  Direct the compiler to use MacPorts libraries using:

        ```bash
        export CPATH=/opt/local/include
        export LIBRARY_PATH=/opt/local/lib
        ```

*   **Windows**

    See [this bugreport](https://github.com/IRkernel/IRkernel/issues/54#issuecomment-84467798)
    for instruction on how to compile on windows.

### R

Start `R` in the same terminal, and proceed as below.

You can install snapshot packages and the kernel spec via

```r
install.packages(c('rzmq','repr','IRkernel','IRdisplay'),
                 repos = c('http://irkernel.github.io/', getOption('repos')),
                 type = 'source')
IRkernel::installspec()
```

To update your source installation, repeat the `install.packages` step.

### Development Version

If you want to compile the latest and greatest (and maybe buggiest…) from git, 
the easiest way is via the `devtools` package.

On Ubuntu/Debian, a header package is needed to compile RCurl:

```bash
sudo apt-get install libcurl4-openssl-dev
```

Start a R session and run:

```r
install.packages('devtools')
# Need RCurl for install_github
install.packages('RCurl')
library(devtools)
# Install the packages
install_github('IRkernel/repr')
install_github('IRkernel/IRdisplay')
install_github('IRkernel/IRkernel')
# Install the kernel spec
IRkernel::installspec()
```

To update the git versions, repeat the `install_github('IRkernel/...')` steps.

## Running the notebook

If you have IPython 3.0 installed, you can create a notebook and switch to
IRkernel from the dropdown menu. 

You can also start a `qtconsole` with an R kernel:

```bash
# “ir” is the kernel name installed by the above 'IRkernel::installspec()'
ipython qtconsole --kernel=ir
```

You can also substitute `console` for `qtconsole` in this command.
