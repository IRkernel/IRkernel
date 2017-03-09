# Native R kernel for Jupyter [![Build Status]][Travis]

[Build Status]: https://travis-ci.org/IRkernel/IRkernel.svg?branch=master
[Travis]: https://travis-ci.org/IRkernel/IRkernel

For detailed requirements and install instructions see [irkernel.github.io](http://irkernel.github.io/)

## Requirements

* [Jupyter](http://jupyter.org).
* A current [R installation](https://www.R-project.org).

## Installation

We will soon submit the IRkernel package to CRAN. Until then, you can install it via the `devtools` package:

```R
install.packages('devtools')
devtools::install_github('IRkernel/IRkernel')
# or devtools::install_local('IRkernel-master.tar.gz')
IRkernel::installspec()  # to register the kernel in the current R installation
```

Per default `IRkernel::installspec()` will install a kernel with the name “ir” and a
display name of “R”. Multiple calls will overwrite the kernel with a kernel spec pointing to the last
R interpreter you called that commands from. You can install kernels for multiple versions of R
by supplying a `name` and `displayname` argument to the `installspec()` call (You still need to
install these packages in all interpreters you want to run as a jupyter kernel!):

```r
# in R 3.3
IRkernel::installspec(name = 'ir33', displayname = 'R 3.3')
# in R 3.2
IRkernel::installspec(name = 'ir32', displayname = 'R 3.2')
```

Now both R versions are available as an R kernel in the notebook.

### If you encounter problems during installation

1. Have a look at the [full installation instructions](http://irkernel.github.io/installation/)!
2. [Search the existing open and closed issues](https://github.com/IRkernel/IRkernel/issues?utf8=%E2%9C%93&q=is%3Aissue).
3. If you are sure that this is a new problem, [file an issue](https://github.com/IRkernel/IRkernel/issues/new).

## Running the notebook

If you have Jupyter installed, you can create a notebook using IRkernel from the dropdown menu.

You can also start other interfaces with an R kernel:

```bash
# “ir” is the kernel name installed by the above `IRkernel::installspec()`
# change if you used a different name!
jupyter qtconsole --kernel=ir
jupyter console --kernel=ir
```

## Run a stable release in a Docker container

Refer to the [jupyter/docker-stacks r-notebook](https://github.com/jupyter/docker-stacks/tree/master/r-notebook) repository

If you have a Docker daemon running, e.g. reachable on localhost, start a container with:

```bash
docker run -d -p 8888:8888 jupyter/r-notebook
```

Open localhost:8888 in your browser. All notebooks from your session will be saved in the current directory.

On other platforms without docker, this can be started using `docker-machine` by replacing “localhost” with an IP from `docker-machine ip <MACHINE>`. With the deprecated `boot2docker`, this IP will be `boot2docker ip`.

## Develop and run from source in a Docker container

```bash
make docker_dev_image #builds dev image and installs IRkernel dependencies from github
make docker_dev #mounts source, installs, and runs Jupyter notebook; docker_dev_image is a prerequisite
make docker_test #builds the package from source then runs the tests via R CMD check; docker_dev_image is a prerequisite
```
