# Native R kernel for Jupyter [![Build Status]][Travis]

[Build Status]: https://travis-ci.org/IRkernel/IRkernel.svg?branch=master
[Travis]: https://travis-ci.org/IRkernel/IRkernel

For detailed requirements and install instructions see [irkernel.github.io](http://irkernel.github.io/)

## Requirements

* [Jupyter](http://jupyter.org).
* A current [R installation](http://www.r-project.org).

## Installation

We provide Windows and Mac OS X binary packages of all the needed packages:

```r
install.packages(c('repr', 'IRkernel', 'IRdisplay'),
                 repos = c('http://irkernel.github.io/', getOption('repos')))
IRkernel::installspec()
```

Per default `IRkernel::installspec()` will install a kernel with the name “ir” and a
display name of “R”. Multiple calls will overwrite the kernel with a kernel spec pointing to the last
R interpreter you called that commands from. You can install kernels for multiple versions of R
by supplying a `name` and `displayname` argument to the `installspec()` call (You still need to
install these packages in all interpreters you want to run as a jupyter kernel!):

```r
# in R 3.2
IRkernel::installspec(name = 'ir32', displayname = 'R 3.2')
# in R 3.1
IRkernel::installspec(name = 'ir31', displayname = 'R 3.1')
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
# “ir” is the kernel name installed by the above 'IRkernel::installspec()'
# change if you used a different name!
jupyter qtconsole --kernel=ir
jupyter console --kernel=ir
```

## Run in a Docker container

If you have a Docker daemon running, e.g. reachable on localhost, start a container with:

```bash
git clone https://github.com/IRkernel/IRkernel.git
cd IRkernel
docker build -t irkernel .
cd <path to your notebooks>
docker run -itp 8888:8888 -v $(pwd):/notebooks/ irkernel
```

In your browser open the URL <http://localhost:8888/>. All notebooks from your session will be saved in the current directory.

On other platforms without docker, this can be started using `docker-machine` by replacing “localhost” with an IP from `docker-machine ip <MACHINE>`. With the deprecated `boot2docker`, this IP will be `boot2docker ip`.
