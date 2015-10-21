# Native R kernel for Jupyter

For detailed requirements and install instructions see [irkernel.github.io](http://irkernel.github.io/)

## Requirements

* [Jupyter](http://jupyter.org).
* A current [R installation](http://www.r-project.org).

## Installation

We provide Windows and Mac OS X binary packages of all the needed packages:

```r
install.packages(c('rzmq','repr','IRkernel','IRdisplay'),
                 repos = c('http://irkernel.github.io/', getOption('repos')))
IRkernel::installspec()
```

## Running the notebook

If you have Jupyter installed, you can create a notebook using IRkernel from the dropdown menu.

You can also start other interfaces with an R kernel:

```bash
# “ir” is the kernel name installed by the above 'IRkernel::installspec()'
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

On other platforms without docker, this can be started using docker-machine by replacing localhost with an IP from docker-machine ip <MACHINE>. With the deprecated boot2docker, this IP will be boot2docker ip.
