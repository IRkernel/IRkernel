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
