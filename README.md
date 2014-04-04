# Native R kernel for IPython

This is still highly experimental and unreliable. Your code should be safe,
since IPython handles saving and loading notebooks in another process, but
you'll lose all your variables if it crashes.

##Installing

You'll need zmq development headers to compile rzmq. Install this, e.g. with apt:

```Shell
apt-get install libzmq3-dev
```

or with homebrew:

```Shell
brew install zmq
# or upgrade
brew update
brew upgrade zmq
```

Install the dependencies and the package:

```coffee
library(devtools)
install_github("armstrtw/rzmq")
install_github("hadley/evaluate")
install_github("jeroenooms/jsonlite")
install_github("takluyver/IRdisplay")
install_github("takluyver/IRkernel")
```

You'll also need [IPython](http://ipython.org/). If you already have a Python
environment set up, install IPython using your preferred tools. If not, installing
[Anaconda](http://continuum.io/downloads) is the quickest way to get everything
you need.

# Running the notebook

```Shell
ipython notebook --KernelManager.kernel_cmd="['R', '-e', 'IRkernel::main(\'{connection_file}\')']"
```

You can also substitute 'qtconsole' or 'console' for 'notebook' in this command.
