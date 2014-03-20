# Native R kernel for IPython

This is still highly experimental and unreliable. Your code should be safe,
since IPython handles saving and loading notebooks in another process, but
ou'll lose all your variables if it crashes.

##Installing

First you'll need the latest version of zmq. Install with homebrew:

```Shell
brew install zmq
# or upgrade
brew update
brew upgrade zmq
```

Install the dependencies:

```R
install.packages(c("rjson", "uuid", "digest"))
# You'll also need to install the rzmq library from Github for now
# https://github.com/armstrtw/rzmq
devtools::install_github("armstrtw/rzmq")
```

And the package itself:

```Shell
# Run this with the directory or tarball:
R CMD INSTALL IR_kernel
```

# Running the notebook

```Shell
ipython notebook --KernelManager.kernel_cmd="['R', '-e', 'library(ipyr); main(\'{connection_file}\')']"
```

You can also substitute 'qtconsole' or 'console' for 'notebook' in this command.
