DO NOT USE. SUPER ALPHA, MUCH UNSAFE, VERY DISASTER. WOW.

# Native R kernel for IPython
code using the IPython interface.

__Installing__

First you'll need the latest version of zmq. Install with homebrew:

```coffee
brew install zmq
# or upgrade
brew update
brew upgrade zmq
```

__Installing dependencies__ (This will all change soon too)

```coffee
install.packages(c("rjson", "uuid", "digest"))
# You'll also need to install the rzmq library
# https://github.com/armstrtw/rzmq

```

# Running the notebook

```coffee
# This is in flux, will change soon
ipython qtconsole --KernelManager.kernel_cmd="['Rscript', 'kernel.r', '{connection_file}']"
```
