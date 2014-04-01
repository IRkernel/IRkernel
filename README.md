# Native R kernel for IPython

This is still highly experimental and unreliable. Your code should be safe,
since IPython handles saving and loading notebooks in another process, but
you'll lose all your variables if it crashes.

##Installing

First you'll need the latest version of zmq. Install with homebrew:

```Shell
brew install zmq
# or upgrade
brew update
brew upgrade zmq
```

Install the dependencies and the package:

```coffee
install.packages(c("rjson", "uuid", "digest", "evaluate"))
# You'll also need to install rzmq and evaluate from Github for now
# https://github.com/armstrtw/rzmq
# https://github.com/hadley/evaluate
library(devtools)
install_github("armstrtw/rzmq")
install_github("hadley/evaluate")
install_github("takluyver/IRdisplay")
install_github("takluyver/IR_kernel")
```


# Running the notebook

```Shell
ipython notebook --KernelManager.kernel_cmd="['R', '-e', 'ipyr::main(\'{connection_file}\')']"
```

You can also substitute 'qtconsole' or 'console' for 'notebook' in this command.
