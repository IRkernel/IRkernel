This is an R kernel for [IPython](http://ipython.org/), so that you can run R
code using the IPython interface.

To use it, run:

```
ipython qtconsole --KernelManager.kernel_cmd="['Rscript', '/path/to/ir_kernel/kernel.r', '{connection_file}']"
```

You can replace 'qtconsole' with 'console' or 'notebook'.
