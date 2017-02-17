## Release summary

This package is new on CRAN. It contains the R kernel for the Jupyter
ecosystem. The kernel executes R code, which the frontend (Jupyter Notebook or
other frontends) submits to the kernel via the network.

## Test environments

* local Win7 64bit install, R 3.2.5, R 3.3.0 and r-devel (3.4)
* Ubuntu 12.04 (on travis-ci), oldrelease, release, and r-devel

## R CMD check results

```r
Found the following (possibly) invalid URLs:
  URL: http://localhost:8888/
    From: README.md
    Status: Error
    Message: libcurl Fehlercode 7
    \tFailed to connect to localhost port 8888: Verbindungsaufbau abgelehnt

Status: 1 NOTE
```

This arises from the fact that a common usage of this package involves starting a
server on localhost. As expected this URL is only reachable after starting it.

## Downstream dependencies

It is assumed that there won't be any code dependencies on this package, as it
implements an application without any API apart from the startup function and the
implemented Jupyter Messaging spec. It's usually started as
`R --slave -e IRkernel::main() --args {connection_file}`.
