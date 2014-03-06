# Native R kernel for IPython

__Installing__


__Installing dependencies__

```coffee
install.packages(c("rjson", "uuid", "digest"))
# You'll also need to install the rzmq library
# https://github.com/armstrtw/rzmq

```

__Dependencies for `rzmq`__

```coffee
git clone git://github.com/zeromq/libzmq.git
cd libzmq
./autogen.sh
./configure     # add other options here
make
make check
sudo make install
```
libzmq also requires `pkg-config`. See more info on getting libzmq set up [here](http://zeromq.org/docs:source-git).

