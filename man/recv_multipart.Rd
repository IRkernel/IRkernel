\name{recv_multipart}
\alias{recv_multipart}
\title{Receive multipart ZMQ message}
\usage{
recv_multipart(socket)
}
\arguments{
  \item{socket}{The ZMQ socket from which to receive data}
}
\description{
Returns a string vector, one element per message part. This
will hopefully become part of rzmq.
}

