\name{send_multipart}
\alias{send_multipart}
\title{Send multipart ZMQ message.}
\usage{
send_multipart(socket, parts)
}
\arguments{
  \item{socket}{The ZMQ socket on which to send data}

  \item{parts}{A string vector; each element will be sent
  as one part of the message}
}
\description{
This will hopefully become part of rzmq.
}

