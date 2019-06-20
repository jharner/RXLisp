\name{[[.XLispReference}
\alias{[[.XLispReference}
%\alias{[[<-.XLispReference}
\alias{$.XLispReference}
\title{Syntactic methods for XLisp Reference Objects}
\description{
  These methods provide a convenient way to
  invoke methods and access slots in XLisp objects
  from within R.
  The subsetting operator  invokes the XLisp \code{send} function
  on the XLisp object associated with the R reference
  object.
  The slot accessor (\code{$}) merely creates a function
  that, when called, will invoke the \code{send} function.
}
\usage{
"[[.XLispReference"(x, i, ...)
"$.XLispReference"(x, name)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{the XLispReference object on which to operate.}
  \item{i}{the name of the slot to be accessed (either queried or set).}
  \item{name}{the name of the method or message to invoke on the
   specified XLisp object}
  \item{\dots}{additional arguments (named or unnamed) to be passed in
   the method invocation.}
}
\value{
 \code{[[} returns the value of the specified slot,
 converted to an R object.

 \code{$} returns a function that when invoked
 will call the specified XLisp method for this object
 passing it the arguments in the call to that R function.

}
\references{\url{http://www.xlispstat.org},
\url{http://www.omegahat.org/RXLisp}}
\author{Duncan Temple Lang <duncan@research.bell-labs.com>}


\seealso{
\code{\link{.XLispInit}}
}

\examples{
 h <- .XLisp("histogram", rnorm(100))
 h[["title"]]
 h$close()
}
\keyword{interface}

