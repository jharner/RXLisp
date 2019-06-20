\name{.XLisp}
\alias{.XLisp}
\title{Invoke XLisp-Stat Function}
\description{
  This provides an interface to calling XLisp-Stat functions
  from R, converting the arguments from R values to XLisp-Stat objects
  and similarly converting the resulting value back to an R object.
  Functions are currently identified by name.
}
\usage{
.XLisp(fun, ..., .convert = TRUE, upper = TRUE)
}
\arguments{
  \item{fun}{the name of the XLisp-Stat function, given as a character string.}
  \item{\dots}{any arguments to be passed to the XLisp-Stat function,
    which will be converted from R objects to XLisp-Stat values.
    Any named elements in this list are treated as named arguments
    to the XLisp function. This means they are converted to upper-case
    and prefixed with a \code{:} and inserted into the XLisp argument list.
  }
   \item{.convert}{a logical value indicating whether to convert the
     result back to a regular R object.
     If this is \code{FALSE}, a reference to the 
     XLisp-Stat object is returned which can be used in subsequent
     calls to XLisp.
     If this is \code{TRUE}, an attempt to
     convert the value to an R object using the conversion mechanism is made.
    }
    \item{upper}{a logical value controlling whether to convert the
      (XLisp-Stat) function name being called to upper-case. }
}
\details{
  
}
\value{
 An R object representing the result of the call to the XLisp function.
}
\references{\url{http://www.xlispstat.org},
\url{http://www.omegahat.org/RXLisp}}
\author{Duncan Temple Lang <duncan@research.bell-labs.com>}

\seealso{
 \code{\link{.XLispInit}}
}

\examples{
 .XLispInit()
 .XLisp("+", 1, 2, 3)
 .XLisp("mean", c(1, 2, 3))


  # Generate some  random numbers from a Poisson distn.
  # Note the need to specify an integer.
 .XLisp("poisson-rand", as.integer(30), 1.8)

\dontrun{
  # returns a symbol which is not currently handled by the conversion
  # mechanism. But the load will work!
 .XLisp("load", system.file("examples", "Rinit.lsp", package="RXLisp"))
 .XLisp("mypow", 3, 2)
}
}
\keyword{interface}
