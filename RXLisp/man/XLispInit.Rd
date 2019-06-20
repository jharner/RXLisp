\name{.XLispInit}
\alias{.XLispInit}
\title{Initialize XLisp-Stat Session}
\description{
  This initializes the XLisp-Stat engine so that it can be used
  to process calls to XLisp functions, etc.
  This must be called before any access to the XLisp interpreter
  is made.
}
\usage{
.XLispInit(args=.XLispInitArgs, registerEvent = TRUE)
}
\arguments{
  \item{args}{a character vector giving the command line arguments
    used to initialize the session. The first value should be the
    pseudo name given to the application as in \code{argv[0]}, the name
    of the application, passed from the shell in
    a regular C application. The subsequent arguments are processed in
    the standard fashion by the XLisp-Stat engine.}
  \item{registerEvent}{a logical value indicating whether the
    initialization
    should also connect the XLisp event loop to the R event loop.
    Generally, this will be \code{TRUE}. One may want to override
    this if a different event loop strategy will be used or no
    XLisp graphics will be deployed.}
}  
\details{
  This initializes the XLisp-Stat engine, including the memory
  management, intialization code, etc.
}
\value{
 \code{NULL}.
}
\references{\url{http://www.xlispstat.org}}
\author{Duncan Temple Lang <duncan@research.bell-labs.com>}

\seealso{
 \code{\link{.XLisp}}
}

\examples{
 .XLispInit()
 .XLisp("+", 1, 2, 3, 4)
}
\keyword{interface}