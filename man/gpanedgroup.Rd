\name{gpanedgroup}
\alias{gpanedgroup}

\title{A paned group holds two widgets with a handle between them to
adjust the amount of space allocated to each}
\description{
  A constructor for a paned group.
}
\usage{
gpanedgroup(widget1, widget2, horizontal = TRUE, container = NULL, ..., toolkit = guiToolkit())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{widget1}{Left (top) widget}
  \item{widget2}{Right (bottom) widget}
  \item{horizontal}{Left/right (\code{TRUE}) or top/bottom}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{ignored}
    \item{toolkit}{Which GUI toolkit to use}

}
\details{}
\value{}
\references{}
\author{}
\note{}
\seealso{}
\examples{
\dontrun{
 gpanedgroup(glabel("left widget"), glabel("right widget"),
 container=TRUE)
}
}
\keyword{interface }% at least one, from doc/KEYWORDS
