\name{gseparator}
\alias{gseparator}
\title{Widget to draw separator line}
\description{
  Simple widget to draw a line used  clarify layout of widgets.
}
\usage{
gseparator(horizontal = TRUE, container = NULL, ..., toolkit = guiToolkit())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{horizontal}{If \code{TRUE} line is horizontal, otherwise vertical.}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{Ignored }
    \item{toolkit}{Which GUI toolkit to use}

}
\value{}
\references{}
\author{}
\note{}
\seealso{}
\examples{
\dontrun{
  group = ggroup(container=TRUE)
  add(group, glabel("left widget"))
  add(group, gseparator(horizontal=FALSE))
  add(group, glabel("right widget"))
}
}
\keyword{interface}% at least one, from doc/KEYWORDS
