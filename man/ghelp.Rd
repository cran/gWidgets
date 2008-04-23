\name{ghelp}
\alias{ghelp}
\alias{ghelpbrowser}

\title{Widget to interface with help pages}
\description{
  A widget to interface with the help pages and that widget placed in a
  browser. The widget is a notebook capable of showing several pages at once.
}
\usage{
ghelp(topic = NULL, package = NULL, container = NULL, ..., toolkit = guiToolkit())
ghelpbrowser(title = "Help browser", maxTerms=100, width=550, height=600, toolkit = guiToolkit()) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{topic}{Help topic}
  \item{package}{Which package to look for topic in}
  \item{container}{Optional container to attach widget to}
  \item{title}{Title of help browser}
  \item{maxTerms}{Maximum number of search responses}
  \item{width}{Width of browser window in pixels}
  \item{height}{Height  of browser window in pixels}
  \item{\dots}{Passed to \code{add} method of container}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
  Finding the help page can be a little slow.

  The \code{add(obj,value)} method can be used to add a new page. The
  page may be specified as a list with components \code{topic} and
  \code{package}, as a string in the form \code{package:::topic}, or a
  string of the form \code{topic}.
  
}
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
  obj <- ghelp(container=TRUE)
  add(obj,"base:::mean")

}
}
\keyword{interface}% at least one, from doc/KEYWORDS
