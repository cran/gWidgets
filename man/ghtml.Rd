\name{ghtml}
\alias{ghtml}
\title{Constructors for widgets to handle text input}
\description{
	The ghtml widget is used to enter single lines of text. The
	gtext widget creates a text buffer for handling multiple lines
	of text. 
}
\usage{
ghtml(url, handler = NULL, action = NULL, container = NULL, ..., toolkit = guiToolkit())

}
\arguments{
  \item{url}{url to load.}
  \item{handler}{Handles a click on a URL. The default is to open the
     clicked url in the widget. To override, the first argument,
     a list \code{h}, has component \code{h\$url} containing the url.}
  \item{action}{ Passed along to the  handler as \code{h\$action}}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{ ignored}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
  This widget loads the given url into a widget. Currently no toolkits
  support this.

  The \code{svalue} method returns the current url.

  The \code{svalue<-} method loads the current url in the widget.

}
\value{}
\references{}
\author{}
\note{}
\seealso{}
\examples{
\dontrun{
  ghtml(system.file("html","gedit.html",package="gWidgets"),
    container = gwindow())
}
}
\keyword{interface }
