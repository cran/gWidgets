\name{gstatusbar}
\alias{gstatusbar}

\title{Constructor of status bar widget}
\description{
  A status bar widget is used to send message to the user. A familiar
  instance is the bottom area of a web browser.
  }
\usage{
gstatusbar(text = "", container = NULL, ..., toolkit = guiToolkit())
}

\arguments{
  \item{text}{Initial text of status bar}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{Ignored}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
 The statusbar keeps a message stack. The \code{svalue} method pops the
 last message from the stack. The \code{svalue<-} method pushes a new
 message onto the stack.
}
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
  group = ggroup(horizontal=FALSE, container=TRUE)
  add(group, obj <- gedit(), expand=TRUE)
  add(group, sb <- gstatusbar("Type in box"))
addhandlerchanged(obj, handler=function(h,...) svalue(sb) <- "You typed 
in box")
}
}
\keyword{interface}% at least one, from doc/KEYWORDS
