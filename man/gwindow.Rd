\name{gwindow}
\alias{gwindow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Constructor for base container}
\description{
  Widgets are packed inside containers which may in turn be packed
  inside other containers. The base container is known as a window. Only
  one container may be packed inside a window.
}
\usage{
gwindow(title = "Window", visible = TRUE, name=title,
  width = NULL, height= NULL, location=NULL,
handler = NULL, action = NULL,
 ..., toolkit = guiToolkit())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{title}{Title of window}
  \item{visible}{If \code{TRUE} window is drawn when
  constructed. Otherwise, window can be drawn latter using \code{visible<-}.}
\item{name}{Name for registry of windows}
\item{width}{Default width for window at creation}
\item{height}{Default height for window at creation}
\item{location}{If non-NULL, can be used to suggest default location of
window. This can be a coordinate pair (x,y) with (0,0) the upper left
corner, or a gwindow instance. In the latter case the location is
suggested by the location of the current window. This is useful for
placing dialogs near the parent window.}
\item{handler}{Handler for unrealize event (when window manager closes
window). Use addHandlerDestroy to get destroy event.}
\item{action}{Passed to handler}
  \item{\dots}{Not used}
  \item{toolkit}{Which GUI toolkit to use}  
}
\details{
   A base window can also be created using the argument
   \code{container=TRUE} when constructing a widget.

   The \code{svalue} method refers to the window title. Use
   \code{svalue<-} to change the title.

   The \code{add} method is used to add a widget or container to the
   base window. Only one may be added, so usually it would be another
   container.

   The \code{dispose} method destroys the window.

   The \code{size} method sets the minimum size. Use the \code{width}
   and \code{height} arguments to set the default size when the window
   is constructed.
}
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
  win = gwindow("Window example", handler=function(h,...) print("See
  ya"))
  add(win,gbutton("Cancel", handler = function(h,...) dispose(win)))
}
}
\keyword{interface}% at least one, from doc/KEYWORDS
