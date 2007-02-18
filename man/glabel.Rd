\name{glabel}
\alias{glabel}
%%\alias{rotatelabel}
\alias{gbutton}
\alias{gimage}
\title{Constructors for widgets that  show text or images}
\description{
	A label, button or graphic show basic bits of text or images
	in a widget. Each of these can have a handler assigned for
	when the widget is clicked.

	Labels show text. The text can be marked up. An option is
	available so that the displayed text can be edited. 

	Buttons show text and/or images in a clickable object whose
	shading indicates that the button is to clicked on.

	Images can be shown.
}
\usage{
glabel(text = "", markup = FALSE, editable = FALSE, handler = NULL,
action = NULL, container = NULL, ..., toolkit = guiToolkit())

%%rotatelabel(obj, angle, ...) 

gbutton(text = "", border=TRUE, handler = NULL, action = NULL, container = NULL, 
    ..., toolkit = guiToolkit()) 

gimage(filename = "", dirname = "",  size = "", 
    handler = NULL, action = NULL, container = NULL, ..., toolkit = guiToolkit()) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{Text to show in the label or button. For buttons, if
    this text matches a stock icon name, an icon is shown as well}
  \item{border}{If \code{TRUE} a border is drawn to make a button look like a
  button. If \code{FALSE}, the no border so the button looks like a label.}
  \item{markup}{ Logical indicating if text for a label uses markup }
  \item{editable}{Logical. If TRUE, then the label's text can be set
  by clicking on the label and filling in the edit box. }
  \item{filename}{Specifies location of image. May be a stock icon
  name or filename. }
  \item{dirname}{Directory of file. If "stock", then a stock icon is used.}
  \item{size}{Size of image when stock image is used. Values are in
  \code{c("menu", "small_toolbar","large_toolbar","button","dialog")}}
  \item{handler}{Handler called on a click event}
  \item{action}{ Passed to handler }
  \item{container}{Optional container to attach widget to.}
%  \item{obj}{a \code{glabel} instance}
%  \item{angle}{Angle to rotate label, in degrees}
  \item{\dots}{ ignored }
    \item{toolkit}{Which GUI toolkit to use}

}
\details{
  
	The \code{svalue()} method returns the value of the widget. For
	a label, this is the text. For a button, the same. For an image,
	it is the filename of the figure or the stock icon name, if the
	icon was set from a stock icon.
	
	
	The \code{svalue<-()} method can be used to set the value of
	the widget. For an image, the value is a filename containing the
	image to display.

	The \code{addhandlerclicked} method is called on click
	events.

	For labels, if \code{editable=TRUE} is specified,
	clicking on the text allows one to edit the label's value
	overriding the click handler in the process. However, the
	\code{addhandlerchanged} handler can be given to respond to the
	text after it has been chnaged.


}
\value{}
\references{}
\author{}
\note{ 
}
\seealso{ }
\examples{
\dontrun{
   glabel("a label", container=TRUE)
   glabel("Click me to edit label", editable=TRUE, container=TRUE)
   glabel("Click me for a message", container=TRUE,
   handler=function(h,...) {cat("Hi\n")})
}
}
\keyword{ interface }% at least one, from doc/KEYWORDS
