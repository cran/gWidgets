\name{gimage}
\alias{gimage}
\title{Constructor to show images}
\description{
  This constructor produces a widget intended to show images
  stored as files on the file system. 
}
\usage{
gimage(filename = "", dirname = "",  size = "", 
    handler = NULL, action = NULL, container = NULL, ..., toolkit = guiToolkit()) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{Specifies location of image. May be a stock icon
  name or filename. (In the future may be a url.)}
  \item{dirname}{Directory of file. If "stock", then a stock icon is used.}
  \item{size}{Size of image when stock image is used. Values are in
    \code{c("menu", "small_toolbar","large_toolbar","button","dialog")}}
  \item{handler}{Handler called on a click event}
  \item{action}{ Passed to handler }
  \item{container}{Optional container to attach widget to.}
%  \item{obj}{a \code{glabel} instance}
%  \item{angle}{Angle to rotate label, in degrees}
  \item{\dots}{Passed to \code{add} method of container}
    \item{toolkit}{Which GUI toolkit to use}

}
\details{
  
	The \code{svalue()} method returns the filename of the figure or
	the stock icon name, if the icon was set from a stock icon.
	
	
	The \code{svalue<-()} method can be used to set the value of
	the widget. The value is a filename containing the image to
	display.

	The \code{addhandlerclicked} method is called on click
	events. 

}
% \value{}
% \references{}
% \author{}
% \note{}
\seealso{See \code{\link{getStockIcons}} to get a list of available
  icons and \code{\link{addStockIcons}} to add to this list.}
\examples{
\dontrun{
   w <- gwindow("Stock icon example")
   gimage("ok",dirname="stock", cont = w)
}
}
\keyword{ interface }% at least one, from doc/KEYWORDS
