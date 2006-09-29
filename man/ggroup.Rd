\name{ggroup}
\alias{ggroup}
\alias{gframe}
\alias{gexpandgroup}
\title{Containers for packing in subsequent widgets}
\description{
	A \code{ggroup} is the primary container for packing in
	subsequent widgets, either from left to right or top to
	bottom. Widgets are packed in using the \code{add} method and
	can be removed with the \code{delete} method.

	The \code{gframe} and \code{gexpandgroup} widgets add a label
	to the group and in the latter case an arrow for
	hiding/showing the group.
}
\usage{
ggroup(horizontal = TRUE, spacing = 5, container = NULL, ..., toolkit = guiToolkit())

gframe(text = "", markup = FALSE, pos = 0, container = NULL, 
    ..., toolkit = guiToolkit()) 
gexpandgroup(text = "", markup = FALSE, handler = NULL, action = NULL, 
    container = NULL, ..., toolkit = guiToolkit())
}
\arguments{
  \item{horizontal}{Specifies if widgets are packed in left to right
	or top to bottom (\code{FALSE})}
  \item{spacing}{Space in pixels around each widget. Can be changed
	with \code{svalue}}
  \item{text}{Text for label}
  \item{markup}{Optional markup. (See \code{\link{glabel}} for details.)}
  \item{pos}{Where to place label: 0 is to left, 1 to right,
  interpolates.}
  \item{handler}{Called when expand arrow is clicked}
  \item{action}{Passed to handler}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{For \code{gframe}, \code{gexpandgroup} these are passed
    along to \code{ggroup}}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{

	The groups pack in widgets from either left to right (when
	\code{horizontal=TRUE}) or from top to bottom (when
	\code{horizontal=FALSE}). 


	The \code{spacing} value determines the number of pixels
	of padding between each widget. This can be set when the group
	is constructed, or later using \code{svalue}. 

	The \code{svalue<-} method can be used to adjust the border
	width. By default it is 2 pixels.
	
	To put  space between just two widgets, the
	\code{addSpace(obj, value, ...)}
	method may be used where \code{value} is the number of pixels
	of padding between the just packed widget, and the next one to
	be packed.

	Widgets are packed together like blocks.  When packing from
	top to bottom, the width of each widget can either be the
	natural size of the widget (the default), or if the
	\code{expand=TRUE} argument is given to the \code{add} method
	the widget will expand to fill the possible space.

	The \code{addSpring(obj,...)} method will push the just packed
	widget and the next-to-be packed widget as far apart as possible.

}
\value{}
\references{}
\author{}
\note{ }
\seealso{}
\examples{
\dontrun{
  ## basic group
  group = ggroup(horizontal=FALSE, container=gwindow())
  add(group, glabel("widget 1"))
  add(group, gbutton("widget 2"))

  ## nested groups
  group = ggroup(horizontal=FALSE, container=gwindow())
  innergroup = ggroup(container = group)
  add(innergroup, gbutton("button 1"))
  add(innergroup, gbutton("button 2"))
  add(group, gtext("Text area"))

  ## expand argument
  group = ggroup(horizontal=FALSE, container=gwindow())
  add(group,gbutton("no expand"))
  add(group,gbutton("expand=TRUE"), expand=TRUE)

  ## add spring
  group = ggroup(container=gwindow("menubar-like example"))
  add(group, glabel("File",handler=function(h,...) print("file")))
  add(group, glabel("Edit",handler=function(h,...) print("edit")))
addSpring(group)				   
  add(group, glabel("Help",handler=function(h,...) print("help")))

}
}
\keyword{ interface }

