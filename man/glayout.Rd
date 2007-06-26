\name{glayout}
\alias{glayout}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A container for aligning widgets in a table}
\description{
  A container for laying out widgets in a table. The widgets are added
  using matrix notation (\code{[i,j]<-}).

}
\usage{
glayout(homogeneous = FALSE, spacing = 10, container = NULL, ..., toolkit = guiToolkit())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{homogeneous}{A logical indicating if the  cells are all the same size}
  \item{spacing}{Spacing in pixels between cells}
  \item{container}{Optional container to attach widget to.}
  \item{\dots}{ignored}
    \item{toolkit}{Which GUI toolkit to use}

}
\details{
  Widgets are added using matrix notation. A widget can span
  several cells, for instance \code{obj[1:2,2:3] <- widget}
  would place the widget in the first and second rows and second
  and third columns. 

  As a convenience, if the value to be assigned is a character it will
  be turned into a \code{glabel} object before being added.

  
  For gWidgetsRGtk2, the widget may only be realized once, and once realized can
  not be  modified. As such the process to add a new table is: use
  \code{glayout} contstructor to make object. Add widgets with matrix
  notation. Call \code{visible<-} method on object to make widget
  visible.


  For gWidgetsrJava the extra argument \code{anchor} is a pair of values
  from -1,0,1 to indicate the x and y positioning of the widget within
  the cell. In gWidgetsRGtk2 such positioning is done by attaching the
  widget to a ggroup container with the desired postion and then adding
  the ggroup container to the glayout container.

  For gWidgetsrJava the extra argument \code{expand}, a logical, will
  force the widget to expand to fill the entire space if \code{TRUE}.
}
	
\value{}
\references{}
\author{}
\note{}
\seealso{}
\examples{
\dontrun{
  ## show part of mtcars dataframe in a layout
  obj = glayout(container=TRUE)
  for(i in 1:6) {
    for (j in 1:4) obj[i,j] <- glabel(mtcars[i,j])
  }
  visible(obj) <- TRUE

}
}
\keyword{interface}% at least one, from doc/KEYWORDS
