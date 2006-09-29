\name{gtable}
\alias{gtable}
\title{Constructor for widget to display tabular data}
\description{
	This widget displays either a vector, matrix or data frame in
	a tabular format. The main usage is for user selection of a
	row or rows.
}

\usage{
gtable(items, multiple = FALSE, chosencol = 1, icon.FUN = NULL, filter.column = NULL, filter.labels = NULL, filter.FUN = NULL, handler = NULL, action = NULL, container = NULL, ..., toolkit = guiToolkit())
}

\arguments{
  \item{items}{A vector, matrix or data frame to be displayed. A
  vector and matrix is coerced into a data frame.}
  \item{multiple}{A logical. If \code{TRUE} multiple lines can be selected}
  \item{chosencol}{By default, only the value in this column is
  returned by the \code{svalue} method.}
  \item{icon.FUN}{If given, this function is applied to the data frame
  to be shown. It should return a vector of stock icon names}
  \item{filter.column}{If not \code{NULL} a filter by droplist is
  given which can be used to filter the displayed values shown using
  the values in this column.}
  \item{filter.labels}{If filtering is desired, but by a more
  complicated means that just a columns value, then these labels are
  used for the drop list. These are used by \code{filter.FUN}}
  \item{filter.FUN}{A function with signature \code{(obj, filter.by)}
  to specify a vector of logical values indicating which rows should
  be shown.}
  \item{handler}{Called on a double click event}
  \item{action}{Passed to handler}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{ ignored }
  \item{toolkit}{Which GUI toolkit to use}
}
\details{

  When no filtering is requested, the column headers can be clicked to
  sort the values.

  
	The \code{svalue} method returns the selected value(s). By
	default, only the value(s) in the chosen column are
	returned. Use the argument \code{drop=FALSE} to return the
	entire row. To return the row index, use the argument
	\code{index=TRUE}. When filtering is being used, this index
	refers to the entire data frame, not just the visible data frame.

	The \code{"["} notion treats the object like a data frame. When
	filtering or sorting, this notation refers to the entire data frame, not
	the visible or rearranged data frame.

	Assignment via \code{"[<-"} may be used. In gWidgetsRGtk there are
	limitations. The number of columns may not be
	reduced. Assignment with both row and column indices missing may
	be needed for some desired actions.

	As with data frames, the data type of a variable is
	important. In particular,  assignment with
	\code{"[<-"} for factors can cause warnings if the values are
	not in the factor's levels. When the value being assigned is a
	matrix there is a coercion to a data frame which may change
	the type.


	The \code{visible} and \code{visible<-} methods refer to which
	rows of the data store are visible in the widget. These are
	specified by a vector of class logical. This may be used when
	there is filtering, not sorting.

	The \code{dim} method returns the dimension of the data frame.

	The \code{dimnames} method works, although you can't see the
	rownames, you can  assigne to column names, although
	\code{names} is more convenient.

	
	A single click is used for selection of a value. 

	The \code{addhandlerdoubleclick} handler responds to a double
	click event. 


}
\value{}
\references{}
\author{}
\seealso{See also \code{\link{gtree}} for displaying tree-like data and
\code{\link{gdf}} for tabular data meant to be edited }
\examples{
\dontrun{
  icon.FUN = function(df) {
    x = rep("file", length = length(df$isdir))
    x[df$isdir] = "directory"
    x
  }
  files = file.info(dir())
  files = cbind(rownames(files), files[,c("size","isdir")])
  names(files)[1] = "Filename"
  gtable(files, icon.FUN=icon.FUN, container=TRUE)
}
}
\keyword{interface}% at least one, from doc/KEYWORDS
