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
% \value{}
% \references{}
% \author{}
\seealso{See also \code{\link{gtree}} for displaying tree-like data and
\code{\link{gdf}} for tabular data meant to be edited }
\examples{
\dontrun{
  ## example to select CRAN mirror
  m <- getCRANmirrors()[,c(1,4)]
  setCRAN <- function(URL) { ## see chooseCRANmirror
    repos = getOption("repos")
    repos["CRAN"] <- gsub("/$", "", URL)
    options(repos=repos)
  }
  w <- gwindow("gtable example",width=400)
  gp = ggroup(horizontal=FALSE, cont=w)
  tab <- gtable(m, chosencol = 2, cont=gp, expand=TRUE,
       handler = function(h,...) setCRAN(svalue(h$obj)))
  bg <- ggroup(cont=gp)
  addSpring(bg)
  gbutton("dismiss", cont=bg, handler = function(h,...) dispose(w))


  ## an example with icons.
  ## Select variables from a data frame

  ## find icons by class
  icon.FUN <- function(items) {
    dfName <- svalue(cb)
    df <- try(get(dfName, envir=.GlobalEnv), silent=TRUE)
    if(inherits(df,"try-error"))
      return(rep(NULL,dim(items)[1]))

    sapply(items[,1, drop=TRUE], function(i) {
      class(df[,i])[1]
    })
  }
  ## list data frames in an environment
  lsDF <- function(envir=.GlobalEnv) {
    varNames <- ls(envir=envir)
    dfs <- sapply(varNames, function(i) inherits(get(i,envir=envir),"data.frame"))
    varNames[dfs]
  }
  ## set up GUI
  w <- gwindow("Select variables",width=250)
  g <- ggroup(horizontal=FALSE, cont=w)
  l <- glabel("Data frame", cont=g)
  cb <- gcombobox(lsDF(), cont=g)
  tbl <- gtable(data.frame(variables=character(0)), icon.FUN=icon.FUN, cont=g,
    expand=TRUE)
  ## add handlers
  addHandlerChanged(cb, handler <- function(h,...) {
    dfName <- svalue(h$obj)
    dfNames <- names(get(dfName,envir=.GlobalEnv))
    tbl[,] <- data.frame(variables=dfNames, stringsAsFactors=FALSE)
  })
  addHandlerClicked(tbl, handler = function(h,...) {
    cat("Do something with",svalue(cb),"::", svalue(h$obj),"\n")
  })

}
}
\keyword{interface}% at least one, from doc/KEYWORDS
