\name{gradio}
\alias{gradio}
\alias{gdroplist}
\alias{gcombobox}
\alias{gcheckboxgroup}
\title{Widgets to allow selection from a vector of items}
\description{
   Widgets to select one (or several) from a given vector of
   items. These are a radio group where all values are shown at once,
   but only one may be selected; a checkbox group where more than one
   may be selected; and a
   combo box (or droplist) where initially only a single value is shown, and the others
   are a mouse click away,
}
\usage{
gradio(items, selected = 1, horizontal = FALSE, handler
= NULL, action = NULL, container = NULL, ..., toolkit = guiToolkit())

gcombobox(items, selected = 1, editable = FALSE, coerce.with=NULL, handler = NULL, 
    action = NULL, container = NULL, ..., toolkit = guiToolkit())

gdroplist(items, selected = 1, editable = FALSE, coerce.with=NULL, handler = NULL, 
    action = NULL, container = NULL, ..., toolkit = guiToolkit()) 

gcheckboxgroup(items, checked = FALSE,  horizontal = FALSE, 
    handler = NULL, action = NULL, container = NULL, ..., toolkit = guiToolkit()) 
}


\arguments{
  \item{items}{ Vector of values to select from }
  \item{selected}{ For gradio the initial selected value (as an index)
  		   For a drop list, the first selected value. Use 0 to
		     leave blank}
  \item{checked}{For gcheckboxgroup a logical vector of initial values.}
  \item{editable}{For gdroplist, a logical indicating if the user can
  add an entry to the list of available answers}
  \item{coerce.with}{Apply this function to selected value before returning}
  \item{horizontal}{A logical specifying the  layout for gradio and gcheckboxgroup}
  \item{handler}{Called when selection is changed}
  \item{action}{Passed to handler when called.}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{ Ignored}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
	The basic methods all work similarly for these three widgets.

	The \code{svalue} method returns the selected values by
	name. If the extra argument \code{index=TRUE} is specified,
	the indices of the selected values is given. 


	The \code{svalue<-} method can be used to set the selected
	value.

	The \code{"["} method refers to the vector defining the items.

	The \code{"[<-"} method can be used to change the vector
	defining the items. The length will most likely need to be the same.

	The \code{"length"} method returns the number of items.
	
	For \code{gdroplist} the argument \code{editable=TRUE} adds a
	text-edit box where the user can type in a selection. By
	default this value is returned as a character by
	\code{svalue}. Use \code{coerce.with} to coerce this prior to returning.

}
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
  flavors = c("vanilla", "chocolate", "strawberry")

  group = ggroup(container=TRUE)
  add(group,glabel("Favorite flavor:"))
  add(group, gradio(flavors))

  group = ggroup(container=TRUE)
  add(group,glabel("Favorite flavor:"))
  add(group, gdroplist(flavors, editable =TRUE))

  group = ggroup(container=TRUE)
  add(group,glabel("Favorite flavors:"))
  add(group, obj <- gcheckboxgroup(flavors))

  svalue(obj) <- c(TRUE, FALSE, TRUE)
  svalue(obj)
  obj[3] <- "raspberry"

}

}
\keyword{interface}% at least one, from doc/KEYWORDS
