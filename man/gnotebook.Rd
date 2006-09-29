\name{gnotebook}
\alias{gnotebook}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{constructor for notebook widget}
\description{
	A notebook widget organizes different pages using tabs,
	allowing only one page to be shown at once. Clicking on
	the tab raises the associated page.
}
\usage{
gnotebook(tab.pos = 3, closebuttons = FALSE, dontCloseThese = NULL, container = NULL, ..., toolkit = guiToolkit())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab.pos}{Where to place tabs (1 bottom, 2 left side, 3 top, 4
  right side}
  \item{closebuttons}{Is there a close button in the tab?}
  \item{dontCloseThese}{If \code{closebuttons=TRUE} this will make it
  impossible to remove these tabs. Specified by tab number}
  \item{container}{Optional container to attach notebook widget to }
  \item{\dots}{Not utilized}
    \item{toolkit}{Which GUI toolkit to use}

}
\details{
  In what follows, it is useful to think of a notebook as a vector with
  named entries, each entry being a widget, the name being the tab label.

  Notebooks have the following methods:

  The \code{svalue} method returns the current page number. The
  \code{svalue<-} method is used to set the page number.


  New pages are added with the \code{add} method. The extra argument
  \code{label} is used to specify the tab label. This may be a string,
  or a \code{glabel}. The extra argument \code{index}
  can be used to specify which page to add to. By default, a new page is
  created at the end of the notebook. The extra argument
  \code{override.closebutton} can be used to add or not add a close
  button in the tab label.

  The \code{dispose} method will return the current page if is not
  overridden by \code{dontCloseThese}.

  The \code{delete(obj, widget,...)} method will delete the widget on a
  given page. This can then be replaced with the \code{[<-} method.

  The \code{length} method returns the number of pages.

  The \code{names} method returns the tab labels.

  The \code{names<-} method may be used to replace the tab
  labels. Something like \code{names(obj)[1]<-"new label"} should work.

  The \code{"["} method refers to the widgets in the notebook.

  The \code{"[<-"} method can be used to replace a widget on a notebook page.
  
}
\value{}
\references{}
\author{}
\note{}
\seealso{}
\examples{
\dontrun{
  nb = gnotebook(container=TRUE)
  add(nb, glabel("Widget 1"), label="page 1")
  add(nb, glabel("Widget 2"), label=glabel("page 2"))

  length(nb)
  names(nb)
  names(nb)[1] <- "Page 1"

  svalue(nb) <- 3
  dispose(nb)
  length(nb)
}
}
\keyword{interface}% at least one, from doc/KEYWORDS
