\name{gmenu}
\alias{gmenu}
\alias{gtoolbar}
\title{ Constructors to make menubar or toolbars}
\description{
	A menubar or toolbar are created using these
	constructors. These are specified using a lists, and these may
	be seen as simply mapping these lists into the corresponding widget.
}
\usage{
gmenu(menulist,  popup = FALSE, action=NULL, container = NULL, ..., toolkit = guiToolkit())

gtoolbar (toolbarlist, style = c("both", "icons", "text",
	 "both-horiz"), 
	 action=NULL, container = NULL, 
    ..., toolkit = guiToolkit()) 

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{menulist}{A list defining a menu bar}
  \item{popup}{Logical indicating if this should return a popup menu}
  \item{toolbarlist}{A list defining a toolbar}
  \item{style}{What style to use}
  \item{action}{Passed to menubar handlers}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{ingored}
    \item{toolkit}{Which GUI toolkit to use}

}
\details{
	The lists defining a menubar or toolbar are very similar.

	Each is a list with named components. A component is a
	terminal node if it has a \code{handler} component, which is a
	function to be called (without arguments) when the menu item
	or toolbar item is selected. Optionally, an \code{icon}
	component can be given specifying a stock icon to accompany
	the text. A non-null component named \code{separator} will
	also indicate a terminal node. In this case, a visible
	separator will be displayed.

	A menubar list can use the hierarchical nature of a list to
	generate submenus. For toolbars this is not the case.

	These constructors map the list into the widget. The methods
	for the constructors refer to these list defining the widget.

	The \code{svalue} method returns the list.

	The \code{svalue<-} method can be used to change the list, and
	hence redo the menubar.

	The \code{"["} method refers to the components of the list.

	The code{"[<-"} method can be used to change pieces of the
	menubar or toolbar.

	The \code{add} method with signature \code{(obj,lst)} or
	\code{(obj,gmenu.instance)} can be used to apped to the
	current menubar/toolbar. The second argument is a list or an
	gmenu or gtoolbar instance.

	The \code{delete} method 
	can be used to delete part of the menubar/toolbar. The
	\code{value} argument can be either a character vector with
	the top-level names to delete, or a named list, or an instance
	of either gmenu or gtoolbar.

	Except in RGtk2, menubars and toolbars should be added to the
	top level window. For 
}
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
 lst = list()
 lst$File$Open$handler = function(h,...) print("open")
 lst$File$Quit$handler = function(h,...) print("quit")
 lst$File$Quit$icon = "quit"
 lst$Edit$Find$handler = function(h,...) print("Find")
 lst$Edit$Replace$handler = function(h,...) print("Replace")

 obj = gmenu(lst, container=TRUE)

 lst1 = list()
 lst1$Help$Help$handler = function(h,...) print("help")

 add(obj, lst1)

 
}
}
\keyword{interface }% at least one, from doc/KEYWORDS
