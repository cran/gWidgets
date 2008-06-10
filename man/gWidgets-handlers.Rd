\name{gWidgets-handlers}
\alias{gWidgets-handlers}
\alias{removehandler}
\alias{addhandlerchanged}
\alias{addhandlerkeystroke}
\alias{addhandlerclicked}
\alias{addhandlerdoubleclick}
\alias{addhandlerrightclick}
\alias{addhandlerexpose}
\alias{addhandlermousemotion}
\alias{addhandlerunrealize}
\alias{addhandlerdestroy}
\alias{addhandleridle}
\alias{addpopupmenu}
\alias{add3rdmousepopupmenu}
\alias{addhandler}
\alias{addHandler}
\alias{removeHandler}
\alias{blockHandler}
\alias{unblockHandler}
\alias{addHandlerChanged}
\alias{addHandlerKeystroke}
\alias{addHandlerClicked}
\alias{addHandlerDoubleclick}
\alias{addHandlerRightclick}
\alias{addHandlerMouseMotion}
\alias{addHandlerExpose}
\alias{addHandlerUnrealize}
\alias{addHandlerDestroy}
\alias{addHandlerIdle}
\alias{addPopupmenu}
\alias{add3rdMousePopupmenu}
\alias{removeHandler}

\title{Methods to add  event handlers to objects}
\description{

	In the gWidgets API handlers are called in reponse to certain
	events such as keystrokes or clicks. This set of methods makes
	a consistent interface to some typical events. Not all
	handlers are defined for each widget. 

}
\usage{
addhandlerchanged(obj, handler = NULL, action = NULL, ...) 

addhandlerkeystroke(obj, handler = NULL, action = NULL, ...) 

addhandlerclicked(obj, handler = NULL, action = NULL, ...) 

addhandlerdoubleclick(obj, handler = NULL, action = NULL, ...) 

addhandlerrightclick(obj, handler = NULL, action = NULL, ...) 

addhandlermousemotion(obj, handler = NULL, action = NULL, ...)

addhandlerexpose(obj, handler = NULL, action = NULL, ...) 

addhandlerunrealize(obj, handler = NULL, action = NULL, ...) 

addhandlerdestroy(obj, handler = NULL, action = NULL, ...) 

addhandleridle (obj, handler = NULL, action = NULL, interval = 1000,   ...) 

addpopupmenu(obj, menulist, action=NULL, ...)

add3rdmousepopupmenu(obj, menulist, action=NULL, ...)

%%addhandler(obj, signal, handler = NULL, action = NULL, ...)

removehandler(obj, ID, ...)
}

\arguments{
  \item{obj}{The object to assign handler to}
%%  \item{signal}{The signal the handler responds to}
  \item{handler}{A function to call if the given event occurs.
    The function's first argument is a list with some specific
    components. The component \code{obj} contains the object that
    the handler was assigned to. The \code{action} component
    contains the value given to the argument \code{action}. This can be used
    with \code{do.call} to make simple handlers. Or, this can be
    used to pass in other widgets, etc. 
    
    Sometimes there are other components. For drag and drop
    handlers the component \code{dropdata} refers to the dropped
    data. For \code{ggraphics} the \code{addhandlerclicked}
    contains components \code{x} and \code{y} indicating where the
    click occurred.
  }
  \item{action}{Used to pass extra information into handlers }
  \item{interval}{For \code{addhandleridle} this specifies the
    time in milliseconds between calls to the handler.}
  \item{menulist}{For \code{addpopupmenu} and
    \code{add3rdmousepopupmenu} this specifies a menubar using a
    list which is in turn passed to \code{gmenu}.}
  \item{ID}{When a handler is assigned, an id is returned. This
    id can be used to remove a handler from an object.}
  \item{...}{}
}
\details{
  At first these handlers were all lowercase. These functions are still
  availabe, although the mixed case usage is encouraged
	
	In GTK, and other toolkits, an event causes a signal to
	be triggered and these handlers are called in response to that
	signal. 

	These signals have various names known to the GTK
	programmer. say. These functions attempt to shield the
	gWidgets user from needing to learn these signals. For
	gWidgetsRGtk, if these handlers prove insufficient then the
	non-exported \code{addhandler} function has an additional
	\code{signal} argument: \code{(obj,signal,handler,
	action,...)} for specifying a GTK signal. By avoiding this, we
	can make the gWidgets API non-toolkit specific.

	 The signals are defined to match the event
	described by the method name, e.g., "doubleclick." 

	The handlers all have signature \code{(h,...)} where the first
	argument is a list with components \code{obj} containing the
	widget the handler is added to and \code{action} containing
	the values passed along to the \code{action} argument. This
	can be used to pass in other widget's names, when they can not
	be found from a function closure, say.


	The handlers do not have lazy evaluation. The value of
	\code{action} is the one at the time of creation of the
	widget. (See the example). In GTK, a means to cheat this is to pass in
	a gWidget instance, as the underlying GTK objects are stored
	as pointers, not copies, so that when queried, their current
	state is used.


	\item{\code{addHandlerChanged}:}{ This handler is called when a widget
	is "changed."  This is interpreted differently by the various
	widgets. For \code{gedit} change refers to a changed value,
	not a keystroke change (when ENTER is pressed). For notebooks,
	this is called when a page is changed.
	}

	\item{\code{addHandlerKeystroke}:}{ This handler is called when keys are
	pressed in the text widgets. The extra argument \code{key} is
	used to pass back the key code of the pressed key.
	}	

	\item{\code{addHandlerClicked}:}{This handler is called when a widget,
	such as a button or label, is clicked.
	}	

	\item{\code{addHandlerDoubleclick}:}{This handler is called when a
	widget is doubleclicked, like in the tree widget. Not all
	widgets receive a double click signal. Only when a single
	mouse click is needed for selection is this implemented.}

      	\item{\code{addHandlerMouseMotion}:}{This handler is called when a
	  the mouse moves over a widget. In some toolkits it is called
	  just once per visit to the widget, for others maybe multiple
	  times. This is like a mouseover for web pages. The drag motion
	  handler is similar, only it is called when a drag event is
	  dragged over a widget.}

	\item{ \code{addHandlerExpose}:}{ handler is called when a widget is
	exposed. For instance when a page in a notebook is exposed.}

	\item{ \code{addHandlerUnrealize}:}{ handler is called when a widget
	is being unrealized.}

	\item{ \code{addHandlerDestroy}:}{ handler is called when a widget
	is being destroyed. For top level windows, this usually allows
	one to intercept the window destroy event for purposes of saving
	work etc.}

	\item{ \code{addHandlerIdle}:}{ handler is called every so often,
	and can be used to update a widget's content. This method has
	an extra argument 
	\code{interval} specifying the interval in milliseconds with a
	default of 1000 or 1 second. }


	Although not handlers, the \code{addPopupMenu} method adds a
	popup menu to a mouse click. The popup menu is specified using
	a list that is passed to \code{gmenu}.

	A refinement of this is the \code{add3rdMousePopupmenu} method
	which puts the popupmenu on the right mouse click.
}
% \value{}
% \author{}
% \note{}

\seealso{\code{\link{gWidgets-methods}}}
\examples{
\dontrun{
	## a default handler, useful for when action is enough to
	## specify desired results

	handler.default = function(h,...) do.call(h$action,list(svalue(h$obj)))
	group = ggroup(horizontal=FALSE, container=gwindow("Click
		button"))
        button = gbutton("Click me", container=group)
	addhandlerclicked(button, handler=handler.default, action="print")


	## use two widgets, one to update the other
	group = ggroup(horizontal=FALSE, container=gwindow("two widgets"))
	button = gbutton("click me", container=group)
	label = glabel("Button has not been clicked", container=group)
	addhandlerclicked(button, handler = function(h,...) {
	svalue(h$obj) <-"click me again"
	svalue(h$action) <- "Button has been clicked"
	}, action = label)


	## lazy evaluation is not used here
	obj = 4
	gbutton("click",cont=TRUE, handler=function(h,...)
	print(h$action), action=obj)
	obj = 2
	## now click button and value of 4 will be printed, not 2

	## Whereas, if one uses a gWidget we get the same as lazy
       ## loading
       obj = gedit("4")		 
	gbutton("click",cont=TRUE, handler=function(h,...)
		print(svalue(h$action)), action=obj)
	svalue(obj) <- "2"
	## Now click and "2" is printed.
}
}
\keyword{interface}% at least one, from doc/KEYWORDS
