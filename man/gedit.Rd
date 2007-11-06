\name{gedit}
\alias{gedit}
\alias{gtext}
\title{Constructors for widgets to handle text input}
\description{
	The gedit widget is used to enter single lines of text. The
	gtext widget creates a text buffer for handling multiple lines
	of text. 
}
\usage{
gedit(text = "", width = 25, coerce.with = NULL, handler = NULL, action = NULL, container = NULL, ..., toolkit = guiToolkit())

gtext (text = NULL, width = NULL, height = 300, font.attr = NULL, 
    wrap = TRUE, handler = NULL, action = NULL, container = NULL, 
    ..., toolkit = guiToolkit()) 
}
\arguments{
  \item{text}{Initial text in widget}
  \item{width}{Width of widget. For gedit, this means the number of
    characters. For gtext the pixel widget}
  \item{height}{Height of gtext widget in pixels}
  \item{font.attr}{Optional specification of font attributes}
  \item{wrap}{For gtext, are long lines wrapped?}
  \item{coerce.with}{For gedit, when the value is retrieved this
    function is applied to the result. (The stored value is always a
    character, this can be used to make it numerc, to quote it, ...}
  \item{handler}{Handler called when text is changed. For gedit, this
    means the enter key is pressed.}
  \item{action}{ Passed to handler}
  \item{container}{Optional container to attach widget to}
  \item{\dots}{ ignored}
  \item{toolkit}{Which GUI toolkit to use}
}
\details{
	The \code{gedit} widget has the following methods:

	The \code{svalue} method retrieves the value. If a function is
	given to the argument \code{coerce.with} it is applied before
	the value is returned. This can be used to coerce the text
	value (always of class character) to a numeric, or to a date,
	or to be quoted, ...

	The \code{svalue<-} method is used to set the value.

	The \code{"["} and \code{"[<-"} methods refer to the widgets
	"type-ahead" values. A familiar usage is when a url is typed
	into a web browser, matches appear from a users history that
	could possibly complete the typed url.


	The \code{gtext} widget has the following methods.

	The \code{svalue} method returns the text held in the
	buffer. If \code{drop=TRUE}, then only the text in the buffer
	selected by the mouse is returned.

	The \code{svalue<-} method replaces the text in the buffer
	with the new text.

	New text is added with the \code{add} method. The basic usage
	is \code{add(obj,text)} where "text" could be a single line or
	a vector of text, or a gwidget (although some, like gedit,  are kind of
	flaky). Extra arguments include \code{do.newline} a
	logical indicating if a new line after the last line should be
	added (default is \code{TRUE}); \code{font.attr} to specify
	any font attributes; \code{where} indicating where to add the
	text (either \code{end}  or \code{beginning}).

	The font can be changed. The \code{font.attr} argument to the
	constructon and to \code{add} specifies fonts using a
	namedcharacter vector. For instance
	\code{c(style="normal", weights="bold",sizes="medium")}.
	The command \code{obj[['tags']]} will produce a list
	containing all the available attributes. 

	The \code{font<-} method is used to change the font of the
	currently selected text. It too takes a named character vector
	specifying the font attributes.

	The \code{dispose} method clears the text in the buffer.

}
% \value{}
% \references{}
% \author{}
% \note{}
% \seealso{}
\examples{
\dontrun{
	gedit("type here", container=TRUE)
	
	## change handler
	obj = gedit(container=TRUE)
	addhandlerchanged(obj, handler=function(h,...) 
	  cat("You typed", svalue(h$obj),"\n"))

	 ## coerce to numeric
	 obj = gedit("7", container=TRUE, coerce.with=as.numeric)
	 svalue(obj)

        ## gtext example
        obj = gtext("First line", container=TRUE)
        add(obj,"second line", font.attr=c(family="monospace"))
        add(obj,"third line", font.attr=c(foreground.colors="red"))
}
}
\keyword{interface }
