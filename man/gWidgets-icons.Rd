\name{gWidgets-icons}
\alias{gWidgets-icons}
\alias{addStockIcons}
\alias{getStockIcons}
\title{Functions for adding icons}
\description{
  Two functions for listing "stock" icons, and adding "stock" icons. A
  stock icon can be referenced within gWidgets by it simple name, such
  as "ok" or "close". 
}
\usage{
addStockIcons(iconNames, iconFiles, ...,toolkit=guiToolkit())

getStockIcons(..., toolkit=guiToolkit())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{iconNames}{A vector of icon names}
  \item{iconFiles}{A matching vector of filenames for the icons}
  \item{...}{ignored}
  \item{toolkit}{Which toolkit to use}
}
\details{
  The file type must be supported by the toolkit.
}
\references{}
\author{}
\note{}
\seealso{}
\examples{
\dontrun{
 iconNames = c("larrow","rarrow")
 iconFiles = c("/usr/share/icons/larrow.png",
 "/usr/share/icons/rarrow.png")
 addStockIcons(iconNames, iconFiles)
}
}
\keyword{interface}% at least one, from doc/KEYWORDS
