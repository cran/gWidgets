## needed to find methods
.onLoad <- function(lib, pkg) {
  require(methods)
  require(utils)
  ## check that a toolkit package is loaded 
  tmp = installed.packages() 
  choices = tmp[grep("^gWidgets.",tmp[,1]),1]
  if(length(choices) == 0) {
    stop("\n\n*** gWidgets requires a toolkit implementation to be\n installed, for instance gWidgetsRGtk2, gWidgetstcltk, or gWidgetsrJava***\n\n.")
  }

  
}

.onAttach <- function(...) {
}
