## needed to find methods
.onLoad <- function(lib, pkg) {
  require(methods)
}

.onAttach <- function(...) {
### This was giving troubles with cyclic dependencies so is removed
  ## initialize toolkit if not there
  ##guiToolkit()
}
