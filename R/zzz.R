## needed to find methods
.onLoad <- function(lib, pkg) {
  require(methods)
}

.onAttach <- function(...) {
  ## initialize toolkit if not there
  guiToolkit()
}
