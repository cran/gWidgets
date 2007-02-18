## needed to find methods
.onLoad <- function(lib, pkg) {
  require(methods)
##   require(rJava)
##   ## get java files. For now borrowed from iWidgets
##   .jinit(paste(libname,pkgname,"java",sep=.Platform$file.sep))

}

.onAttach <- function(...) {
### This was giving troubles with cyclic dependencies so is removed
  ## initialize toolkit if not there
  ##guiToolkit()
}
