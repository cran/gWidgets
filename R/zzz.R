## needed to find methods
.onLoad <- function(lib, pkg) {
  require(methods)
  require(utils)

  doRequire <- function(pkg) do.call("require",list(pkg))
  popup <- function() {
    ## popup install message if note presnet
    ## already checked that no gWidgetsXXX is available
    all <- installed.packages()
    pkgs <- rownames(all)
    
    title <- "gWidgets needs a toolkit package"
    msg <- "gWidgets needs a toolkit package installed."

    if("tcltk" %in% pkgs) {
      if("RGtk2" %in% pkgs) {
        msg <- paste(msg, "Try installing gWidgetsRGtk2.", sep="\n")
      } else {
        msg <- paste(msg, "Try installing gWidgetstcltk.", sep="\n")
      }

      doRequire("tcltk")
      w <- tktoplevel()
      tkdialog(w, title,msg,"",0,"close")
      
    } else {
      msg <- paste(msg,
                   "You must install either RGtk2 or tcltk,",
                   "then a toolkit package.",
                   sep="\n")
      cat(msg, "\n")
    }
  }
  
  
  
  ## check that a toolkit package is loaded 
  tmp = installed.packages() 
  choices = tmp[grep("^gWidgets.",tmp[,1]),1]
  if(length(choices) == 0) {

    popup()

    stop("\n\n*** gWidgets requires a toolkit implementation to be\n installed, for instance gWidgetsRGtk2 or gWidgetstcltk***\n\n.")
  }

  
}

.onAttach <- function(...) {
}
