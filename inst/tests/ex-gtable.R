## example to select CRAN mirror
m <- getCRANmirrors()[,c(1,4)]
setCRAN <- function(URL) { ## see chooseCRANmirror
  repos = getOption("repos")
  repos["CRAN"] <- gsub("/$", "", URL)
  options(repos=repos)
}


w <- gwindow("gtable example",width=400, visible=FALSE)
gp <- ggroup(horizontal=FALSE, cont=w)

tab <- gtable(m, chosencol = 2, cont=gp, expand=TRUE,
     handler = function(h,...) setCRAN(svalue(h$obj)))
bg <- ggroup(cont=gp)
addSpring(bg)
gbutton("dismiss", cont=bg, handler = function(h,...) dispose(w))

visible(w) <- TRUE

