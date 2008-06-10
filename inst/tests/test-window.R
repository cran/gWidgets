## Test whether a main window works with
## * menubar
## * toolbar
## * content area
## * statusbar


## toolbar style
style="both"

f <- function(...) print("hi")
mbl <- list(File=list(save=list(icon="save",handler=f)))
tbl <- list(file=list(icon="save",handler=f),
            quit = list(icon="stop",handler=function(...) dispose(w)),
            quit1 = list(icon="quit",handler=f),
            quit2 = list(icon="quit",handler=f)
            )

w <- gwindow("test window")
## do upside down
sb <- gstatusbar("status", cont=w)
txt <- gtext(cont=w)
tb <- gtoolbar(tbl, cont=w, style=style)
mb <- gmenu(mbl, cont=w)


## test statusbar
svalue(sb) <- "This was added to status bar"

## test window
svalue(w) <- "title added via svalue"
