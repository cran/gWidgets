(TeX-add-style-hook "gWidgets"
 (function
  (lambda ()
    (LaTeX-add-environments
     "RArgs")
    (TeX-add-symbols
     '("RListel" 1)
     '("RArg" 1)
     '("RPackage" 1)
     '("RFunc" 1)
     '("RCode" 1)
     "VERSION")
    (TeX-run-style-hooks
     "fancyhdr"
     "jvfloatstyle"
     "fancyhdr"
     "fancyvrb"
     "color"
     "hyperref"
     "url"
     "amsfonts"
     "amsmath"
     "relsize"
     "floatflt"
     "graphicx"
     "mathptm"
     "geometry"
     "times"
     "latex2e"
     "art12"
     "article"
     "12pt"))))

