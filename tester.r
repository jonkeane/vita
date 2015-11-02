library(devtools)

# must install ImageMagick for thumbs conversion

# load_all("~/Dropbox/web/vita")
install("~/Dropbox/web/vita/", dependencies=FALSE) #  installs stringi??

# library(stringr)
# library(bibtex)
# library(brew)
# library(plyr)
# library(tools)
# library(purrr)
# library(bibtex)
library(vita)

make_cvtex(bib = "~/Dropbox/web/vitaKeane/keane.bib", clean=TRUE)

make_vita(bib = "~/Dropbox/web/vitaKeane/keane.bib", clean=FALSE)


