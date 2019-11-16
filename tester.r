# must install ImageMagick for thumbs conversion


# brew install libplist
devtools::install_github("hrbrmstr/plist")

# load_all("~/Dropbox/web/vita")
install("~/Dropbox/web/vita/", dependencies = TRUE)

library(vita)

make_cvtex(bib = "~/Dropbox/web/vitaKeane/keane.bib", clean = TRUE)

make_vita(bib = "~/Dropbox/web/vitaKeane/keane.bib", clean = TRUE)
