
render_entry <- function(entry, link = "page") {
  if (link == "page") { # for linking to the pub page
    link_f <- function(x) {
      paper_path <- file.path("papers", str_c(entry$key, ".html"))
      if (!file.exists(paper_path)) return(x)
      
      str_c("<a href='", paper_path, "'>", x, "</a>")
    }
  } else if (link == "pdf") { # for linking to the pub pdf
    link_f <- function(x) {
      paper_path <- file.path("pdfs", str_c(entry$key, ".pdf"))
      if (!file.exists(paper_path)) return(x)
      
      str_c("<a href='/", paper_path, "'>", x, "</a>")
    } 
  } else {
    link_f <- function(x) x
  }
  
  authors <- str_c(vapply(entry$author, format, character(1)), 
    collapse = ", ")
  
  thumb_path <- file.path("thumbs", str_c(entry$key, ".png"))
  
  if (file.exists(thumb_path)) {    
    thumb <- link_f(str_c("  <img src='/", thumb_path, "' width='100' alt='pub thumbnail'/>\n"))
  } else {
#     thumb <- NULL # spacer?
    # is there a better way than to call an empty spacer div?
    thumb <- str_c("  <div class='thumbSpace'> &nbsp; </div>\n")
    
  }
  
  paper_path <- file.path("pdfs", str_c(entry$key, ".pdf"))  
  links <- c(
    if (file.exists(paper_path)) {
      # determines the pdf link name. There should be a better way to do this, maybe integrated with the grouping?
      pdfType <- if(entry$bibtype == "Misc"){
        if (entry$howpublished == "poster") {
          "poster"
        } else if (entry$howpublished == "invited talk" | entry$howpublished == "conference presentation") {
          "slides"
        } else {
          "pdf"
        } 
      } else if(entry$bibtype %in% c("MastersThesis","Unpublished")){
        "unpublished draft"
      } else {
        "pre-print"
      }
      str_c("<a href='/", paper_path, "'>",pdfType,"</a>")},
    if (!is.null(entry$url)) {
      if (entry$bibtype == "Article") {
        str_c("<a href='", entry$url, "'>from publisher</a>")
      } else if (!is.null(entry$howpublished)){
        if(entry$howpublished == "software"){
          str_c("<a href='", entry$url, "'>via software page</a>")
        } else {
          str_c("<a href='", entry$url, "'>direct link</a>")
        }
      }
    },
    if (!is.null(entry$videourl)) 
      str_c("<a href='", entry$videourl, "'>watch online</a>"),
    if (!is.null(entry$doi)) 
      str_c("<a href='http://dx.doi.org/", entry$doi, "'>via doi</a>\n")
    )
  if (length(links) > 0) {
    download <- str_c("<p class='download'><span>Download</span>: \n", 
      str_c(links, collapse = " | "), "</p>\n")    
  } else {
    download <- NULL
  }

  citation <- cite(entry, citeType="web")
  str_c(
    "<div class='citation'>\n",
    thumb,
    "  <ul>\n",
    "    <li class='author'>", authors, ".</li>\n",
    "    <li class='title'>", link_f(ifelse(entry$bibtype=="InBook",entry$chapter,entry$title)), ".</li>\n",
    if (!is.null(citation))
    str_c("    <li class='citation'>", citation, "</li>\n"),
    if (!is.null(entry$note))
    str_c("    <li class='note'>[",if(!is.null(entry$howpublished))
            str_c(entry$howpublished, ", "),
          entry$note, "]</li>\n"),
    "  </ul>\n",
    download,
    "  <br />",
    "</div>\n"
  )
}

