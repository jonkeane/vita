
render_entry <- function(entry, link = TRUE) {
  if (link) {
    link_f <- function(x) {
      paper_path <- file.path("papers", str_c(entry$key, ".html"))
      if (!file.exists(paper_path)) return(x)
      
      str_c("<a href='", paper_path, "'>", x, "</a>")
    }
  } else {
    link_f <- function(x) x
  }
  
  authors <- rbind.fill(lapply(entry$author, as.data.frame))
  author_string <- str_c(authors$given, " ", authors$family, collapse = ", ")
  
  thumb_path <- file.path("thumbs", str_c(entry$key, ".png"))
  if (file.exists(thumb_path)) {
    thumb <- link_f(str_c("  <img src='/", thumb_path, "' width='100' />\n"))
  } else {
    thumb <- NULL
  }
  

  paper_path <- file.path("papers", str_c(entry$key, ".pdf"))
  links <- c(
    if (file.exists(paper_path))
      str_c("<a href='", paper_path, "'>pre-print</a>"),
    if (!is.null(entry$url)) 
      str_c("<a href='", entry$url, "'>from publisher</a>"),
    if (!is.null(entry$doi)) 
      str_c("<a href='http://dx.doi.org/", entry$doi, "'>via doi</a>\n")
    )
  if (length(links) > 0) {
    download <- str_c("<p class='download'><span>Download</span>: \n", 
      str_c(links, collapse = " | "), "</p>\n")    
  } else {
    download <- NULL
  }

  citation <- cite(entry)
  
  str_c(
    "<div class='citation'>\n",
    thumb,
    "  <ul>\n",
    "    <li class='author'>", author_string, ".</li>\n",
    "    <li class='title'>", link_f(entry$title), ".</li>\n",
    if (!is.null(citation)) 
    str_c("    <li class='citation'>", citation, "</li>\n"),
    if (!is.null(entry$note)) 
    str_c("    <li class='note'>[", entry$note, "]</li>\n"),
    "  </ul>\n",
    download,
    "  <br clear='all' />",
    "</div>\n"
  )
}

as.data.frame.person <- function(x) {
  data.frame(Filter(Negate(is.null), unclass(x)[[1]]))
}
