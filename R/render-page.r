
render_entry_page <- function(entry, titles, paths) {
  out <- render_entry(entry, link = "pdf")
  
  
  if (!is.null(entry$abstract)) {
    abstract <- str_replace(entry$abstract, "\n\n", "</p>\n<p>")
    out <- str_c(out, "<p class='abstract'>", abstract , "</p>\n\n")
  }
  
  # get rid of extraneous bibtex fields
  entry$abstract <- NULL
  entry$"date-added" <- NULL
  entry$"date-modified" <- NULL
  
  out <- str_c(out, 
    "<pre class='bibtex'>",
    str_c(toBibtex(entry), collapse = "\n"), 
    "</pre>\n")
  
  header <- str_c("<h1>", ifelse(entry$bibtype=="InBook",entry$chapter,entry$title), "</h1>\n")
  
  sidebar <- str_c("<hr class='small'/><h3><a href='/'>All publications</a></h3>\n",
                   "<ul>\n",
                   str_c("  <li><a href='/", paths, ".html'>", titles, "</a></li>\n", collapse = "\n"),
                   "</ul>\n"
                   )
  
  list(content = out, header = header, sidebar = sidebar)
}