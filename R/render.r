render_page <- function(entries, type, titles, paths) {
  year <- vapply(entries, function(x) x$year, FUN.VALUE = character(1))
  
  by_year <- sapply(sortByYear(entries), sortByMonth)
  
  header <- str_c("<h1>", titles[type], "</h1>\n")
  
  content <- str_c("<hr />",vapply(by_year, render_year, character(1)), 
    collapse = "\n")
  
  content <- str_c("<div class='sort'> <hr class='small'/><h3>By year</h3>\n<ul>\n", 
                   str_c("  <li><h4><a href='#", str_replace_all(names(by_year), " ", ""), "'>", names(by_year),
                         "</a></h4></li>\n", collapse = "\n"), "</ul></div>\n",content)
  
  
  sidebar <- str_c("<hr class='small'/><h3><a href='/'>All publications</a></h3>\n",
    "<ul>\n",
    str_c("  <li><a href='/", paths, ".html'>", titles, "</a></li>\n", collapse = "\n"),
    "</ul>\n"
  )
      
  list(content = content, sidebar = sidebar, header = header)
}

render_year <- function(entries) {
  year <- entries[[1]]$year
  
  str_c("<h2 id='", str_replace_all(year, " ", ""), "'>", year, "</h2>\n", 
    str_c(vapply(entries, render_entry, character(1)), collapse = "\n"))
}
