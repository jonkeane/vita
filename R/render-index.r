render_index <- function(entries, titles, paths, selectedPubs=FALSE) {
  
  if(selectedPubs == TRUE) {
### Original code for displaying selected pubs  ### begin ###
  selected <- Filter(function(x) !is.null(x$selected), entries)  
  year <- vapply(selected, function(x) as.numeric(x$year), numeric(1))
  selected <- selected[order(-year)]
  
  content <- str_c(
    #"<h2>Selected publications</h2>\n",
    str_c(vapply(selected, render_year, character(1)), 
      collapse = "\n"))
### Original code for displaying selected pubs  ### end ###
  } else{
### Code for displaying all publications ### begin ### 
  by_year <- sapply(sortByYear(entries), sortByMonth)
    
  content <- str_c("<hr />",vapply(by_year, render_year, character(1)), 
                   collapse = "\n")
  
  content <- str_c("<div class='sort'> <hr class='small'/><h3>By year</h3>\n<ul>\n", 
                   str_c("  <li><h4><a href='#", str_replace_all(names(by_year), " ", ""), "'>", names(by_year),
                         "</a></h4></li>\n", collapse = "\n"), "</ul></div>\n",content)
### Code for displaying all pbulications ### end ### 
  }
  

  sidebar <- str_c(      
    "<hr class='small'/><h3>All publications</h3>\n",
    "<ul>\n",
    str_c("  <li><a href='", paths, ".html'>", titles, "</a></li>\n", collapse = "\n"),
    "</ul>\n"
  )
  
  header <- "<h1>Academic publications</h1>\n"
  title <- "Academic publications"
 
  list(content = content, sidebar = sidebar, header = header, title = title)
}