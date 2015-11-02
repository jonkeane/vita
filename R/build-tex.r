orderOfTypes <- c("Journal articles", "Proceedings", "Book chapters", "Conference presentations", "Invited talks", "Posters", "Qualifying papers", "Software")

#' Make vita.
#'
#' @importFrom bibtex read.bib
#' @import stringr
#' @importFrom brew brew
#' @export
make_cvtex <- function(bib = "~/Dropbox/web/vitaKeane/keane.bib", path = "~/Dropbox/web/vitaWeb", templates = "~/Dropbox/web/vitaKeane", texName = "keane-cv.tex", clean = FALSE, compile=TRUE, texBin = "/Library/TeX/texbin/lualatex") {

  old <- setwd(path)
  on.exit(setwd(old))

  if (clean){
    deBibdesk(bib) # only necesary if running alone.
  }

  me <- bibtex::read.bib("me.bib", "UTF-8")

  # remove entries with :nocv at the end of their keys
  me <- purrr::discard(me, function(x) grepl(":nocv$", x$key))

  type <- vapply(me, groupIDr, texCategories, FUN.VALUE=character(1))
  # splits me into groups with membership defined by the vector in types
  by_type <- split(me, type)

  # run through the different types by name, specified to get the right order in orderOfTypes.
  foundTypes <- unique(type)
  headings <- subset(orderOfTypes, orderOfTypes %in% foundTypes)

  content <- c()
  for(type in headings) {
    entries <- by_type[[type]]

    by_year <- sapply(sortByYear(entries), sortByMonth, simplify=FALSE)

    # Most of this is specific to the template used
    content <- append(content, str_c("\\vspace{2ex}
\\pdfbookmark[1]{",type,"}{",type,"}
\\subsection*{",type,"}"))
    content <- append(content, "\\begin{hangparas}{1em}{1}")
    content <- append(content, str_c(vapply(by_year, render_year_tex, character(1)),
          collapse = "

"))
    content <- append(content, "
\\end{hangparas}
         ")
  }

  values <- new.env(parent = globalenv())
  # bold my name
  content <- str_replace_all(content, str_c("(",author,")"), "\\\\textbf{\\1}")
  values$papers <- str_c(content, collapse = "
")

  brew::brew(file.path(templates, "cvtemplate.tex"),
         file.path(path, texName), envir = values)

  if (compile) {
    cmd <- str_c(texBin, texName, sep=" ")
    out <- lapply(cmd, system)
    if (out != 0 ){
      print(str_c("There was an error running ",texBin," on the file ",texName,"."))
    } else {
      # remove extra tex files
      baseName <- strsplit(texName,".",fixed=TRUE)[[1]][1]
      unlink(str_c(baseName,".out"))
      unlink(str_c(baseName,".aux"))
      unlink(str_c(baseName,".log"))
    }


  }

}

# These should be rewritten to be single function.
render_year_tex <- function(entries) {
  str_c(vapply(entries, render_entry_tex, character(1)), collapse = "

        \n")
}

render_entry_tex <- function(entry) {
  str_c(vapply(entry, cite, citeType="tex", character(1)), collapse = "\n")
}
