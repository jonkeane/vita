titles <- c(
  "Article" = "Journal articles",
  "Book" = "Books",
  "Book Chapters" = "Book Chapters",
  "InProceedings" = "Proceedings",
  "Theses" = "Theses", # changed
  "TechReport" = "Technical reports",
  "PresPost" = "Presentations and posters",
  "Software" = "Software",
  "Misc" = "Miscellaneous"
  )

paths <- c(
  "Article" = "articles",
  "Book" = "books",
  "Book Chapters" = "chapters",
  "InProceedings" = "proceedings",
  "Theses" = "theses",
  "TechReport" = "tech-reports",
  "PresPost" = "presentations",
  "Software" = "software",
  "Misc" = "misc"
  )

author<-"Jonathan Keane"


#' Make vita.
#'
#' @importFrom bibtex read.bib
#' @import stringr
#' @importFrom brew brew
#' @export
make_vita <- function(bib = "~/Dropbox/web/vitaKeane/keane.bib", path = "~/Dropbox/web/vitaWeb", templates = "~/Dropbox/web/vita", clean = FALSE) {

  old <- setwd(path)
  on.exit(setwd(old))

  #check for the appropriate directories, delete the contents of papers add in option for deleting the contents of the rest.
  dir.create("pdfs", showWarnings = FALSE)
  dir.create("papers", showWarnings = FALSE)
  dir.create("thumbs", showWarnings = FALSE)
  # clean the papers directory so only papers found will be there
  unlink("papers/*")
  # clean other directories if wanted
  if (clean) {
    unlink("pdfs/*")
    unlink("thumbs/*")
  }

  # extracts file locations from bibdesk hashes, and copies the file to path
  me <- deBibdesk(bibtex::read.bib(bib, "UTF-8"), dirname(bib))

  # remove entries with :noweb at the end of their keys
  me <- purrr::discard(me, function(x) grepl(":noweb$", x$key))

  me <- lapply(me, clean_bib)
  me <- lapply(me, copyPDFs)

  type <- vapply(me, groupIDr, webCategories, FUN.VALUE=character(1))
  # splits me into groups with membership defined by the vector in types
  by_type <- split(me, type)

  # subset titles and paths to be only those that are found.
  foundTypes <- unique(type)


  # this reassigns path and titles in the local environment, which is then passed on to render_index(), render_entry_page(), and render_page()
  paths <- subset(paths, names(paths) %in% foundTypes)
  titles <- subset(titles, names(titles) %in% foundTypes)


  # make thumbnails for each available pdf
  make_thumbs()

  # go throough me by id.
  for(id in seq_along(me)) {
    entry <- me[[id]]
    # fail if the key has not been cleansed of a colon. (necesary?)
    if (str_detect(entry$key, ":")) next()

    # generate the individual paper page
    page <- render_entry_page(entry, titles, paths)

    # setup for brew (?)
    values <- new.env(parent = globalenv())
    values$title <- entry$title
    values$content <- page$content
    # values$sidebar <- "&nbsp;" # for no sidebar
    values$sidebar <- page$sidebar
    values$header <-  page$header

    # actually brew the file.
    brew::brew(file.path(templates, "template.html"),
      str_c("papers/", entry$key, ".html"), envir = values)
  }

  # run through the different types by name
  for(type in names(titles)) {

    page <- render_page(by_type[[type]], type, titles, paths)

    values <- new.env(parent = globalenv())
    values$title <- str_c(titles[type], " by ",author,".")
    values$content <- page$content
    values$sidebar <- page$sidebar
    values$header <-  page$header

    brew::brew(file.path(templates, "template.html"),
      str_c(paths[type], ".html"), envir = values)
  }

  page <- render_index(me, titles, paths)
  brew::brew(file.path(templates, "template.html"), "index.html",
    envir = list2env(page))

}

copyPDFs <- function(entry){
  if ({!file.exists(str_c("pdfs/", entry$key, ".pdf"))}&{!is.null(entry$file)}){
      message("file doesn't exist and moving")
      file = shQuote(entry$file)
      message(file)
      cmd <- str_c("cp ",file, " pdfs/", entry$key, ".pdf")
      lapply(cmd, system)
  }
  if (!is.null(entry$file)) {
    entry$file <- as.null()
  }
  entry
}

#' @importFrom tools file_path_sans_ext
make_thumbs <- function() {
  papers <- tools::file_path_sans_ext(dir("pdfs", "*.pdf"))
  thumbs <- tools::file_path_sans_ext(dir("thumbs"))

  needed <- setdiff(papers, thumbs)
  if (length(needed) == 0) return()

  cmd <- str_c("convert ", "pdfs/", needed, ".pdf[0] -antialias -thumbnail '100' ",
    " thumbs/", needed, ".png")
  lapply(cmd, system)

  invisible()
}

clean_bib <- function(entry) {
  #entry$title <- str_replace_all(entry$title, "[{}]", "")
  entry$key <- str_replace_all(entry$key, ":", "")
  if (!is.null(entry$note)) {
    entry$note <- str_replace_all(entry$note, "\\[|\\]", "")
  }
  if (!is.null(entry$pages)) {
    entry$pages <- str_replace_all(entry$pages, "-|--", "&#8211;")
  }

  # Maybe this should be implemented elsewhere, as a text parser for only display text?
  entry$title <- gsub("\\{\\\\textsc.(.*?)\\}\\}", "\\U\\1", entry$title, fixed=FALSE, perl = TRUE)
  if (!is.null(entry$chapter)) {
  entry$chapter <- gsub("\\{\\\\textsc.(.*?)\\}\\}", "\\U\\1", entry$chapter, fixed=FALSE, perl = TRUE)
  }
  if (!is.null(entry$booktitle)) {
    entry$booktitle <- gsub("\\{\\\\textsc.(.*?)\\}\\}", "\\U\\1", entry$booktitle, fixed=FALSE, perl = TRUE)
  }
  if (!is.null(entry$note)) {
  entry$note <- gsub("\\{\\\\textsc.(.*?)\\}\\}", "\\U\\1", entry$note, fixed=FALSE, perl = TRUE)
    entry$note <- gsub("\\{\\\\href.(.*?)\\}\\{.*?\\}\\}", "\\1", entry$note, fixed=FALSE, perl = TRUE) # turn into a hrefed url?
  }
  if (!is.null(entry$year)) {
    entry$year <- gsub("\\{(.*?)\\}", "\\1", entry$year, fixed=FALSE, perl = TRUE)
  }


  entry
}

deBibdesk <- function(bib, bib_path) {
  bib <- lapply(bib, function(item) {
    if (!is.null(item$`bdsk-file-1`)){
      plist <- plist:::int_get_plist(openssl::base64_decode(item$`bdsk-file-1`))
      rel_path <- XML::readKeyValueDB(plist)$relativePath
      
      item$file <- file.path(bib_path, rel_path)
    }
    return(item)
  })
  
  return(bib)
}
