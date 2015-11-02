cite <- function(entry, citeType) {
  f <- cite_f[[citeType]][[entry$bibtype]]
  if (is.null(f)) return(NULL)
  
  f(entry)
}

cite_f <- list(
  "web" = list(
  Article = function(entry) {
    str_c(
      "<em>", entry$journal, "</em>, ", 
      if (!is.null(entry$volume)) 
        str_c("vol. ", entry$volume, ", "), 
      if (!is.null(entry$number)) 
        str_c("no. ", entry$number, ", "), 
      if (!is.null(entry$pages)) 
        str_c("pp. ", entry$pages, ", "), 
      entry$year, ".")    
  },
  InProceedings = function(entry) {
    str_c(
      "in ",
      if (!is.null(entry$editor)) 
        str_c(str_c(vapply(entry$editor, format, character(1)), collapse = ", "), " eds., "),
      "<em>", entry$booktitle, "</em>, ",
      if (!is.null(entry$number)) 
        str_c("no. ", entry$number, ", "), 
      if (!is.null(entry$pages)) 
        str_c("pp. ", entry$pages, ", "), 
      entry$year, ".")    
  },
  InBook = function(entry) {
    str_c(
      "in ",
      if (!is.null(entry$editor)) 
        str_c(str_c(vapply(entry$editor, format, character(1)), collapse = ", "), " eds., "),
      "<em>", entry$title, "</em>, ",
      if (!is.null(entry$number)) 
        str_c("no. ", entry$number, ", "), 
      if (!is.null(entry$pages)) 
        str_c("pp. ", entry$pages, ", "), 
      entry$year, ".")    
  }
),
"tex" = list(
      #This needs to be fixed for articles to be formatted appropriately
      Article = function(entry) {
        str_c("\\-\\years{", entry$year, "}", str_c(vapply(entry$author, format, character(1)), collapse = ", "), ". ", entry$title, ". ", "In ",
              if (!is.null(entry$editor)) 
                str_c(str_c(vapply(entry$editor, format, character(1)), collapse = ", "), " eds., "),
              "\\textit{", entry$journal, "}",
              if (!is.null(entry$volume)) 
                str_c(", vol. ", entry$volume, ""),
              if (!is.null(entry$number)) 
                str_c(", no ", entry$number, ""), 
              if (!is.null(entry$pages)) 
                str_c(", pp. ", entry$pages, ""),
              ".")
      },
    InProceedings = function(entry) {
      str_c("\\-\\years{", entry$year, "}", str_c(vapply(entry$author, format, character(1)), collapse = ", "), ". ", entry$title, ". ", "In ",
            if (!is.null(entry$editor)) 
              str_c(str_c(vapply(entry$editor, format, character(1)), collapse = ", "), " eds., "),
            "\\textit{", entry$booktitle, "}",
            if (!is.null(entry$number)) 
              str_c(", vol. ", entry$number, ""), 
            if (!is.null(entry$pages)) 
              str_c(", pp. ", entry$pages, ""),
            ".")    
    },
    InBook = function(entry) {
      str_c("\\-\\years{", entry$year, "}", str_c(vapply(entry$author, format, character(1)), collapse = ", "), ". ", entry$chapter, ". ", "In ",
            if (!is.null(entry$editor)) 
              str_c(str_c(vapply(entry$editor, format, character(1)), collapse = ", "), " eds., "),
            "\\textit{", entry$title, "}",
            if (!is.null(entry$number)) 
              str_c(", vol. ", entry$number, ""), 
            if (!is.null(entry$pages)) 
              str_c(", pp. ", entry$pages, ", "),
            ".")    
    },    
    Misc = function(entry) {
      str_c("\\-\\years{", entry$year, "}", str_c(vapply(entry$author, format, character(1)), collapse = ", "), ". ``", entry$title, "'' ", entry$note
            )
    },
    MastersThesis = function(entry) {
      str_c("\\-\\years{", entry$year, "}", str_c(vapply(entry$author, format, character(1)), collapse = ", "), ". \\textit{", entry$title, "} ", entry$school, " ", entry$note
            )
    }
    )
  )
