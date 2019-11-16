#  a list that assigns bibentries to different categories based primerly on their bibtype, although any combination can be used. Additionally, a bibtag ban be paired with a vector of different values, any one of these will be accepted. the syntax is:
#
#  "[bibtextype]" = "category name" # for simple all bibtextypes in one category
#  
#   "[bibtextype]" = list("category name" = list("bibtex tag0" = "value0",
#                                                "bibtex tag1" = "value1",
#                                                "bibtex tag2" = c("value2a",
#                                                                  "value2b")
#                                                ))
#
# All bibentires must be assigned to one and only one category, or errors will ensue. 

texCategories <- list(
  "Article" = "Journal articles", 
  "Book" = "Books", 
  "InBook" = "Book chapters", 
  "InProceedings" = "Proceedings", 
  "MastersThesis" = "Qualifying papers", # changed
  "PhdThesis" = "PhD Thesis", # changed
  "TechReport" = "Technical reports" , 
  "Misc" = list("Conference presentations" = list("howpublished" = "conference presentation"),
                "Posters" = list("howpublished" = "poster"),
                "Invited talks" = list("howpublished" = c("invited talk", "workshop")),
                "Software" = list("howpublished" = "software"))
  )


webCategories <- list(
  "Article" = "Article", 
  "Book" = "Book", 
  "InBook" = "Book Chapters", 
  "InProceedings" = "InProceedings", 
  "MastersThesis" = "Theses",
  "PhdThesis" = "Theses",
  "TechReport" = "TechReport" , 
  "Misc" = list("PresPost" = list("howpublished" = c("conference presentation", "poster", "invited talk", "workshop")), "Software" = list("howpublished" = c("software")))
  )
  
groupIDr <- function(bib, bibTyper) {
  entry <- bib[[1]]
  groupList <- bibTyper[[entry$bibtype]]
  
  group <-if (is.list(groupList)) {
    names(groupList[
      sapply( 
        sapply(groupList, function(lis) any(unclass(entry)[[1]][names(lis)] %in% unlist(lis)))
### This code will separate the keyword field, and match across the sets, however it is very messy and fragile.        
#         sapply(groupList, function(lis) { 
#           any(
#             ifelse(names(lis)=="keywords",
#                    unlist(strsplit(unlist(unclass(entry)[[1]][names(lis)]), " *, *")),
#                    unclass(entry)[[1]][names(lis)]
#                    ) %in% unlist(lis))
#           })
        ,all)
      ])
  } else {
    groupList
  }
  group
}

# vapply(me, groupIDr, texCategories, FUN.VALUE=character(1)) # for testing
# sapply(me, groupIDr, texCategories, simplify=FALSE) # for testing with nonmatches

# vapply(me, groupIDr, webCategories, FUN.VALUE=character(1)) # for testing
# sapply(me, groupIDr, webCategories, simplify=FALSE) # for testing with nonmatches
