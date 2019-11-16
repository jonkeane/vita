sortByYear <- function(entries) {
  year <- vapply(entries, function(x) x$year, FUN.VALUE = character(1))
  rev(split(entries, year))
}

# generate a named set of the month numbers, by name as well as by abbreviation. Add a 0 to match things with no months.
monthNameList <- c(1:12)
names(monthNameList) <- month.name
monthAbbList <- c(1:12)
names(monthAbbList) <- month.abb
monthList <- c(monthNameList,monthAbbList)
monthList['noMonth'] <- 13


sortByMonth <- function(by_year) {
  # if the month is null, put no month, will be at the beginning.
  months <- unlist(sapply(by_year, function(x) ifelse(is.null(x$month),'noMonth',x$month)))
  # convert to numbers using the monthList
  months <- monthList[months]
  by_month <- rev(by_year[order(months)])
  by_month
}

#  sapply(sortByYear(entries), sortByMonth) ->foo # example
