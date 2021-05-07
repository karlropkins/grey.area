##############################
#' @title grey.area misc.tools
##############################
#'
#' @description Some misc functions.
#' @name misc.tools
#' @aliases combinations uncombine
#' @param ... Unspecified or additional arguments. For
#' \code{combinations} and \code{uncombine} vectors to
#' be combined or uncombined, respectively.
#' @param link (character) For \code{combinations}, the
#' string to use as a separator when combining vectors.
#' @param data (data.frame or similar) If supplied,
#' the source for vectors to be combined.
#' @param x (vector) For \code{uncombine}, a vector of
#' previously combined terms to be uncombined.
#' @param n (numeric) For \code{uncombine}, the combined
#' vector to be extracted. The default \code{1} extracts
#' the first vector of combined terms.
#' @param splt (character) For \code{uncombine}, the
#' separator previously used to combine vectors, used to
#' split vectors when uncombining terms.
#' @author Katrina Hemingway and Karl Ropkins
#' @keywords methods
#' @note These are misc functions. They might need
#' tidying
#'
#' \code{combinations} merges supplied vectors but assumes
#' the terms in the supplied vectors are equivalent. This
#' is a little of a specialist function but if you ever
#' have a case where e.g. you want to group journeys
#' between locations need a-to-b and b-to-a to be merged
#' as a single case journeys-between-a-and-b...
#'
#' \code{uncombine} extracts terms from a previous combined
#' vector.
#'
#' @return \code{combinations} and \code{uncombine} return
#' vectors of combined or uncombined terms, respectively.
#' @examples
#' a <- 1:5
#' b <- 5:1
#' #pasting combines two vectors
#' #making  e.g. 1 + 5 and 5 + 1 unique cases
#' paste(a,b)
#' #combinations makes cases where these are equivalent
#' combinations(a,b)

#########################################
#NOTES:
########################################
#to do
#combinations and uncombine need tidying
#   both were written to solve a specific
#   problem. names need deciding and
#   args might need tidying
#########################################
#think about


#########################################
#########################################
#combinations
#########################################
#########################################

# v0.0.2 km + kr (01/10/2020)

#based on mog's method
#merges two or more vectors where
#   a+b and b+a are on case a&b...

#' @rdname misc.tools
#' @export
combinations <- function(..., link = "<<>>", data = NULL){
  #first draft
  temp <- try(data.frame(...), silent = TRUE)
  if(class(temp)[1]=="try-error") stop("cannot combine")
  #this makes a to b = b to a
  apply(temp, 1, function(x) paste(sort(x), collapse = link))
}

#gets bits of combinations out of combinations...

#' @rdname misc.tools
#' @export
uncombine <- function(x, n =1, splt = "<<>>")
  unlist(lapply(strsplit(x, splt), function(y) y[n]))
