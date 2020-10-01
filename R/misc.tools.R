#could try to splat_function this? 

#based on mog's method
combinations <- function(..., link = "<<>>", data = NULL){
  #first draft
  temp <- try(data.frame(...), silent = TRUE)
  if(class(temp)[1]=="try-error") stop("cannot combine")
  #this makes a to b = b to a
  apply(temp, 1, function(x) paste(sort(x), collapse = link))
}

#that gets bits of combinations out of combinations...
uncombine <- function(x, n =1, splt = "<<>>")
  unlist(lapply(strsplit(x, splt), function(y) y[n]))
