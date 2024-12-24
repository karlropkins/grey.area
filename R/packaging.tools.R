####################################################
#' @title packaging.tools
####################################################
#'
#' @name packaging.tools
#' @aliases splat_pack splate_description splat_citation
#' splat_package_intro splat_package_readme splat_package_new
#' splat_function splat_data splat_date splat_version
#' @description Code written when making packages.
#' @param pkg (character) package name.
#' @param pkg.dir (character) main packaging directory.
#' @param overwrite (logical) Overwrite file if already present,
#' default typically FALSE.
#' @param title (character) title of package or document template
#' built as part of splat.
#' @param description (character) text for package description.
#' @param fun (character) name of function to add to package.
#' @param name (character) name of document template if different from
#' item (e.g. function) being added to package.
#' @param data (character) name of data to add to package.
#' @param date (character) date in 'YYYY-MM_DD' format.
#' @param version (character) version in 'n.n.n' (or 'n.n.n.n') format.
#' @param increase (character or numeric) version increase, typically
#' character in 'n.n.n' (or 'n.n.n.n') format, but also allows the
#' numerics 1, 2, 3 or 4 as shorthand for increases of '1', '0.1',
#' '0.0.1' or '0.0.0.1', respectively.
#' @param ... Other arguments passed on.
#'

#######################################
#to do
#######################################
# work to do on everything here...
#######################################
#need to update splat_citation based on CRAN guidance FEB 2023
#    have a template in grey.area
# at some stage need to recheck args
#    all present and right order


#####################################
#to think about
#####################################
#think about save(..., compress='xz')
#    think this is fun???




#####################################
#exported functions
######################################

#' @rdname packaging.tools
#' @importFrom usethis create_package
#' @export
splat_pack <- function(pkg = ".", overwrite = FALSE, pkg.dir =".",
                       ...){

  ##################################
  #this needs pkg set because it makes
  #   pck where there was no package
  ###################################
  #this make it in my pkg in pkg structure
  #    [pkg.dir]\[pkg.name](as project)\[pkg.name] (package)
  ####################################
  #could use missing instead of ="."?
  #could set this somewhere global..?
  if(pkg.dir == ".") pkg.dir <- getwd()
  if(pkg == ".") stop("need pkg (name) to make package")
  pkg.path <- file.path(pkg.dir, pkg)
  if(file.exists(pkg.path))
    if(overwrite){
      #think this is already handled by create_package
      #also not sure it works
      file.remove(pkg.path, overwrite=overwrite)
    } else {
      stop("already exists")
    }
  #make the directory within directory structure
  dir.create(pkg.path)
  #move there
  setwd(pkg.path)
  #build package skeleton
  ##but don't open new project...
  usethis::create_package(pkg, open = FALSE)
  #move down into the package itself
  ##might not be smartest move....
  setwd(file.path(pkg.path, pkg))
  #################################################
  #could get pkg and pass it to each splat_ step...
  #    but from here it is "." not pkg
  #    because we are in package
  #################################################
  #splat reset (have to overwrite) description
  splat_description(overwrite = TRUE, ...)
  #splat citation using description
  splat_citation(overwrite = overwrite, ...)
  #splat package intro using devtools package object...
  splat_package_intro(overwrite = overwrite, ...)
  #splat package readme
  splat_package_readme(overwrite = overwrite, ...)
  #splat package news
  splat_package_news(overwrite = overwrite, ...)
}



#' @rdname packaging.tools
#' @export
splat_description <- function (title = ".", description = ".",
                               pkg = ".", overwrite = FALSE, ...){
  #get pkg...
  pkg <- package_splat_source(pkg)
  #looking for [pkg]/description
  #don't use test_splat_path because it deletes...
  if(!file.exists(file.path(pkg$path, "DESCRIPTION")))
    stop("Halted splat: suspect package (no 'DESCRIPTION')",
         call. = FALSE)
  #####################################
  #NOTE at moment overwrite is not
  #doing anything in this function
  #####################################
  #read in description
  desc <- read.dcf("DESCRIPTION", fields = NULL, all = FALSE,
                   keep.white = NULL)
  #update
  #################################
  #update might need tidying
  #also other DESCRIPTION ARGUMENTS to include/handle...
  #################################
  if(title != ".") desc[colnames(desc)=="Title"] <- title
  if(description != ".") desc[colnames(desc)=="Description"] <- description
  #write that back...
  ##############################
  #query
  ##############################
  #I think assumes we are same directory as DESCRIPTION
  #might be a problem?
  write.dcf(desc, "DESCRIPTION")
}


#' @rdname packaging.tools
#' @export
splat_citation <-
  function (overwrite = FALSE, pkg = ".", ...)
{
  .pkg <- package_splat_source(pkg)
  test_splat_path("CITATION", c(.pkg$path, "inst"), overwrite = overwrite,
                  ...)
  pkg <- .pkg$package
  #title <- if (length(grep(pkg, .pkg$title)) > 0)
  #  .pkg$title
  #else paste(pkg, ": ", .pkg$title, sep = "")
  title <- .pkg$title
  #year <- if ("date" %in% names(.pkg))
  #  format(as.Date(.pkg$date), "%Y")
  #else format(Sys.Date(), "%Y")
  authors <- paste(format(eval(parse(text = .pkg$`authors@r`)),
                          include = c("given", "family")))
  txt.aut <- paste(authors, collapse = ", ")
  txt.aut <- paste("         textVersion = paste(\"", txt.aut,
                   " (\", year, \") \",", sep = "")
  if (length(authors) > 1)
    txt.aut <- gsub(paste(", ", authors[length(authors)],
                          sep = ""), paste(" and ", authors[length(authors)],
                                           sep = ""), txt.aut)
  man.aut <- paste("as.person(\"", authors, "\")", sep = "")
  man.aut[1] <- paste("         author = c(", man.aut[1],
                      ",", sep = "")
  if (length(man.aut) > 1)
    man.aut[2:length(man.aut)] <- paste("                             ",
                                        man.aut[2:length(man.aut)], ",", sep = "")
  man.aut[length(man.aut)] <- gsub("[)],$", ")),", man.aut[length(man.aut)])
  out <- c("citHeader(\"To cite '[packname]' package in publications use:\")",
           "",
           "year <- sub(\"-.*\", \"\", meta$Date)",
           "version <- meta$Version",

           "", "bibentry(bibtype=\"Manual\",", "         title = \"[packname]: [packtitle]\",",
           man.aut, "         year = year,", "         note = paste(\"R package version \",",
           "                    version, sep=\"\"),",
           txt.aut, "                    \"[packname]: [packtitle], \",", "                    \"R package version \",",
           "                    version,\".\", sep=\"\"))",
           "", "citFooter(\"Please cite both '[packname]' and R \",",
           "          \"when using '[packname]'.\",", "          \"For R citation, see \", sQuote(\"citation()\"))")
  out <- gsub("[[]packname[]]", pkg, out)
  out <- gsub("[[]packtitle[]]", title, out)
  #out <- gsub("[[]year[]]", year, out)
  write_splat_content(file.path(.pkg$path, "inst/CITATION"),
                      out)
}


#' @rdname packaging.tools
#' @export
splat_package_intro <- function (name = ".", pkg = ".", overwrite = FALSE,
                                 title = ".", description = ".",
                                 ...){

  ##could shorten function name??
  ##name might not be needed..?
  ##always be [pkg.name]-package for package introduction

  pkg <- package_splat_source(pkg)
  #if not set using pkg-package convention
  if(name == ".") name <- paste(pkg$package, "-package", sep = "")
  ## testing as rationalisation
  test_splat_path(paste(name, ".R", sep=""), c(pkg$path, "R"),
                  overwrite = overwrite, ...)
  #make content
  pkg.name <- pkg$package
  if(title == ".") title <- pkg$title
  title2 <- paste(pkg.name, title, sep=": ")
  if(description == ".") description <- pkg$description
  out <- c("##################################################",
           strwrap(title2, prefix = "#' "),
           "##################################################",
           "#'",
           strwrap(description, prefix = "#' "),
           "#'", "#' This package was splat packed.", "#'",
           "#' @section package functions:",
           "#' [to be added]", "#'",
           paste("#' @name ", pkg.name, "-package", sep=""),
           paste("#' @aliases ", pkg.name, sep=""),
           "#' \"_PACKAGE\"", "NULL")
  destination <- file.path(file.path(file.path(pkg$path, "R"),
                           paste(name, ".R", sep="")))
  write_splat_content(destination, out)
}






#' @rdname packaging.tools
#' @export
splat_package_readme <- function (name = ".", pkg = ".", overwrite = FALSE,
                                 title = ".", description = ".",
                                 ...){

  ##might need to use usethis::use_build_ignore
  ##   and ask it to ignore this file when build package...
  ##could shorten function name??
  ##name might not be needed..?
  ##always be README.md for GITHUB readme

  pkg <- package_splat_source(pkg)
  #if not set using pkg-package convention
  if(name == ".") name <- paste(pkg$package, "-package", sep = "")
  ## testing as rationalisation
  test_splat_path("README.md", c(pkg$path),
                  overwrite = overwrite, ...)
  #make content
  pkg.name <- pkg$package
  if(title == ".") title <- pkg$title
  if(description == ".") description <- pkg$description
  out <- c(paste("# ", pkg.name, sep = ""), "",
  description,
  "", "## Installation", paste("**", pkg.name, "**:", sep=""),
  "", "```{r, eval=FALSE}", "#[installation code]", "```", "",
  "## Background", "This package was created as... [to finish]", "",
  "## Contributing", "Contributions are very welcome [to finish].",
  "", "## License", "[to be added]")
  destination <- file.path(file.path(pkg$path), "README.md")
  write_splat_content(destination, out)
  #hide it from standard compile
  usethis::use_build_ignore("README.md")
}



#' @rdname packaging.tools
#' @export
splat_package_news <- function (name = ".", pkg = ".", overwrite = FALSE,
                                title = ".", description = ".",
                                ...){

  ##might need to use usethis::use_build_ignore
  ##   and ask it to ignore this file when build package...
  ##could shorten function name??
  ##  name might not be needed..?
  ##always be NEWS.md for GITHUB news

  pkg <- package_splat_source(pkg)
  #if not set using pkg-package convention
  if(name == ".") name <- paste(pkg$package, "-package", sep = "")
  ## testing as rationalisation
  test_splat_path("NEWS.md", c(pkg$path),
                  overwrite = overwrite, ...)
  #make content
  pkg.name <- pkg$package
  if(title == ".") title <- pkg$title
  if(description == ".") description <- pkg$description
  out <- c("## v0.0 - Release Notes", "",
           "### Miscellaneous", "",
           "* Package splatted into existance", "")
  destination <- file.path(file.path(pkg$path), "NEWS.md")
  write_splat_content(destination, out)
}



#' @rdname packaging.tools
#' @export
splat_function <- function (fun = ".", overwrite = FALSE, pkg = ".",
                            title = ".", name = ".", ...){

  ##could replace front end with package_splat_source and test_splat_path
  ##could replace end with write_splat_content

  ## fun might not be a good arg name...
  ## rationalise .test and pkg...
  ## could we pass .pkg?

  #check package and path...
  #[pkg]/R/[fun].R
  .pkg <- package_splat_source(pkg)
  #set up title and name
  if(title == "."){
    title <- fun[1]
  }
  doc.name <- if(name != "."){
    name
  } else {
    fun[1]
  }
  if(length(fun) > 1 && name == "."){
    name <- fun[1]
  }
  test_splat_path(paste(doc.name, ".R", sep = ""),
                  c(file.path(.pkg$path), "R"),
                  overwrite = overwrite, ...)
  #check fun is there and function, etc.
  if(all(fun==".")) return(invisible(FALSE))
  #make path to file
  fun.path <- file.path(file.path(.pkg$path, "R"),
                        paste(doc.name, ".R", sep = ""))
  fun.ls <- lapply(fun, function(x){
      if(!exists(x))
        stop("can't find '", x, "': Need a function to splat_function",
             call. = FALSE)
      fun2 <- get(x)
      if(!"function" %in% class(fun2))
        stop("'", x, "' is not a function: Need a function to splat_function",
             call. = FALSE)
      ###################################
      #this seems silly but dump(fun2)
      #names function as fun2 and I
      #can't think of better work around
      ###################################
      #make code
      #make code script...
      dump("fun2", file = fun.path, append=TRUE)
      code <- readLines(fun.path)
      file.remove(fun.path)
      .test <- names(formals(fun2))
      list(name=x, formals=.test, code=code)
  })
  forms <- c()
  for(i in fun.ls){
    forms <- c(forms, i$formals)
  }
  forms <- unique(forms)
  #make doc script
  out <- c(
    "############################################",
    paste("#' @title ", title, sep = ""),
    "############################################",
    "",
    paste("#' @name ", doc.name, sep = ""),
    "#' @description [to be added]"
  )
  if(length(forms)>0)
    for(i in forms){
      out <- c(out, gsub("[[]formal[]]", i, "#' @param [formal] [to be added]"))
    }
  code <- c()
  for(i in fun.ls){
    code <- c(code, "", "#splatted function")
    code <- if(name == "."){
      c(code, "#' @export")
    } else {
      c(code, paste("#' @rdname ", name, sep=""), "#' @export")
    }
    i$code[1] <- paste(i$name, " <- ", sep="")
    code <- c(code, i$code)
  }
  out <- c(out, code, "")
  destination <- file.path(file.path(file.path(.pkg$path, "R"),
                                     paste(doc.name, ".R", sep="")))
  write_splat_content(destination, out)
}


#' @rdname packaging.tools
#' @export
splat_data <- function (data = ".", overwrite = FALSE, pkg = ".",
                            ...){

  ##works on simple examples
  ##very messy - needs work...

  ## rationalise .test and pkg...
  ## could we pass .pkg?

  #check package and path...
  #[pkg]/data/[fun].R
  .pkg <- package_splat_source(pkg)
  ############################
  #tidy this later
  ############################
  #test for data....
  test_splat_path(paste(data, ".rda", sep = ""),
                  c(file.path(.pkg$path), "data"),
                  overwrite = overwrite, ...)
  #test for document...
  test_splat_path(paste(data, ".R", sep = ""),
                  c(file.path(.pkg$path), "r"),
                  overwrite = overwrite, ...)
  #check data is there, etc.
  if(data==".") return(invisible(FALSE))
  if(!exists(data))
    stop("can't find '", data, "': Need data to splat_data",
         call. = FALSE)
  data2 <- get(data)
  ####################################
  #could check class of data2?
  ####################################
  #add data to ~[pkg]/data
  data.path <- file.path(file.path(.pkg$path, "data"),
                        paste(data, ".rda", sep = ""))
  #there must be a nicer way than this...
  lst <- list(data2)
  names(lst) <- data
  save(file = data.path, list=data, envir = list2env(lst),
       compress='xz') #see to think about
  ###################################
  #could use
  ## usethis::use_data(example.data, pkg="just.testing")
  #but guessing I might want to change name of data?

  #make doc script
  #################################################
  #currently rebuilds scripts if you splat new data
  #using overwrite=TRUE. Might want to keep that?
  #if just updating data???
  #################################################
  out <- c(
    "############################################",
    paste("#' @title ", data, sep = ""),
    "############################################",
    "#'",
    paste("#' @name ", data, sep = ""),
    "#' @description splat_packed data [to be described]",
    "#'"
  )

  ##################################
  #tidy this later
  ##################################
  temp <- ""
  test <- try(length(data2), silent=TRUE)
  if(class(test)[1] != "try-error" && !is.null(test)){
    temp <- paste(test, collapse = " x ")
    temp <- paste("(", temp, " long) ")
  }
  test <- try(dim(data2), silent=TRUE)
  if(class(test)[1] != "try-error" && !is.null(test)){
    temp <- paste(test, collapse = "x", sep = "")
    temp <- paste("(", temp, ") ", sep = "")
  }
  out <- c(out,
           paste("#' @format A ", temp, "'", class(data2)[1],
           "' object", sep=""))
  if(!is.null(names(data2))){
    out <- c(out, "#' \\describe{")
    for(i in names(data2))
      out <- c(out,
               paste("#'   \\item{", i, "}{[to document]}", sep=""))
    out <- c(out, "#' }")
  }

  out <- c(out, "#' @source [to doc]", dQuote(data, q = FALSE))
  destination <- file.path(file.path(file.path(.pkg$path, "R"),
                                     paste(data, ".R", sep="")))
  write_splat_content(destination, out)
}





#' @rdname packaging.tools
#' @export
splat_date <- function (title = ".", date = ".",
                        pkg = ".", overwrite = FALSE, ...){

  #adds date to description
  #draft version
  ###############################
  #to do
  ###############################
  #decide how to handle overwrite (currently ignored)
  #allow user to set date other than today (date currently ignored)
  #think about desc reordering (could be unexported fun used by
  #     any decription read/rewriter?)
  #set date in splat_package
  #might be issue with rewrite at end...

  #get pkg...
  pkg <- package_splat_source(pkg)
  #looking for [pkg]/description
  #don't use test_splat_path because it deletes...
  if(!file.exists(file.path(pkg$path, "DESCRIPTION")))
    stop("Halted splat: suspect package (no 'DESCRIPTION')",
         call. = FALSE)
  #####################################
  #NOTE at moment overwrite is not
  #doing anything in this function
  #####################################
  #read in description
  desc <- read.dcf("DESCRIPTION", fields = NULL, all = FALSE,
                   keep.white = NULL)

  #####################################
  #NOTE at moment date is not
  #doing anything in this function
  #####################################
  #update date in description
  cdate <- as.character(Sys.Date())
  if("Date" %in% colnames(desc)){
    desc[colnames(desc)=="Date"] <- cdate
  } else {
    desc <- cbind(desc, cdate)
    colnames(desc)[length(desc)] <- "Date"
   }
  #reorder desc so always have same front end
  desc <- order_splat_description(desc)
  #write that back...b
  ##############################
  #query
  ##############################
  #I think assumes we are same directory as DESCRIPTION
  #might be a problem?
  write.dcf(desc, "DESCRIPTION")
}




#' @rdname packaging.tools
#' @export
splat_version <- function (title = ".", version = ".", increase = ".",
                        pkg = ".", overwrite = FALSE, ...){

  #change or increases version in description
  #draft version
  ###############################
  #to do
  ###############################
  #decide how to handle overwrite (currently ignored)
  #allow user to set version other than today (date currently ignored)
  #think about desc reordering (could be unexported fun used by
  #     any decription read/rewriter?)
  #think about version handling
  #       "+0.1" should change 0.1.999 to 0.2.0
  #set date in splat_package
  #might be issue with rewrite at end...

  #odd but OK!!
  if(is.numeric(increase)){
    increase <- c("1", "0.1", "0.0.1", "0.0.0.1")[increase[1]]
    if(length(increase)<1 || is.na(increase)){
      stop("Halted splat: increase suspect",
           call. = FALSE)
    }
  }
  if(version == "." && increase == "."){
    stop("Halted splat: version or increase needed",
            call. = FALSE)
  }

  #get pkg...
  pkg <- package_splat_source(pkg)

  #looking for [pkg]/description
  #don't use test_splat_path because it deletes...
  if(!file.exists(file.path(pkg$path, "DESCRIPTION")))
    stop("Halted splat: suspect package (no 'DESCRIPTION')",
         call. = FALSE)
  #####################################
  #NOTE at moment overwrite is not
  #doing anything in this function
  #####################################
  #read in description
  desc <- read.dcf("DESCRIPTION", fields = NULL, all = FALSE,
                   keep.white = NULL)

  #####################################
  #NOTE at moment date is not
  #doing anything in this function
  #####################################
  #update version in description

  if("Version" %in% colnames(desc)){
    if(version != "."){
      #check it looks sensible
      temp <- try(as.numeric(strsplit(version, "[.]")[[1]]),
                  silent = TRUE)
      if(class(temp)[1] == "try-error"){
        stop("Halted splat: supplied version suspect [expected 'n.n.n(.n)']",
             call. = FALSE)
      }
      desc[colnames(desc)=="Version"] <- version
    } else {
      temp <- try(as.numeric(strsplit(desc[, "Version"], "[.]")[[1]]),
                  silent = TRUE)
      if(class(temp)[1] == "try-error"){
        stop("Halted splat: description version suspect [expect 'n.n.n(.n)']",
             call. = FALSE)
      }
      inc <- try(as.numeric(strsplit(increase, "[.]")[[1]]),
                 silent = TRUE)
      if(class(inc)[1] == "try-error"){
        stop("Halted splat: supplied increase suspect [expect 'n.n.n(.n)']",
             call. = FALSE)
      }
      if(length(inc)>length(temp)){
        temp <- c(temp, rep(0, length(inc)))[1:length(inc)]
      }
      for(i in 1:length(inc)){
        temp[i] <- temp[i] + inc[i]
        if(inc[i]>0 & length(temp)>i){
          temp[(i+1):length(temp)] <- 0
        }
      }
      temp <- paste(temp, sep = ".", collapse = ".")
      desc[colnames(desc)=="Version"] <- temp
    }
  } else {
    if(version == "."){
      stop("Halted splat: version not found, use version to set",
           call. = FALSE)
    }
    desc <- cbind(desc, version)
    colnames(desc)[length(desc)] <- "Version"
  }

  #order description
  desc <- order_splat_description(desc)
  #write that back...b
  ##############################
  #query
  ##############################
  #I think assumes we are same directory as DESCRIPTION
  #might be a problem?
  write.dcf(desc, "DESCRIPTION")

}










###########################
#unexported code
###########################

#NOTE not exporting but needs devtools
#' @importFrom devtools as.package
package_splat_source <- function(pkg){
  if("package" %in% class(pkg)) pkg else devtools::as.package(pkg)
}

test_splat_path <- function(file = ".", destination = ".",
                                      overwrite = FALSE,
                                      ...){
  #workhore functions for splat_functions
  #not exported
  #NOTE currently no checks that name, etc are all present...

  #file - file name including any extension
  #destination - destination path in form c(package.path, any subdir)
  #content - what to write
  #overwrite - logical
  #check package looks useable...
  .splat.path <- file.path(destination[1], c("R", "src", "data"))
  if (!any(file.exists(.splat.path))) {
    stop("'destination' does not look like a package: no R, src or data",
         call. = FALSE)
  }
  .splat.path <- file.path(destination[1])
  if(length(destination)>1)
    .splat.path <- file.path(.splat.path, destination[2])
  #NOTE might not be smart
  if (!file.exists(.splat.path)){
    dir.create(.splat.path)
  }
  .splat.path <- file.path(.splat.path, file)
  #check if file itself already exists
  if (file.exists(.splat.path)){
    if (overwrite){
      #remove to rewrite
      file.remove(.splat.path)
    } else {
      #stop
      stop("'", file, "' exists and overwrite not allowed",
           call. = FALSE)
    }
  }
}

write_splat_content <- function(destination, content){
  #destination where to write
  #what to write
  #use with test_splat_path
  fileConn<-file(destination, "at")
  writeLines(content, fileConn)
  close(fileConn)
}

order_splat_description <- function(desc){
  #reorder desc so always have order
  #Package, Title, Version, Date, Description, then rest...
  #but might not all be there...
  temp <- unique(c(match(c("Package", "Title", "Version", "Date",
                           "Description"),
                         colnames(desc)),
                   1:length(desc)))
  temp <- temp[!is.na(temp)]
  desc[, temp, drop=FALSE]
}

#not exporting/testing/thinking about this
resplat_data <- function (data = ".", overwrite = TRUE, pkg = ".",
                          ...){

  #replace data without changing documentation

  #check package and path...
  #[pkg]/data/[fun].R
  .pkg <- package_splat_source(pkg)
  ############################
  #tidy this later
  ############################
  #test for data....
  test_splat_path(paste(data, ".rda", sep = ""),
                  c(file.path(.pkg$path), "data"),
                  overwrite = overwrite, ...)
  #test for document...
  ##test_splat_path(paste(data, ".R", sep = ""),
  ##                c(file.path(.pkg$path), "r"),
  ##                overwrite = overwrite, ...)
  #check data is there, etc.
  if(data==".") return(invisible(FALSE))
  if(!exists(data))
    stop("can't find '", data, "': Need data to splat_data",
         call. = FALSE)
  data2 <- get(data)
  ####################################
  #could check class of data2?
  ####################################
  #add data to ~[pkg]/data
  data.path <- file.path(file.path(.pkg$path, "data"),
                         paste(data, ".rda", sep = ""))
  #there must be a nicer way than this...
  lst <- list(data2)
  names(lst) <- data
  save(file = data.path, list=data, envir = list2env(lst),
       compress='xz') #see to think about
  ###################################
  #could use
  ## usethis::use_data(example.data, pkg="just.testing")
  #but guessing I might want to change name of data?

}
