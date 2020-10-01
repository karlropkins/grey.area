####################################################
#' @title sanwal.code
####################################################
#'
#' @name sanwal.code
#' @aliases makeAQArchives makeArchive getArchiveDir getFromArchive
#' testInArchive archiveThis mergeAQAndMet
#' @description Code written for Sanwal for archive build for
#' data analysis.
#' @param pass (character) function output control; what to do
#' if something unexpected happens.
#' @param type (character) data type, aq, met, merge or misc.
#' @param code (character) the names of the dataset to select,
#' typically the code of the aq data.
#' @param obj (data.frame or similar) the dataset, if, e.g., to
#' archive if archiving data.
#' @param aq (data.frame or similar) a dataset of air quality data.
#' @param met (data.frame or similar) a dataset of meteorological
#' data.

#######################################
#to do
#######################################
# work to do on everything here...
#######################################
# at some stage need to recheck args
#    all present and right order
# rethink structure with regards to pass arg...
#    and type, and aqmetmerge

#####################################
#exported functions
######################################

#' @rdname sanwal.code
#' @export
#make AQ archive directories
makeAQArchives <- function(pass=""){
  #assuming these are going in this directory
  if(!"MISC_ARCHIVE" %in% dir())
    dir.create("MISC_ARCHIVE")
  if(!"AQ_ARCHIVE" %in% dir())
    dir.create("AQ_ARCHIVE")
  if(!"MET_ARCHIVE" %in% dir())
    dir.create("MET_ARCHIVE")
  if(!"AQMETMERGE_ARCHIVE" %in% dir())
    dir.create("AQMETMERGE_ARCHIVE")
  return(invisible(NULL))
}

#' @rdname sanwal.code
#' @export
#make archive directories
makeArchive <- function(type=NULL){
  #make this archive as local directory
  if(!is.character(type)){
    stop("type need")
  }
  type <- paste(toupper(type), "_ARCHIVE", sep="")
  if(!type %in% dir())
    dir.create(type)
  return(invisible(NULL))
}


#' @rdname sanwal.code
#' @export
#get archive address
getArchiveDir <- function(type, pass=""){
  home <- getwd()
  my.dir <- toupper(type)
  if(my.dir=="MERGE")
    my.dir <- "AQMETMERGE"
  my.dir <- paste(home, "/", my.dir, "_ARCHIVE/", sep="")
  if(!dir.exists(my.dir)){
    if(pass=="type") {
      invisible(return(NULL))
    } else {
      stop("archive not found")
    }
  }
  my.dir
}

#' @rdname sanwal.code
#' @export
#get code from archive
getFromArchive <- function(code, type, pass="code"){
  my.dir <- getArchiveDir(type)
  out <- try(readRDS(paste(my.dir, code, ".rds", sep="")),
             silent=TRUE)
  if(class(out)[1]=="try-error") {
    if("code" %in% pass) return(NULL) else
      stop("code not found")
  }
  out
}

#' @rdname sanwal.code
#' @export
#test file exists
testInArchive <- function(code, type, pass){
  my.dir <- getArchiveDir(type, pass)
  paste(code, ".rds", sep="") %in% dir(my.dir)
}

#' @rdname sanwal.code
#' @export
#archive this
archiveThis <- function(obj, code, type){
  saveRDS(obj, paste(getArchiveDir(type),
                     code, ".rds", sep=""))
}

#' @rdname sanwal.code
#' @importFrom dplyr left_join
#' @export
#merge aq and met
mergeAQAndMet <- function(aq, met){
  #had to tidy this several times
  #because name overlaps can be different
  #so using dplyr select might not always work...
  ##  aq <- select(aq, -wd, -ws)
  ##  #(some do and some don't have amb_temp)
  ##  if("amb_temp" %in% names(aq)) aq <- select(aq, -amb_temp)

  #because met and aq both have code
  #   i want to keep aq code as code
  if("code" %in% names(met))
    names(met)[names(met)=="code"] <- "met_code"
  #next assuming for the rest that
  #what is met trumps what is in aq
  temp <- c("date", names(aq)[!names(aq) %in% names(met)])
  aq <- aq[, temp]
  dplyr::left_join(aq, met, by = "date")
}



































###########################
#unexported code
###########################

#nothing yet

#NOTEs

#needs dplyr but just loads left_join
