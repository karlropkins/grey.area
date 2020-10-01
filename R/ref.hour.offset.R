############################
#' @title Local Time Offsets
############################
#'
#' @description `ref.hour.offset` is a lookup table of local time
#' offsets.
#'
#' @format A `data.frame` 38 locations and associated time offsets:
#' \describe{
#' \item{local}{(Character) The location for which an offset value is
#' reported.}
#' \item{hour.offset}{(Numeric) The time offset in hours associated
#' with \code{local}.}
#' }
#' @details \code{ref.hour.offset} is a lookup table for use with
#' \code{\link{calcNOAASunrise}} and \code{\link{hour.offset}}.
#' @source NOAA assignment table:
#' http://www.srrb.noaa.gov/highlights/sunrise/timezone.html.
#' @examples
#' #basic structure
#' head(ref.hour.offset)
#' #usage
#' hour.offset("greenwich")
#' hour.offset("india")
#' hour.offset("hawaii")
"ref.hour.offset"
