#######################################
#' @title Data import functions
#######################################
#' @name data.import
#' @aliases importUKNAMetals importUKNAPAHs

#' @description Some functions to import data from held in on-line data
#' archives.

#' @param site (character) The identifier code(s) for the site(s) the data
#' is to be downloaded from.
#' @param pollutant (numeric) The pollutant identifier(s) of the pollutant(s)
#' to download data for.
#' @param year (numeric) The year(s) to download data for.
#' @param ... (other arguments) currently ignored.

#' @return The import functions typically return the request data as a
#' \code{data.frame} if data is available for requested site/pollutant/year
#' combination. In some cases the \code{data.frame} may be set up
#' for use with use with other packages.
#'
#' \code{importUKNAMetals}: import function for metal data from the UK
#' Non-automatic Heavy Metals Network.
#' Previously called the Urban/Industrial Heavy Metals Network and Rural
#' Heavy Metals Network.
#'
#' See https://uk-air.defra.gov.uk/networks/network-info?view=metals for
#' details and source information.
#'
#' \code{importUKNAPAHs}: import function for metal data from the UK
#' Non-automatic Polycyclic Aromatic Hydrocarbons (PAH) Network.
#'
#' see https://uk-air.defra.gov.uk/networks/network-info?view=pah for details
#' and source information.

##############################
# to think about
##############################

# need to decide how to document (and/or handle ??) site and pollutant
#        identifiers currently remarked in function...

# these could probably be faster
#        options to speed up ???

# include the tidy PAHs code from
#       ~pkg/respeciate/_projects/marylebone03/_marylebone_data_builds_03.Rmd
#

#' @note These functions were developed while working on projects that
#' required multiple downloads of data from these sources. The code is
#' shared freely but with the caveat that I am not the data provider.
#' So:
#'
#' (a) You should cite both this package and the data source (identified
#' for each function), and
#'
#' (b) You should be aware that they only work for as long as the structure
#' of the data in the archive remains as it was when the code was written,
#' and neither I or your have any control in that...
#'







##########################################
# getUKgetUKNAMetals
#########################################

# get metal archive data

## examples - don't run in package (online)

# getUKNAMetals("UKA00315", 1032, 2018)

## <option value="UKA00315">London Marylebone Road</option>
## Metals: Arsenic, Cadmium, Chromium, Cobalt, Copper, Iron, Lead,
##         Manganese, Nickel, Selenium, Vanadium, Zinc
#  sites <- "UKA00315"
#  pols <- c(1032, 1033, 1034, 1055, 262, 356, 263, 1035, 1036, 1378, 1038, 264)
#  years <- 2011:2022
#  out <- importUKNAMetals(site, pols, years)
#  saveRDS(out, "uk.metals.1.rds")

#' @rdname data.import
#' @export

importUKNAMetals <- function(site, pollutant, year, ...){
  #wrapper for unexported getUKNANMetal
  .sites <- lapply(site, function(s){
    .pols <- lapply(pollutant, function(p){
      .years <- lapply(year, function(y){
        temp <- try(getUKNANMetal(s, p, y),
                    silent=TRUE)
        if(class(temp)[1]=="try-error") {
          warning("bad source")
          NULL
        } else {
          temp
        }
      })
      .years <- do.call(rbind, .years)
    })
    .pols <- do.call(rbind, .pols)
  })
  out <- do.call(rbind, .sites)
  out
}

# unexported single site, pollutant and year download function
#    above is just a wrapper for this...

getUKNANMetal <- function(site, pollutant, year, ...){

  #example download code

  #https://uk-air.defra.gov.uk/data/non-auto-data?uka_id=UKA00315&view=data&network=metals&year=2018&pollutant=1032#view
  #https://uk-air.defra.gov.uk/data/non-auto-data?uka_id=UKA00315&view=data&network=metals&year=2018&pollutant=1033#view
  #https://uk-air.defra.gov.uk/data/non-auto-data?uka_id=UKA00315&view=data&network=metals&year=2018&pollutant=1034#view
  #https://uk-air.defra.gov.uk/data/non-auto-data?uka_id=UKA00315&view=data&network=metals&year=2018&pollutant=1055#view
  #https://uk-air.defra.gov.uk/data/non-auto-data?uka_id=UKA00315&view=data&network=metals&year=2018&pollutant=262#view
  #https://uk-air.defra.gov.uk/data/non-auto-data?uka_id=UKA00315&view=data&network=metals&year=2018&pollutant=356#view
  #https://uk-air.defra.gov.uk/data/non-auto-data?uka_id=UKA00315&view=data&network=metals&year=2018&pollutant=263#view
  #https://uk-air.defra.gov.uk/data/non-auto-data?uka_id=UKA00315&view=data&network=metals&year=2018&pollutant=1035#view
  #https://uk-air.defra.gov.uk/data/non-auto-data?uka_id=UKA00315&view=data&network=metals&year=2018&pollutant=1036#view
  #https://uk-air.defra.gov.uk/data/non-auto-data?uka_id=UKA00315&view=data&network=metals&year=2018&pollutant=1378#view
  #https://uk-air.defra.gov.uk/data/non-auto-data?uka_id=UKA00315&view=data&network=metals&year=2018&pollutant=1038#view
  #https://uk-air.defra.gov.uk/data/non-auto-data?uka_id=UKA00315&view=data&network=metals&year=2018&pollutant=264#view

  #example sites
  #<option value="UKA00451">Auchencorth Moss</option>
  #<option value="UKA00126">Avonmouth 1</option>
  #<option value="UKA00127">Avonmouth 2</option>
  #<option value="UKA00455">Banchory</option>
  #<option value="UKA00464">Beacon Hill</option>
  #<option value="UKA00212">Belfast Centre</option>
  #<option value="UKA00465">Bristol Avonmouth</option>
  #<option value="UKA00466">Bristol Hallen</option>
  #<option value="UKA00084">Britannia 1</option>
  #<option value="UKA00085">Britannia 2</option>
  #<option value="UKA00073">Brookside 1</option>
  #<option value="UKA00467">Brookside 2</option>
  #<option value="UKA00453">Cardiff</option>
  #<option value="UKA00468">Cardiff Rumney</option>
  #<option value="UKA00143">CBS 1</option>
  #<option value="UKA00144">CBS 2</option>
  #<option value="UKA00009">Central London</option>
  #<option value="UKA00514">Chadwell St Mary</option>
  #<option value="UKA00604">Chesterfield Loundsley Green</option>
  #<option value="UKA00614">Chilbolton Observatory</option>
  #<option value="UKA00459">Cockley Beck</option>
  #<option value="UKA00149">Cohen 1</option>
  #<option value="UKA00150">Cohen 2</option>
  #<option value="UKA00151">Cohen 3</option>
  #<option value="UKA00145">Cornwall Works 1</option>
  #<option value="UKA00146">Cornwall Works 2</option>
  #<option value="UKA00062">Cottered</option>
  #<option value="UKA00325">Cwmystwyth</option>
  #<option value="UKA00516">Dartford Bean</option>
  #<option value="UKA00481">Detling</option>
  #<option value="UKA00074">Elkington 1</option>
  #<option value="UKA00075">Elkington 2</option>
  #<option value="UKA00076">Elkington 3</option>
  #<option value="UKA00131">Elswick 1</option>
  #<option value="UKA00132">Elswick 2</option>
  #<option value="UKA00133">Elswick 3</option>
  #<option value="UKA00164">Elswick 4</option>
  #<option value="UKA00165">Elswick 5</option>
  #<option value="UKA00469">Elswick 6</option>
  #<option value="UKA00172">Elswick 7</option>
  #<option value="UKA00130">Eskdalemuir</option>
  #<option value="UKA00606">Fenny Compton</option>
  #<option value="UKA00470">Glasgow</option>
  #<option value="UKA00047">Harwell</option>
  #<option value="UKA00147">Hawthorne 1</option>
  #<option value="UKA00148">Hawthorne 2</option>
  #<option value="UKA00461">Heigham Holmes</option>
  #<option value="UKA00063">Hitchin</option>
  #<option value="UKA00077">IMI 1</option>
  #<option value="UKA00522">IMI 2</option>
  #<option value="UKA00078">IMI 3</option>
  #<option value="UKA00079">IMI 4</option>
  #<option value="UKA00158">IMI 5</option>
  #<option value="UKA00174">IMI 6</option>
  #<option value="UKA00471">IMI Refiners Walsall</option>
  #<option value="UKA00457">Inverpolly</option>
  #<option value="UKA00080">ISC 1</option>
  #<option value="UKA00081">ISC 2</option>
  #<option value="UKA00082">Johnson Matthey 1</option>
  #<option value="UKA00083">Johnson Matthey 2</option>
  #<option value="UKA00058">Leeds Vicar Lane</option>
  #<option value="UKA00059">London Brent</option>
  #<option value="UKA00370">London Cromwell Road 2</option>
  #<option value="UKA00315">London Marylebone Road</option>
  #<option value="UKA00435">London Westminster</option>
  #<option value="UKA00166">Lough Navar</option>
  #<option value="UKA00068">Manchester Wythenshawe</option>
  #<option value="UKA00456">Monkswood</option>
  #<option value="UKA00034">Motherwell Centre</option>
  #<option value="UKA00542">Motherwell South</option>
  #<option value="UKA00066">Newcastle</option>
  #<option value="UKA00070">North Petherton</option>
  #<option value="UKA00065">North Tyneside</option>
  #<option value="UKA00271">Penallt</option>
  #<option value="UKA00560">Pontardawe Brecon Road</option>
  #<option value="UKA00557">Pontardawe Tawe Terrace</option>
  #<option value="UKA00501">Port Talbot Margam</option>
  #<option value="UKA00159">Record Marples 1</option>
  #<option value="UKA00160">Record Marples 2</option>
  #<option value="UKA00351">Redcar</option>
  #<option value="UKA00563">Redcar Dormanstown</option>
  #<option value="UKA00519">Redcar Normanby</option>
  #<option value="UKA00473">Runcorn Weston Point</option>
  #<option value="UKA00506">Scunthorpe Low Santon</option>
  #<option value="UKA00381">Scunthorpe Town</option>
  #<option value="UKA00474">Sheffield Brinsworth</option>
  #<option value="UKA00250">Sheffield Centre</option>
  #<option value="UKA00575">Sheffield Devonshire Green</option>
  #<option value="UKA00181">Sheffield Tinsley</option>
  #<option value="UKA00240">Swansea</option>
  #<option value="UKA00520">Swansea Coedgwilym</option>
  #<option value="UKA00521">Swansea Morriston</option>
  #<option value="UKA00475">Walsall Bilston Lane</option>
  #<option value="UKA00820">Walsall Pleck Park</option>
  #<option value="UKA00278">Wytham Wood</option>
  #<option value="UKA00168">Yarner Wood</option>

  #example pollutants
  #<option value="1032" selected="selected">Arsenic</option>
  #<option value="1033">Cadmium</option>
  #<option value="1034">Chromium</option>
  #<option value="1055">Cobalt</option>
  #<option value="262">Copper</option>
  #<option value="356">Iron</option>
  #<option value="263">Lead</option>
  #<option value="1035">Manganese</option>
  #<option value="1036">Nickel</option>
  #<option value="1378">Selenium</option>
  #<option value="1038">Vanadium</option>
  #<option value="264">Zinc</option>

  .url <- "https://uk-air.defra.gov.uk/data/non-auto-data?uka_id="
  .url <- paste(.url, site, "&view=data&network=metals&year=", sep="")
  .url <- paste(.url, year, "&pollutant=", pollutant, "#", sep="")
  .tbl <- readLines(.url)

  test <- grep("<table", .tbl)
  #if tbl 0 end or mor than 1 case???
  .tbl <- .tbl[test:length(.tbl)]
  test <- grep("</table", .tbl)
  #if tbl 0 end or more than 1 case???
  .tbl <- .tbl[1:test]
  .hd <- .tbl[grep("\t\t\t\t\t\t\t<th>", .tbl)]
  .hd <- gsub("\t\t\t\t\t\t\t\t<th>", "", .hd)
  .hd <- gsub("</th>", "", .hd)
  .hd #the headers
  .d <- .tbl[grep("\t\t\t\t\t\t\t<td>", .tbl)]
  .d <- gsub("\t\t\t\t\t\t\t<td>", "", .d)
  .d <- gsub("</td>", "", .d)
  .d <- gsub("<sup>", "", .d)
  .d <- gsub("</sup>&nbsp;", "", .d)
  .d <- gsub("<span class=\"green bold\">", "", .d)
  .d <- gsub("</span>&nbsp;", "", .d)

  ##########################################################
  #data.frame build
  #########################################################
  #makes assumptions about data dimensions
  #might be fine
  ##########################################################
  .out <- data.frame(
    site = site,
    .d[seq(1, length(.d), by = length(.hd))],
    .d[seq(2, length(.d), by = length(.hd))],
    .d[seq(3, length(.d), by = length(.hd))],
    .d[seq(4, length(.d), by = length(.hd))],
    .d[seq(5, length(.d), by = length(.hd))],
    .d[seq(6, length(.d), by = length(.hd))])
  ########################
  #naming
  ########################
  #added site code BUT
  #could get site name from download???
  ########################
  #also could make data.frame names
  names(.out) <- c("site", .hd)

  ########################
  #reformatting
  ########################
  #could do this in import wrapper
  #might speed things up a little
  .out$`Start Date` <- as.Date(.out$`Start Date`, tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d"))
  .out$`End Date` <- as.Date(.out$`End Date`, tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d"))
  .out$Measurement <- as.numeric(.out$Measurement)

  .out
}










##########################################
# getUKgetUKNAPAHs
#########################################

# get PAH archive data


## examples
## like with metals

# getUKNANPAH("UKA00315", 1072, 2018)

#sites <- "UKA00315"
#pols <- c(1080, 1072, 1414, 1415, 1078, 1082, 1424, 1425, 1076, 1085, 1075, 1071,
#          1079, 1084, 1091, 1073, 1090, 1074, 1086, 1087, 1088, 1089, 1081)
# years <- 2011:2022
# out <- importUKNAPAHs(site, pols, years)
# saveRDS(out, "my.pahs.1.rds")

#' @rdname data.import
#' @export

importUKNAPAHs <- function(site, pollutant, year, ...){
  #wrapper for getUKNANPAH
  .sites <- lapply(site, function(s){
    .pols <- lapply(pollutant, function(p){
      .years <- lapply(year, function(y){
        temp <- try(getUKNANPAH(s, p, y),
                    silent=TRUE)
        if(class(temp)[1]=="try-error") {
          warning("bad source")
          NULL
        } else {
          temp
        }
      })
      .years <- do.call(rbind, .years)
    })
    .pols <- do.call(rbind, .pols)
  })
  out <- do.call(rbind, .sites)
  out
}


# unexported single site/pah/year import function
#   usign with above wrapper

getUKNANPAH <- function(site, pollutant, year, ...){

  #based on metals methods

  .url <- "https://uk-air.defra.gov.uk/data/non-auto-data?uka_id="
  .url <- paste(.url, site, "&view=data&network=paha&year=", sep="")
  .url <- paste(.url, year, "&pollutant=", pollutant, "#", sep="")
  .tbl <- readLines(.url)

  test <- grep("<table", .tbl)
  #if tbl 0 end or mor than 1 case???
  .tbl <- .tbl[test:length(.tbl)]
  test <- grep("</table", .tbl)
  #if tbl 0 end or more than 1 case???
  .tbl <- .tbl[1:test]
  .hd <- .tbl[grep("\t\t\t\t\t\t\t<th>", .tbl)]
  .hd <- gsub("\t\t\t\t\t\t\t\t<th>", "", .hd)
  .hd <- gsub("</th>", "", .hd)
  .hd #the headers
  .d <- .tbl[grep("\t\t\t\t\t\t\t<td>", .tbl)]
  .d <- gsub("\t\t\t\t\t\t\t<td>", "", .d)
  .d <- gsub("</td>", "", .d)
  .d <- gsub("<sup>", "", .d)
  .d <- gsub("</sup>&nbsp;", "", .d)
  .d <- gsub("<span class=\"green bold\">", "", .d)
  .d <- gsub("</span>&nbsp;", "", .d)
  #currently replacing measurements <x as NA...
  .d[grepl("&lt;", .d)] <- NA

  ##########################################################
  #data.frame build
  #########################################################
  #makes assumptions about data dimensions
  #might be fine
  ##########################################################
  .out <- data.frame(
    site = site,
    .d[seq(1, length(.d), by = length(.hd))],
    .d[seq(2, length(.d), by = length(.hd))],
    .d[seq(3, length(.d), by = length(.hd))],
    .d[seq(4, length(.d), by = length(.hd))],
    .d[seq(5, length(.d), by = length(.hd))],
    .d[seq(6, length(.d), by = length(.hd))])
  ########################
  #naming
  ########################
  #added site code BUT
  #could get site name from download???
  ########################
  #also could make data.frame names
  names(.out) <- c("site", .hd)

  ########################
  #reformatting
  ########################
  #could do this in import wrapper
  #might speed things up a little
  .out$`Start Date` <- as.Date(.out$`Start Date`, tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d"))
  .out$`End Date` <- as.Date(.out$`End Date`, tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d"))
  .out$Measurement <- as.numeric(.out$Measurement)

  .out
}








