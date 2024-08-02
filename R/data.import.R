#######################################
#' @title Data import functions
#######################################
#' @name data.import
#' @aliases importUKNAN importUKNAMetals importUKNAPAHs importUKNAHCs

#' @description Some functions to import data from held in on-line data
#' archives.

#' @param network_id (\code{importUKNAN} only; character) The identifier
#' code for the network archive to request data from.
#' @param uka_id (character) The 'UKA' identifier code(s) for the site(s) the
#' data is to be downloaded from.
#' @param pollutant_id (numeric) The pollutant identifier(s) of the
#' pollutant(s) to download data for.
#' @param year (numeric) The year(s) to download data for.
#' @param ... (other arguments) currently ignored.
#' @param silent (logical) Return data silently? If
#' \code{TRUE} (default) a \code{NULL} is returned without warning if the
#' requested data is not available; if \code{FALSE}, options are suggested
#' when returning a \code{NULL}.

#' @return The import functions typically return the request data as a
#' \code{data.frame} if data is available for requested site/pollutant/year
#' combination. In some cases the \code{data.frame} may be set up
#' for use with use with other packages.
#'
#' \code{importUKNAN}: Generic import function for data from DEFRA's
#' Non-Automatic Networks. (In-development and largely untested;
#' currently recommend using dedicated import if below or getting in
#' contact.)
#'
#' See https://uk-air.defra.gov.uk/networks/network-info?view=non-automatic.
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
#'
#' \code{importUKNAHCs}: import function for metal data from the UK
#' Non-automatic Hydrocarbon Network.
#'
#' see https://uk-air.defra.gov.uk/networks/network-info?view=nahc for details
#' and source information.


##############################
# to think about
##############################

# need to decide how to document (and/or handle ??) site and pollutant
#     identifiers currently remarked in olde versions of functions...
#         currently if silent = FALSE, it lists 'valid' options
#             (but not all combinations are valid)
#             (also this is messy if you use it with a multiple
#              site/pollutant/year call...)

# should we set defaults
#     openair import functions do

# could we make a importMeta/getMeta like function?

# these could probably be faster
#     options to speed up ???
#         suspect data.table should speed this up
#              (based on experience with respeciate...)

# adding authors to documents
#     me and Tobey?

# active the html in documentation

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
#' and neither I or you have any control in that...
#'


###############################################
# importUKNAN
###############################################

#' @rdname data.import
#' @export

# generic non-automatic network import function

# tested for...
############################

# will data.table speed things up ???
#     in importUKNAN....
#         test replacing do.call(rbind, list) with data.table:::rbind.list(list)

# issues identified
############################

# using archive report checking is slowing stuff down
#    using importUKNAN("metals", ...) is 25% slower than importUKNAMetals(...)
# "https://uk-air.defra.gov.uk/data/download-non-auto-data?uka_id=[SITE]&network=[NET]&year=[YEAR]&pollutant=[POL]"
# also tried...
#"https://uk-air.defra.gov.uk/data/non-auto-data?uka_id=UKA00211&network=nahc&s=View+Site"
# previously used because accurate data file, faster/more direct... no web scrap needed...!
#    but it seems to return a default data set if you requested one that does not exist...
#    without an obvious way to test...
#         some archive identify the returned set by adding 'selected' to item in the
#         drop-down list of valid options BUT not all do this...
#    also this return a comma-delimited which needs careful handling because some
#    pollutants have commas in names, e.g. 1,3-butadiene...
# currently checking webpage html for year/site/pollutant lists for valid options
#         (basically reading the drop-down lists of options and values...)
#         (year, site [name and uka_ code] and pollutant [name and pollutant_id code])
#               (NB: NOT sure pollutant_id's are consistent across archives...)
#    also checking output table to make sure pollution is the requested one...
#    might need sensible start and end date values.
#          (requested year plus maybe one start overlapping year before and
#           and one end year overlapping year after...)


# this does not work for NAHC network...
#get error for...
# a <- sneak_import("nahc", "UKA00518", "995", "2016", silent=F);a
#     looks like it can't find the pol_ids
#         guessing benzene = 995 because it is on the figure labels...
#         some sites only have benzene
#         might have to find one that has 1,3-... and find code...
#     could modify importUKNAN ???
#         if you send it the valid pol_id list it does not looks for it
#             that gets you past error
#         could also extend this to other valid lists..?
#             that might spped things up a little ???

# to think about
###############################

# move work ga_UKNAN_check into importUKNAN function
#      (maybe???)
#      any time saving/advantage there ???

# make look-up tables for networks,
#      maybe as part of replacements for the the other import functions
#           that could give us an option to make better look-up tables???






#example
################################

#pols <- c("1032", "1033", "1034", "1055",
#          "262", "356", "263", "1035", "1036", "1378", "1038", "264")
#yrs <- 2016:2017
#importUKNAN("metals", "uka00212", pols, yrs)
#importUKNAMetals("uka00212", pols, yrs)


importUKNAN <- function(network_id, uka_id, pollutant_id, year,
                        ..., silent = TRUE){

  #set up
  ###################
  .ref0 <- "https://uk-air.defra.gov.uk/data/non-auto-data?uka_id=[SITE]&view=data&network=[NET]&year=[YEAR]&pollutant=[POL]#"
  #only currently letting you have one network at time
  if(length(network_id)!=1){
    stop("IMPORT> sorry only one network at a time!",
         call. = FALSE)
  }
  .ref0 <- gsub("[[]NET[]]", network_id, .ref0)
  .args <- list(...)

  #build request matrix
  .rqts <- expand.grid(uka_id, year, pollutant_id)
  #print(.rqts)

  #do all requests...
  .ans <- lapply(1:nrow(.rqts), function(.r){

    #1 or more site, pollutant and year
    #only tested for pollutant and year so far...
    .ref <- gsub("[[]SITE[]]", .rqts[.r,1], .ref0)
    .ref <- gsub("[[]POL[]]", .rqts[.r,3], .ref)
    .ref <- gsub("[[]YEAR[]]", .rqts[.r,2], .ref)
    if(!silent){
      print(.ref)
    }

    .url <- readLines(.ref)

    # valid years, sites and pollutants
    .yrs <- ga_UKNAN_check(.url, "year")
    .sts <- ga_UKNAN_check(.url, "uka_id")
    #.pols <- ga_UKNAN_check(.url, "pollutant")
    ############################
    #testing passing .pols via importUKNAN
    #for importUKNAHC...
    .pols <- if(".pols" %in% names(.args)){
      .args$.pols
    } else {
      ga_UKNAN_check(.url, "pollutant")
    }
    ############################


    if(is.null(.sts)){
      if(!silent){
        print("unknown network")
      }
      return(NULL)
    }
    if(is.null(.yrs) & is.null(.pols)){
      if(!silent){
        print("site not in that network?")
        print("   valid sites:")
        print(.sts)
      }
      return(NULL)
    }

    #####################
    #checks
    #print(.yrs)
    #print(.pols)
    #print(.sts)

    #get site.name
    .st.nm <- grep("\t\t\t\t\t\t\t\t<h2>", .url)
    if(length(.st.nm)<1){
      .st.nm <- "[UNKNOWN]"
    } else {
      .st.nm <- .url[.st.nm]
      .st.nm <- gsub("\t\t\t\t\t\t\t\t<h2>|</h2>", "", .st.nm)
    }
    if(.st.nm %in% .sts$value){
      .st.code <- .sts$id[.sts$value==.st.nm]
    } else {
      .st.code <- "[UNKNOWN]"
    }

    #get the data
    .tbl <- .url
    test <- grep("<table", .tbl)
    #if tbl 0 end or more than 1 case???
    if(length(test)==1){
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
      #currently resetting &micro; to u
      .d <- gsub("&micro;", "u", .d)
      #currently replacing measurements <x as NA...
      .d[grepl("&lt;", .d)] <- NA

      ##########################################################
      #data.frame build
      #########################################################
      #makes assumptions about data dimensions
      #might be fine
      ##########################################################
      if(length(.d)>5){
        .out <- data.frame(
          network = network_id,
          uka_id = .st.code,
          site = .st.nm,
          pol_id= NA,
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
        names(.out) <- c("network","uka_id", "site", "pol_id", .hd)

        #get pollutant.name
        .test <- subset(.pols, id %in% .rqts[.r,3])
        if(nrow(.test)==0){
          .out <- NULL
        } else {
          .pol.id <- .test$id
          .out$pol_id <- .pol.id
          #test for valid pollutant
          .out <- subset(.out, grepl(make.names(tolower(.test$value)),
                                     make.names(tolower(Pollutant))))
          #test for valid date
          #this treats year=200 as valid
          #.out <- .out[grepl(as.character(.rqts[.r,2]), paste(.out$`Start Date`, .out$`End Date`)),]
        }
      } else {
        .out <- NULL
      }

      ########################
      #reformatting
      ########################
      #could do this in import wrapper
      #might speed things up a little
      if(is.null(.out) || nrow(.out)<1){
        .out <- NULL
      } else {
        .out$`Start Date` <- as.Date(.out$`Start Date`,
                                     tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d"))
        .out$`End Date` <- as.Date(.out$`End Date`,
                                   tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d"))
        .out$Measurement <- as.numeric(.out$Measurement)
        #test years are valid
        if(all(format(.out$`Start Date`, "%Y")==as.character(.rqts[.r,2]) |
               format(.out$`End Date`, "%Y")==as.character(.rqts[.r,2]))){
          .out <- .out
        } else {
          .out <- NULL
        }
      }
    } else {
      .out <- NULL
    }

    if(!silent){
      print("sites")
      print(.sts)
      print("years")
      print(.yrs)
      print("pollutants")
      print(.pols)
      print(.ref)
    }
    .out
  })

  #replacing with rbindlist
  #.out <- do.call(rbind, .ans)
  #does rbindlist want to be use.names=TRUE as well??
  .out <- data.table::rbindlist(.ans, fill = TRUE)
  .out <- as.data.frame(.out)
  if(nrow(.out)<1){
    NULL
  } else {
    .out
  }
}


# unexported workhorse function

ga_UKNAN_check <- function(url, target){
  .ref <- "class=\"form-control\" name=\"[TAR]\""
  .ref <- gsub("[[]TAR[]]", target, .ref)
  .x1 <- grep(.ref, url)
  if(length(.x1)==1){
    .tars <- url[(.x1+1):length(url)]
    .x1 <- which(.tars=="")
    .tars <- .tars[1:(.x1[1]-1)]
    .tars <- gsub("<option value=\"|</option>|\" selected=\"selected", "", .tars)
    #.tars <- gsub("<option value=\"|</option>", "", .tars)
    #was planning to used the selected to identify which select BUT
    #it does not seem
    .tars <- lapply(strsplit(.tars, "\">"), function(x){
      data.frame(id=x[1], value=x[2])
    })
    .tars <- do.call(rbind, .tars)
    if(all(is.na(.tars))){
      .tars <- NULL
    }
  } else {
    .tars <-NULL
  }
  .tars
}







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
#  out <- importUKNAMetals(sites, pols, years)
#  saveRDS(out, "uk.metals.1.rds")

#' @rdname data.import
#' @export

importUKNAMetals <- function(uka_id, pollutant_id, year, ..., silent = TRUE){
  importUKNAN(network_id = "metals", uka_id = uka_id,
              pollutant_id = pollutant_id, year = year,
              ..., silent = silent)
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


importUKNAPAHs <- function(uka_id, pollutant_id, year, ..., silent = TRUE){
  importUKNAN(network_id = "paha", uka_id = uka_id,
              pollutant_id = pollutant_id, year = year,
              ..., silent = silent)
}



##########################################
# importUKNAHCs
#########################################

# https://uk-air.defra.gov.uk/data/non-auto-data?uka_id=UKA00444&view=data&network=nahc&year=2007&pollutant=997#view


## examples
## like with metals

# importUKNAHCs("uka00212", c(995,997), 2008)

#' @rdname data.import
#' @export

importUKNAHCs <- function(uka_id, pollutant_id, year, ..., silent = TRUE){
  #need to tell what pollutants could be there
  #because I do not think nahc always return this...
  .pols <- data.frame(id=c(997, 995),
                      value=c("1,3-butadiene", "benzene"))
  importUKNAN(network_id = "nahc", uka_id = uka_id,
              pollutant_id = pollutant_id, year = year,
              .pols=.pols, ..., silent = silent)
}












#old unexported versions of import functions...

ga_importUKNAMetals <- function(uka_id, pollutant_id, year, ...){
  site <- uka_id
  pollutant <- pollutant_id

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



ga_importUKNAPAHs <- function(uka_id, pollutant_id, year, ...){

  #temp fix with updating
  site <- uka_id
  pollutant <- pollutant_id

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




import_test <- function(network_id, uka_id, pollutant_id, year,
                        ..., silent = TRUE){

  #set up
  ###################
  .ref0 <- "https://uk-air.defra.gov.uk/data/non-auto-data?uka_id=[SITE]&view=data&network=[NET]&year=[YEAR]&pollutant=[POL]#"
  #only currently letting you have one network at time
  if(length(network_id)!=1){
    stop("IMPORT> sorry only one network at a time!",
         call. = FALSE)
  }
  .ref0 <- gsub("[[]NET[]]", network_id, .ref0)
  .args <- list(...)

  #build request matrix
  .rqts <- expand.grid(uka_id, year, pollutant_id)
  #print(.rqts)

  #do all requests...
  .ans <- lapply(1:nrow(.rqts), function(.r){

    #1 or more site, pollutant and year
    #only tested for pollutant and year so far...
    .ref <- gsub("[[]SITE[]]", .rqts[.r,1], .ref0)
    .ref <- gsub("[[]POL[]]", .rqts[.r,3], .ref)
    .ref <- gsub("[[]YEAR[]]", .rqts[.r,2], .ref)
    if(!silent){
      print(.ref)
    }

    .url <- readLines(.ref)
    return(data.frame())

    # valid years, sites and pollutants
    .yrs <- ga_UKNAN_check(.url, "year")
    .sts <- ga_UKNAN_check(.url, "uka_id")
    #.pols <- ga_UKNAN_check(.url, "pollutant")
    ############################
    #testing passing .pols via importUKNAN
    #for importUKNAHC...
    .pols <- if(".pols" %in% names(.args)){
      .args$.pols
    } else {
      ga_UKNAN_check(.url, "pollutant")
    }
    ############################


    if(is.null(.sts)){
      if(!silent){
        print("unknown network")
      }
      return(NULL)
    }
    if(is.null(.yrs) & is.null(.pols)){
      if(!silent){
        print("site not in that network?")
        print("   valid sites:")
        print(.sts)
      }
      return(NULL)
    }

    #####################
    #checks
    #print(.yrs)
    #print(.pols)
    #print(.sts)

    #get site.name
    .st.nm <- grep("\t\t\t\t\t\t\t\t<h2>", .url)
    if(length(.st.nm)<1){
      .st.nm <- "[UNKNOWN]"
    } else {
      .st.nm <- .url[.st.nm]
      .st.nm <- gsub("\t\t\t\t\t\t\t\t<h2>|</h2>", "", .st.nm)
    }
    if(.st.nm %in% .sts$value){
      .st.code <- .sts$id[.sts$value==.st.nm]
    } else {
      .st.code <- "[UNKNOWN]"
    }

    #get the data
    .tbl <- .url
    test <- grep("<table", .tbl)
    #if tbl 0 end or more than 1 case???
    if(length(test)==1){
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
      #currently resetting &micro; to u
      .d <- gsub("&micro;", "u", .d)
      #currently replacing measurements <x as NA...
      .d[grepl("&lt;", .d)] <- NA

      ##########################################################
      #data.frame build
      #########################################################
      #makes assumptions about data dimensions
      #might be fine
      ##########################################################
      if(length(.d)>5){
        .out <- data.frame(
          network = network_id,
          uka_id = .st.code,
          site = .st.nm,
          pol_id= NA,
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
        names(.out) <- c("network","uka_id", "site", "pol_id", .hd)

        #get pollutant.name
        .test <- subset(.pols, id %in% .rqts[.r,3])
        if(nrow(.test)==0){
          .out <- NULL
        } else {
          .pol.id <- .test$id
          .out$pol_id <- .pol.id
          #test for valid pollutant
          .out <- subset(.out, grepl(make.names(tolower(.test$value)),
                                     make.names(tolower(Pollutant))))
          #test for valid date
          #this treats year=200 as valid
          #.out <- .out[grepl(as.character(.rqts[.r,2]), paste(.out$`Start Date`, .out$`End Date`)),]
        }
      } else {
        .out <- NULL
      }

      ########################
      #reformatting
      ########################
      #could do this in import wrapper
      #might speed things up a little
      if(is.null(.out) || nrow(.out)<1){
        .out <- NULL
      } else {
        .out$`Start Date` <- as.Date(.out$`Start Date`,
                                     tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d"))
        .out$`End Date` <- as.Date(.out$`End Date`,
                                   tryFormats = c("%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d"))
        .out$Measurement <- as.numeric(.out$Measurement)
        #test years are valid
        if(all(format(.out$`Start Date`, "%Y")==as.character(.rqts[.r,2]) |
               format(.out$`End Date`, "%Y")==as.character(.rqts[.r,2]))){
          .out <- .out
        } else {
          .out <- NULL
        }
      }
    } else {
      .out <- NULL
    }

    if(!silent){
      print("sites")
      print(.sts)
      print("years")
      print(.yrs)
      print("pollutants")
      print(.pols)
      print(.ref)
    }
    .out
  })

  #replacing with rbindlist
  #.out <- do.call(rbind, .ans)
  #does rbindlist want to be use.names=TRUE as well??
  .out <- data.table::rbindlist(.ans, fill = TRUE)
  .out <- as.data.frame(.out)
  if(nrow(.out)<1){
    NULL
  } else {
    .out
  }
}

