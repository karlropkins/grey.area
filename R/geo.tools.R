##############################
#' @title grey.area geo tools
##############################
#'
#' @description Some functions for use with (latitude, longitude)
#' coordinates.
#' @name geo.tools
#' @aliases geoFrame geoBearing geoDistance geoElevation geoDestination
#' geoConvertBNG2LatLon geoConvertLatLonDeg2Dec
#' @param lat,lon Latitudes and longitudes to be used in geographical
#' calculations. Note: This can be supplied as two vectors or as a
#' list or data.frame containing lat and lon vectors.
#' @param pair.lat.lon (Logical) Should \code{lat} and \code{lon}
#' be paired? If \code{TRUE} (as in default) \code{lat} and \code{lon}
#' series lengths are matched and extra values are discarded.
#' @param units (Character) The units that results should be reported in.
#' For, geoDistance, \code{geoElevation} and \code{geoDestination},
#' the current options are (the default) \code{"m"} and \code{"km"}.
#' @param method (For \code{geoElevation}) the elevation method.
#' Currently, the only enabled option is \code{'google.elevation'}
#' which uses the Google Elevation API.
#' @param n (For \code{geoElevation}) the single request size limit.
#' Some elevation data request methods cannot handle large numbers of
#' \code{lat, lon} at a go. So, where this is case \code{geoElevation}
#' generates and merges multiple requests. \code{n} sets the maximum
#' number of \code{lat, lon} per request.
#' @param bearing,distance (For \code{geoDestination}) the bearing
#' (relative to North) and distance to move from the start point
#' \code{lat}, \code{lon}.
#' @param easting,northing (For \code{geoConvertBNG2LatLon})
#' Easting and Northing coordinates.
#' @param crs (For \code{geoConvertBNG2LatLon}) the
#' coordinate reference system for supplied \code{easting}
#' and \code{northing}, by default 27700, the EPSG code
#' for British National Grid coordinates.
#' @param format (For \code{geoConvertLatLonDev2Deg}) formatting for supplied
#' \code{lat} and \code{lon} assumed to be supplied in degrees (deg/min/sec)
#' format. The default \code{NULL} assumes \code{lat} and \code{lon} are not
#' formatted.
#' @param test (For \code{geoConvertLatLonDev2Deg}) test supplied \code{lat}
#' and \code{lon} coordinates before converting. This generates a WARNING only
#' if any look like a suspect case for conversion, e.g a \code{lat > 90},
#' \code{lon > 180}, degrees > 60, etc...
#' @param ... Additional arguments, currently ignored.
#' @author Karl Ropkins
#' @keywords methods
#' @note All functions currently require or convert to
#' \code{lat} and \code{lon} in WGS84 coordinates and
#' conventional decimal format.
#'
#' \code{geoBearing} DETAILS NEEDED.
#'
#' \code{geoDistance} uses the haversine formula to account to the
#' Earth's surface curvature, and uses 6371 km as the radius of earth.
#'
#' \code{geoElevation} uses the Google elevation API to get elevations,
#' so required R to be internet enabled.
#'
#' \code{geoDestination} DETAILS NEEDED.
#' @return \code{geoFrame} is data handler used by other geo...
#' functions. It returns a list containing the named components
#' \code{lat} and \code{lon}. if latitude, longitude pairing has
#' been applied \code{pair.lat.lon = TRUE} these components will
#' be the same length.
#'
#' \code{geoBearing} returns a vector of bearings for supplied lat, lon
#' combinations. NOTE: Bearings length will be one less than the length
#' number of lat (or lon) because bearings are measured prior point to
#' latter point.
#'
#' \code{geoDistance} returns a vector of distances between supplied lat,
#' lon points in a supplied series, so again one less the supplied number
#' of points. By default, this will be supplied in meters, but units can
#' be modified as part of the call using, e.g. \code{units = "km"} to get
#' distance(s) in kilometers.
#'
#' \code{geoElevation} returns a vector of elevations, one for each
#' lat, lon pair supplied. By default, this will be supplied in meters,
#' but units can be modified as part of the call using, e.g.
#' \code{units = "km"} to get elevations(s) in kilometers.
#'
#' \code{geoDestination} returns a list of named lat/lon pairs for
#' the path from the supplied lat/lon start point, assuming the
#' journey described by the supplied bearings and distance.
#'
#' \code{geoConvertBNG2LatLon} converts British National
#' Grid (BNG) Easting/Northing coordinates to (WGS84)
#' Latitude/Longitude coordinates.
#'
#' \code{geoConvertLatLonDeg2Dem} converts Latitude/Longitude coordinates
#' logged in degrees to decimal.
#' @examples
#' #example 1
#' lat <- 1:10
#' lon <- 1:10
#' #get the distance point1 to point2, point2 to point2, etc.
#' dist <- geoDistance(lat, lon)
#' #get the bearing point1 to point2, point2 to point2, etc.
#' bear <- geoBearing(lat, lon)
#' #reconstruct the journey from start point,
#' #using distances and bearings
#' geoDestination(lat[1], lon[1], bear, dist)
#' #(very nearly...)

#########################################
#NOTES:
########################################
#to do
#look at geoElevation (not working again)
#########################################
#think about
#option for from rather than to for geoBearings
#err catcher and tidy for geoDestination

#####################
#####################
##geoFrame
#####################
#####################
# kr v.0.1.4 2019/05/21

#local function to manage/standarize geographic data
#[in development]

#' @rdname geo.tools
#' @export
geoFrame <- function(lat, lon = NULL, ..., pair.lat.lon = TRUE){

  #check for nulls
  if(missing(lat))
    stop("missing both lat, lon", call. = FALSE)

  #NOTE: this is a cut of previous
  #might have missed something but now a lot simpler...

  #lat, lon checking
  ans <- if(is.null(lon)){
    #must all be in lat at moment...
    if(!all(c("lat", "lon") %in% names(lat)))
      stop("unable to find 'lat' and/or 'lon', ",
           call. = FALSE)
    lat
  } else {
    list(lat=lat, lon=lon)
  }
  #check length?

  if(pair.lat.lon){
    temp <- min(c(length(ans$lat), length(ans$lon)))
    ans$lat <- ans$lat[1:temp]
    ans$lon <- ans$lon[1:temp]
  }

  #output results
  ans

}


#####################
#####################
##geoBearing
#####################
#####################

#' @rdname geo.tools
#' @export
geoBearing <- function(lat, lon = NULL, ...){

    ans <- geoFrame(lat = lat, lon = lon, ...)

    if(length(ans$lat)<2 | length(ans$lon)<2)
        stop("need at least two lat/lon sets to calculate bearing",
             call. = FALSE)

    #setup
    deg2rad <- function(x) x * (pi/180)
    rad2deg <- function(x) x * (180/pi)

    lat2 <- ans$lat[-1]
    lat1 <- ans$lat[-length(ans$lat)]
    lon2 <- ans$lon[-1]
    lon1 <- ans$lon[-length(ans$lon)]

    #difference in longitudinal coordinates
    dLon <- deg2rad(lon2) - deg2rad(lon1)

    #difference in the phi of latitudinal coordinates
    dPhi <- log(tan(deg2rad(lat2) / 2 + pi / 4) /
                  tan(deg2rad(lat1) / 2 + pi / 4))
    #we need to recalculate
    #dLon if it is greater than pi

###########################
    #    if(abs(dLon) > pi) {
    #        if(dLon > 0) { dLon = (2 * pi - dLon) * -1 } else
    #                     { dLon = 2 * pi + dLon } }
###########################
    #dLon <- ifelse(abs(dLon) > pi,
    #        ifelse(dLon > 0, dLon = (2 * pi - dLon) * -1,
    #                         dLon = 2 * pi + dLon),
    #        dLon)
###########################
#testing...
    dLon <- ifelse(abs(dLon) > pi,
                   ifelse(dLon > 0, (2 * pi - dLon) * -1,
                          2 * pi + dLon),
                   dLon)
###########################

    #return the angle, normalized
    (rad2deg(atan2(dLon, dPhi)) + 360) %% 360

}


#####################
#####################
##geoDistance
#####################
#####################

#' @rdname geo.tools
#' @export
geoDistance <- function(lat, lon = NULL, units = "m", ...){

    ans <- geoFrame(lat = lat, lon = lon, ...)

    if(length(ans$lat)<2 | length(ans$lon)<2)
        stop("need at least two lat/lon sets to measure distance",
             call. = FALSE)

    #setup
    deg2rad <- function(x) x * (pi/180)
    rad2deg <- function(x) x * (180/pi)

    lat2 <- ans$lat[-1]
    lat1 <- ans$lat[-length(ans$lat)]
    lon2 <- ans$lon[-1]
    lon1 <- ans$lon[-length(ans$lon)]

    #using The haversine formula
    dLat <- deg2rad(lat2-lat1)
    dLon <- deg2rad(lon2-lon1)
    lat1 <- deg2rad(lat1)
    lat2 <- deg2rad(lat2)

    #the square of half the chord length between the points
    a <- sin(dLat/2) * sin(dLat/2) +
         sin(dLon/2) * sin(dLon/2) * cos(lat1) * cos(lat2)

    #the angular distance in radians
    c <- 2 * atan2(sqrt(a), sqrt(1-a))

    #radius of earth, km
    R <- 6371

    #scaling for output
    sc <- NULL
    if(units == "km") sc <- 1
    if(units == "m") sc <- 1000

#############
#handling for unrecognised units
#############

#could make option to replace na's with 0
#see previous version below
#or smoothLatLonPath handling

    #output in requested scale
    R * c * sc
}



#####################
#####################
##geoElevation
#####################
#####################
# kr v0.1.7

#geoElevation <- function(lat, lon = NULL, units = "m", ...){
#
#    #set up frame
#    ans <- geoFrame(lat = lat, lon = lon, ...)
#
#    #make google elevation api call
#    #see web guidance at
#    ##http://code.google.com/apis/maps/documentation/elevation/
#
#    temp <- paste(ans$lat, ans$lon, sep = ",", collapse = "|")
#    api.call <- paste("http://maps.googleapis.com/maps/api/elevation/xml?locations=",
#                      temp, "&sensor=false", sep = "")
#    ans <- readLines(api.call)
#
#    #strip out elevation information
#    x <- gsub(" ", "", ans)
#    x <- x[grep("elevation", x)]
#    y <- gsub("</elevation>", "", x)
#    y <- gsub("<elevation>", "", y)
#    y<- as.numeric(y)
#
##may want to develop this
##to recover other information
##(status, lat, lon, resolution)
##and to use the interpolation
##may also want to check out other apis
##return(ans) for list
#
#    #units
#    #scaling for output
#    sc <- NULL
#    if(units == "km") sc <- 0.001
#    if(units == "m") sc <- 1
#
#############
##handling for unrecognised units
##############
#
##############
##na handling - return as 0
##might want option to turn off - return as NA
##############
#
#    ans <- y * sc
#    ans <- ifelse(is.na(ans), 0, ans)
#    ans
#
#}

#' @rdname geo.tools
#' @export
geoElevation <- function (lat, lon = NULL, units = "m", ...,
                          method="google.elevation", n = 10){

    ans <- geoFrame(lat = lat, lon = lon, ...)

    geo.fun <- NULL

    if(method=="google.elevation"){

        #make google elevation api call
        #see web guidance at
        ##http://code.google.com/apis/maps/documentation/elevation/

        geo.fun <- function(lat, lon, units){
            temp <- paste(lat, lon, sep = ",", collapse = "|")
            api.call <- paste("http://maps.googleapis.com/maps/api/elevation/xml?locations=",
                              temp, "&sensor=false", sep = "")
            ans <- readLines(api.call)
            x <- gsub(" ", "", ans)
            x <- x[grep("elevation", x)]
            y <- gsub("</elevation>", "", x)
            y <- gsub("<elevation>", "", y)
            y <- as.numeric(y)
            sc <- NULL
            if (units == "km")
                 sc <- 0.001
            if (units == "m")
                 sc <- 1

#note error handling for NA below
#might want to rethink or move out of function

            ans <- y * sc
            ifelse(is.na(ans), 0, ans)
        }
        if(n>50) n <- 50
    }

#alternative dem sources
#in development

#    if(method=="earth.tools"){
#        geo.fun <- function(lat, lon, units){
#             if(is.na(lat) || is.na(lon)) return(NA)
#             url <- paste("http://www.earthtools.org/height",
#                          lat, lon, sep = "/")
#             ans <- readLines(url)
#             n <- grep("<meters>", ans)
#             if(length(n)<1) return(NULL)
#             ans <- ans[n[1]]
#             ans <- gsub("</meters>", "", ans)
#             ans <- gsub("<meters>", "", ans)
#             ans <- as.numeric(ans)
#             if(units=="km") ans <- ans/1000
#             ans
#         }
#        n <- 1
#    }

    if(is.null(geo.fun)) return(NULL)

    out <- rep(NA, length(lat))
    id <- unique(c(seq(1,length(lat), n), length(lat)))

    for(i in 1:(length(id)-1)){
        id.ref <- if(i < (length(id)-1))
                      id[i]:(id[i+1]-1) else id[i]:id[i+1]
        check <- id.ref[!is.na(lat[id.ref])]
        if(length(check)>0){
            temp <- geo.fun(lat[check], lon[check], units)
        if(length(temp)==length(check))
            out[check] <- temp
        }
    }

    out

}





####################
####################
##geoDestination
####################
####################
#kr v.0.3 2019/05/21

#' @rdname geo.tools
#' @export
geoDestination <- function (lat, lon = NULL, bearing = NULL,
                            distance = NULL, units = "m", ...){

  #needs tidying
  ans <- geoFrame(lat = lat, lon = lon, ...)
  if(length(ans$lat)!=1 || length(ans$lon)!=1)
    warning("only expecting one lat/lon pair, ignoring rest")
  deg2rad <- function(x) x * (pi/180)
  rad2deg <- function(x) x * (180/pi)

  #need lots of err catchers in here

  lat1 <- deg2rad(ans$lat[1])
  lon1 <- deg2rad(ans$lon[1])

  bearing <- deg2rad(bearing)

  sc <- NULL
  if (units == "km")
    sc <- 1
  if (units == "m")
    sc <- 1000
  #units to km
  distance <- distance / sc
  #Earth's radius /km
  R <- 6371

  ang.dist <- deg2rad(distance)/deg2rad(R)

  len <- min(c(length(bearing), length(distance)), na.rm = TRUE)


  if(length(bearing)!=length(distance))
    warning("bearing and distance lengths differ, ignoring unpaired data")
  lat2 <- c(lat1, rep(NA, len))
  lon2 <- c(lon1, rep(NA, len))

  for(i in 2:length(lat2)){
    lat2[i] <- asin(sin(lat2[i-1]) * cos(ang.dist[i-1]) +
                      cos(lat2[i-1]) * sin(ang.dist[i-1]) * cos(bearing[i-1]))
    lon2[i] <- lon2[i-1] + atan2(sin(bearing[i-1]) * sin(ang.dist[i-1]) * cos(lat2[i-1]),
                                 cos(ang.dist[i-1]) - sin(lat2[i-1]) * sin(lat2[i]))
  }
  lat2 <- rad2deg(lat2)
  lon2 <- rad2deg(lon2)
  #output
  list(lat=lat2, lon=lon2)
}





#####################
#####################
##geoConvertBNG2LatLon
#####################
#####################

#exported

# at the moment, this needs sf, added to imports
#that might change

#kr v.0.0.2 2021/05/08

#' @rdname geo.tools
#' @export
geoConvertBNG2LatLon <- function(east, north = NULL, ...,
                                 crs=27700){

  #to convert british national grid (BNG)
  #to lat long (WGS84)

  #using sf package
  #might not stay with that...

  #cheat so we can use geoframe
  if(is.data.frame(east)){
    names(east)[tolower(names(east))=="easting"] <- "lat"
    names(east)[tolower(names(east))=="northing"] <- "lon"
  }
  ans <- geoFrame(lat = east, lon = north, ...)
  ans <- as.data.frame(ans) #move this to geoFrame?
  ans <- sf::st_as_sf(ans, coords = c("lat", "lon"),
                      crs=crs)
  out <- as.data.frame(sf::st_coordinates(sf::st_transform(ans, 4326)))
  names(out) <- c("lon", "lat")
  out
}



#####################
#####################
##geoConvertLatLonDec2Deg
#####################
#####################

#exported

# at the moment, this needs sf, added to imports
#that might change

#kr v.0.0.4 2023/05/08

#' @rdname geo.tools
#' @export
geoConvertLatLonDec2Deg <- function(lat, lon = NULL, ...,
                                    format = NULL, test = TRUE){

  #to convert lat/lon degrees to decimal
  #had several goes at this but first time this made it to package...

  #might not stay with that...
  #NOTE by default geoframe assumes lat and lon are paired and
  #   drops any extra/unpaired cases
  #   not sure about NAs

  ans <- geoFrame(lat = lat, lon = lon, ...)
  if("lat" %in% names(ans)){
    ans$lat <- geo_ConvertXDec2Deg(ans$lat, format, if(test) "as.lat" else "")
  }
  if("lon" %in% names(ans)){
    ans$lon <- geo_ConvertXDec2Deg(ans$lon, format, if(test) "as.lon" else "")
  }
  ans
}




#############################
#unexported
#############################

geo_ConvertXDec2Deg <- function(x, format, test=""){

  #most of what I get is latlon_deg written as latlon_dec
  #so 51.30 when I was expecting 51.50

  #so no formatting, nothing as nice as 51'30''40.50'''

  #lots of online methods
  #still not sure of best way...

  if(is.numeric(x)){
    x <- as.character(x)
  }

  #############################
  #might want an else if character???
  #to account for interesting formatting
  #############################

  if(any(!grepl("[.]", x))){
    x[!grepl("[.]", x)] <- paste(x[!grepl("[.]", x)], ".", sep="")
  }
  x <- paste(x, "000000", sep="") #make should we have enough to work with
  if(is.null(format)){
    temp <- strsplit(x, "[.]")
    dgrs <- do.call(c, rbind(lapply(temp, function(x) x[1][1])))
    neg <- dgrs < 0
    dgrs <- abs(as.numeric(dgrs))
    x <- do.call(c, rbind(lapply(temp, function(x) x[2][1])))

    #no format guidance, split by position
    mins <- as.numeric(substr(x, 1, 2))/60
    secs <- gsub("[.]", "", substr(x, 3, nchar(x)))
    secs <- paste(substr(secs, 1, 2), ".", substr(secs, 3, nchar(secs)),
                  sep="")
    secs <- (as.numeric(secs))/3600

  } else {
    #if there is formatting guidance
    #split by format
  }

  if(test %in% c("as.lat", "as.lon")){
    temp <- c(0,0,0)
    if(test=="as.lat"){
      temp[1] <- length(dgrs[dgrs>90])
    }
    if(test=="as.lon"){
      temp[1] <- length(dgrs[dgrs>180])
    }
    temp[2] <- length(mins[mins >= 1])
    temp[3] <- length(secs[secs >= 0.01])
    if(sum(temp)>0){
      #################
      #how to display temp nicely
      #temp[lat/lon, mins, secs] #how many bad...
      warning("Deg2Dec: suspect conversions: ", test, "> ", temp,
              call. = FALSE)
    }
  }

  #rebuild as x_decimal
  x <- dgrs + mins + secs
  x[neg] <- -x[neg]

  #output
  x
}
