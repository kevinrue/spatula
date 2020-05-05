#' S4Vectors bindings for Spatial objects
#'
#' Implements various internal methods for \linkS4class{SpatialPoints} and \linkS4class{SpatialPolygons} objects,
#' so that they are compatible with Bioconductor containers - see the \pkg{S4Vectors} package for more details.
#'
#' @author Aaron Lun
#' @examples
#' # Storing Spatial classes in containers:
#' coords <- matrix(runif(30), nrow=10)
#' X <- DataFrame(location=I(sp::SpatialPoints(coords)))
#' X
#' 
#' coords2 <- matrix(runif(15), nrow=5)
#' Y <- DataFrame(location=I(sp::SpatialPoints(coords2)))
#' Y
#'
#' # Subsetting and such just works:
#' rbind(X, Y)
#' X[1:10,,drop=FALSE]
#'
#' # Works for spatial polygons as well, based on the polygon centroid.
#' spm <- makeSpatialPolygons(matrix(runif(10), ncol=2), matrix(runif(10), ncol=2))
#' pX <- DataFrame(poly=I(spm))
#' pX
#'
#' spm2 <- makeSpatialPolygons(matrix(runif(10), ncol=2), matrix(runif(10), ncol=2))
#' pY <- DataFrame(poly=I(spm2))
#' pY
#'
#' combined <- rbind(pX, pY)
#' combined
#' combined[c(1,1,2,3,3),,drop=FALSE]
#' 
#' @docType methods
#' @name spatula-bind
#' @aliases
#' bindROWS,SpatialPoints-method
#' extractROWS,SpatialPoints-method
#' extractROWS,SpatialPoints,ANY-method
#'
#' bindROWS,SpatialPolygons-method
#' extractROWS,SpatialPolygons-method
#' extractROWS,SpatialPolygons,ANY-method
#'
#' showAsCell,SpatialPoints-method
#' showAsCell,SpatialPolygons-method
#'
#' bindROWS,.PointsVector-method
#' bindROWS,.PolygonsVector-method
NULL

#' @export
#' @importFrom S4Vectors bindROWS
setMethod("bindROWS", "SpatialPoints", function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE) {
    do.call(rbind, c(list(x), objects))
})

#' @export
#' @importFrom S4Vectors extractROWS normalizeSingleBracketSubscript
setMethod("extractROWS", "SpatialPoints", function(x, i) {
    i <- normalizeSingleBracketSubscript(i, seq_len(NROW(x)))
    x[i,] # forcing it to drop, as the sp authors have repurposed drop= to do something else for SpatialPoints' [.
})

#' @export
#' @importFrom S4Vectors bindROWS
#' @importFrom utils relist
#' @importFrom sp rbind.SpatialPolygons rbind.SpatialPolygonsDataFrame
setMethod("bindROWS", "SpatialPolygons", function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE) {
    # Hacking our way around the unique ID requirement.
    ref.ids <- .get_ids(x)
    obj.ids <- lapply(objects, .get_ids)
    all.ids <- list(ref.ids, obj.ids)
    new.ids <- relist(make.unique(unlist(all.ids)), all.ids)

    x <- .replace_ids(x, new.ids[[1]])
    for (i in seq_along(objects)) {
        objects[[i]] <- .replace_ids(objects[[i]], new.ids[[2]][[i]])
    }

    do.call(rbind, c(list(x), objects))
})

#' @export
#' @importFrom S4Vectors extractROWS normalizeSingleBracketSubscript
setMethod("extractROWS", "SpatialPolygons", function(x, i) {
    # WHAT A PAIN! Need to override sp's refusal to duplicate entries.
    ids <- .get_ids(x)
    i <- normalizeSingleBracketSubscript(i, ids)
    x@polygons <- x@polygons[i]

    ids <- ids[i]
    if (anyDuplicated(ids)) {
        ids <- make.unique(ids)
        x <- .replace_ids(x, ids)
    }

    # Trying to trigger clean-up of other slots. 
    x[seq_along(x),]
})

#' @importFrom sp coordinates
.coord_shower <- function(object) {
    X <- coordinates(object)
    collected <- lapply(seq_len(ncol(X)), function(i) showAsCell(X[,i,drop=FALSE]))
    output <- do.call(paste, c(collected, list(sep=", ")))
    sprintf("(%s)", output)
}

#' @export
#' @importFrom S4Vectors showAsCell
#' @importFrom sp SpatialPoints
setMethod("showAsCell", "SpatialPoints", .coord_shower)

#' @export
#' @importFrom S4Vectors showAsCell
#' @importFrom sp SpatialPolygons
setMethod("showAsCell", "SpatialPolygons", .coord_shower) # uses the lab_pts, probably the average.
