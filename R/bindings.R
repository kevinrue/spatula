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
#'
#' spm2 <- makeSpatialPolygons(matrix(runif(10), ncol=2), matrix(runif(10), ncol=2))
#' pY <- DataFrame(poly=I(spm2))
#'
#' rbind(pX, pY)
#' pX[c(1,1,2,3,3),,drop=FALSE]
#' 
#' @docType methods
#' @name spatula-bind
#' @aliases
#' bindROWS,SpatialPoints-method
#' bindROWS,SpatialPolygons-method
#' extractROWS,SpatialPolygons-method
#' showAsCell,SpatialPoints-method
#' showAsCell,SpatialPolygons-method
NULL

#' @export
#' @importFrom S4Vectors bindROWS
setMethod("bindROWS", "SpatialPoints", function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE) {
    out <- bindROWS(.PointsVector(x), lapply(objects, .PointsVector),
        ignore.mcols=ignore.mcols, use.names=use.names, check=check)
    out@spatial
})

#' @export
#' @importFrom S4Vectors bindROWS
setMethod("bindROWS", "SpatialPolygons", function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE) {
    out <- bindROWS(.PointsVector(x), lapply(objects, .PointsVector),
        ignore.mcols=ignore.mcols, use.names=use.names, check=check)
    out@spatial
})

#' @export
#' @importFrom S4Vectors extractROWS
setMethod("extractROWS", "SpatialPolygons", function(x, i) {
    # WHAT A PAIN! Need to override sp's refusal to duplicate entries.
    ids <- .get_ids(x@polygons)
    if (anyDuplicated(ids[i])) {
        ids[i] <- make.unique(ids[i])
        ok <- .replace_ids(pl, ids)
    }

    x[i]
})

#' @importFrom sp coordinates
.coord_shower <- function(object) {
    X <- coordinates(object)
    collected <- lapply(seq_len(ncol(X)), function(i) showAsCell(X[,i,drop=FALSE]))
    output <- do.call(paste, c(collected, list(sep=", ")))
    sprintf("(%s)", output)
}

#' @export
#' @importFrom sp SpatialPoints
setMethod("showAsCell", "SpatialPoints", .coord_shower)

#' @export
#' @importFrom sp SpatialPolygons
setMethod("showAsCell", "SpatialPolygons", .coord_shower) # uses the lab_pts, probably the average.
