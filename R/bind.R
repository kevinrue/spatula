#' ROWS methods for Spatial objects
#'
#' Implements methods for \code{\link{bindROWS}} and \code{\link{extractROWS}} 
#' for \linkS4class{SpatialPoints} and \linkS4class{SpatialPolygons} objects,
#' so that they are compatible with Bioconductor containers.
#' See the \pkg{S4Vectors} package for more details.
#'
#' @author Aaron Lun
#' @examples
#' # Storing Spatial classes in containers:
#' coords <- matrix(runif(30), nrow=10)
#' X <- DataFrame(location=I(sp::SpatialPoints(coords)))
#' 
#' coords2 <- matrix(runif(15), nrow=5)
#' Y <- DataFrame(location=I(sp::SpatialPoints(coords2)))
#'
#' # Subsetting and such just works:
#' rbind(X, Y)
#' X[1:10,,drop=FALSE]
#' 
#' @docType methods
#' @name spatula-bind
#' @aliases
#' bindROWS,SpatialPoints-method
#' bindROWS,SpatialPolygons-method
#' extractROWS,SaptialPolygons-method
NULL

#' @export
#' @rdname spatula-bind
#' @importFrom S4Vectors bindROWS
setMethod("bindROWS", "SpatialPoints", function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE) {
    out <- bindROWS(.PointsVector(x), lapply(objects, .PointsVector),
        ignore.mcols=ignore.mcols, use.names=use.names, check=check)
    out@spatial
})

#' @export
#' @rdname spatula-bind
#' @importFrom S4Vectors bindROWS
setMethod("bindROWS", "SpatialPolygons", function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE) {
    out <- bindROWS(.PointsVector(x), lapply(objects, .PointsVector),
        ignore.mcols=ignore.mcols, use.names=use.names, check=check)
    out@spatial
})

#' @export
#' @rdname spatula-bind
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
