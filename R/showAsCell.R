#' Showing \pkg{sp} classes
#'
#' Implements dedicated \code{\link{showAsCell}} methods for the various \pkg{sp} classes
#' so that they play nice with Bioconductor objects,
#' particularly when being stored as \code{\link{DFRame}} columns.
#'
#' @param object An instance of an \pkg{sp} class with vector-like characteristics,
#' e.g., a \linkS4class{SpatialPointsDataFrame}.
#' 
#' @return
#' A character vector containing a representation of each element of \code{object}.
#'
#' @examples
#' coords <- matrix(runif(30), 10)
#' X <- sp::SpatialPoints(coords)
#' showAsCell(X)
#'
#' df <- DataFrame(location=I(X), other=LETTERS[1:10])
#' df
#'
#' # Works for spatial polygons as well, based on the polygon centroid.
#' spm <- makeSpatialPolygons(matrix(runif(10), ncol=2), matrix(runif(10), ncol=2))
#' DataFrame(poly=I(spm))
#'
#' @author Aaron Lun
#' @name showAsCell
NULL

#' @importFrom sp coordinates
.coord_shower <- function(object) {
    X <- coordinates(object)
    collected <- lapply(seq_len(ncol(X)), function(i) showAsCell(X[,i,drop=FALSE]))
    output <- do.call(paste, c(collected, list(sep=", ")))
    sprintf("(%s)", output)
}

#' @export
#' @rdname showAsCell
#' @importFrom sp SpatialPoints
setMethod("showAsCell", "SpatialPoints", .coord_shower)

#' @export
#' @rdname showAsCell
#' @importFrom sp SpatialPolygons
setMethod("showAsCell", "SpatialPolygons", .coord_shower) # uses the lab_pts, probably the average.
