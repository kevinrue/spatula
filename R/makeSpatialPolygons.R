#' Construct SpatialPolygons
#'
#' A convenience function to construct \linkS4class{SpatialPolygons} from a list of coordinate matrices.
#'
#' @param ... One or more 2-dimensional numeric matrices of coordinates, 
#' where each matrix represents a polygon to be treated as a separate entry in the \linkS4class{SpatialPolygons}.
#' @param hole See \code{\link{Polygon}}; if \code{NULL}, that function's default value is used.
#' @param proj4string See \code{\link{SpatialPolygons}}; if \code{NULL}, that function's default value is used.
#' 
#' @details
#' For simple polygons, this utility avoids the hassle of creating a \linkS4class{Polygon},
#' and then creating a \linkS4class{Polygons}, and then \linkS4class{SpatialPolygons}.
#' However, more complex cases (involving several polygons per entry of the \linkS4class{SpatialPolygons} object)
#' will have to be constructed manually.
#'
#' @return
#' A \linkS4class{SpatialPolygons} object containing the specified polygons.
#'
#' @author Aaron Lun
#' @examples
#' spm <- makeSpatialPolygons(
#'     rbind(c(0,0), c(0.5, 0.2), c(0.2, 0.6), c(0.9, 0.8)),
#'     rbind(c(1,1), c(0.3, 0.7), c(0.1, 0.2), c(0.8, 0.3))
#' )
#' spm
#'
#' @export
#' @importFrom sp Polygon Polygons SpatialPolygons
makeSpatialPolygons <- function(..., hole=NULL, proj4string=NULL) {
    x <- list(...)

    if (is.null(hole)) {
        args <- list()
    } else {
        args <- list(hole=hole)
    }
    as.poly <- mapply(FUN=Polygon, coords=x, MoreArgs=args, SIMPLIFY=FALSE)
    as.poly <- lapply(as.poly, list)
    
    if (is.null(names(x))) {
        IDs <- seq_along(as.poly)
    } else {
        IDs <- names(x)
    }
    as.polys <- mapply(FUN=Polygons, srl=as.poly, ID=IDs, SIMPLIFY=FALSE)

    if (is.null(proj4string)) {
        args <- list()
    } else {
        args <- list(proj4string=proj4string)
    }
    do.call(SpatialPolygons, c(list(as.polys), args))
}
