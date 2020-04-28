#' Combining \pkg{sp} class objects
#'
#' Utilities to combine instances of \pkg{sp} classes.
#' Perhaps these would have a better home in \pkg{sp} itself,
#' but until then, here we are.
#'
#' @param x,... Multiple \linkS4class{SpatialPoints} objects of the same dimensionality.
#' @param ignore.mcols,recursive Further arguments passed to \code{\link{c,Vector-method}}.
#'
#' @return An object of the same class as \code{x} containing coordinates from all objects in \code{...}.
#'
#' @author Aaron Lun
#'
#' @examples
#' coords <- matrix(runif(30), nrow=10)
#' X <- sp::SpatialPoints(coords)
#'
#' coords2 <- matrix(runif(15), nrow=5)
#' Y <- sp::SpatialPoints(coords2)
#'
#' c(X, Y, X, Y) 
#'
#' @name bind
#' @export
#' @importMethodsFrom S4Vectors c
setMethod("c", "SpatialPoints", function(x, ..., ignore.mcols = FALSE, recursive = FALSE) {
    stuff <- lapply(list(...), .PointsVector)
    out <- do.call(c, c(list(.PointsVector(x)), stuff, list(ignore.mcols=ignore.mcols, recursive=recursive)))
    out@points
})
