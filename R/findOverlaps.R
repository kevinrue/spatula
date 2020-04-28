#' Find overlaps in spatial data
#'
#' Wrappers around the \code{findOverlaps} machinery from the \pkg{IRanges} package,
#' to allow them to work with various \pkg{sp} classes.
#'
#' @section Finding overlaps:
#' \code{findOverlaps(query, subject, maxgap=NULL, minoverlap=NULL, 
#' type=NULL, select=c("all", "first", "last", "arbitrary"), ...)}
#' finds the specified overlaps between \code{query} and \code{subject}.
#' By default, it returns a \linkS4class{Hits} object specifying pairs of overlapping entries between \code{query} and \code{subject};
#' for other values of \code{select}, it instead returns an integer vector of length equal to \code{query},
#' containing the entry with the specified type of overlap in \code{subject}.
#' 
#' Methods are currently provided for the following combinations of classes:
#' \itemize{
#' \item \code{query} is a \linkS4class{SpatialPoints} instance,
#' \code{subject} is a \linkS4class{SpatialPolygons} instance (and vice versa).
#' \item \code{query} and \code{subject} are both \linkS4class{SpatialPolygons} instances.
#' This requires installation of the \pkg{rgeos} package.
#' }
#' Any \code{*DataFrame} subclasses of the above instances can also be supplied.
#' 
#' \code{maxgap}, \code{minoverlap} and \code{type} are currently ignored.
#'
#' @author Aaron Lun
#'
#' @examples
#' spt <- sp::SpatialPoints(matrix(runif(100), ncol=2))
#' spm <- makeSpatialPolygons(
#'    rbind(c(0,0), c(0.5, 0.2), c(0.2, 0.6)),
#'    rbind(c(1,1), c(0.3, 0.7), c(0.1, 0.2), c(0.8, 0.3))
#' )
#' 
#' findOverlaps(spt, spm)
#' findOverlaps(spt, spm, select="first")
#'
#' findOverlaps(spm, spt)
#'
#' findOverlaps(spm, spm)
#'
#' @name findOverlaps
#' @aliases
#' findOverlaps,SpatialPoints,SpatialPolygons-method
#' findOverlaps,SpatialPolygons,SpatialPoints-method
#' findOverlaps,SpatialPolygons,SpatialPolygons-method
NULL

#' @importFrom sp over geometry
#' @importFrom S4Vectors Hits selectHits
.overlapper <- function(query, subject, select) {
    # Clean out any 'DataFrame' nonsense.
    query <- geometry(query)
    subject <- geometry(subject)

    if (select %in% c("first", "arbitrary")) {
        over(query, subject, returnList=FALSE)
    } else {
        ov <- over(query, subject, returnList=TRUE)
        out <- Hits(rep(seq_along(query), lengths(ov)), unlist(ov, use.names=FALSE), 
            nLnode=length(query), nRnode=length(subject))
        selectHits(out, select=select)
    }
}

#' @export
#' @importFrom IRanges findOverlaps
setMethod("findOverlaps", c("SpatialPoints", "SpatialPolygons"), 
    function(query, subject, maxgap = NULL, minoverlap = NULL, type = NULL, 
        select = c("all", "first", "last", "arbitrary"), ...) 
{
    .overlapper(query, subject, select=match.arg(select))
})

#' @export
#' @importFrom IRanges findOverlaps
setMethod("findOverlaps", c("SpatialPolygons", "SpatialPoints"), 
    function(query, subject, maxgap = NULL, minoverlap = NULL, type = NULL, 
        select = c("all", "first", "last", "arbitrary"), ...) 
{
    .overlapper(query, subject, select=match.arg(select))
})

#' @export
#' @importFrom IRanges findOverlaps
setMethod("findOverlaps", c("SpatialPolygons", "SpatialPolygons"), 
    function(query, subject, maxgap = NULL, minoverlap = NULL, type = NULL, 
        select = c("all", "first", "last", "arbitrary"), ...) 
{
    .overlapper(query, subject, select=match.arg(select))
})
