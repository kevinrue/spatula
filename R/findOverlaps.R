#' Find overlaps in spatial data
#'
#' Wrappers around the \code{findOverlaps} machinery from the \pkg{IRanges} package,
#' to allow them to work with various \pkg{sp} classes.
#'
#' @section Finding overlaps between points:
#' \code{findOverlaps(query, subject, maxgap=1e-8, minoverlap=NULL, 
#' type=NULL, select=c("all", "first", "last", "arbitrary"), ...)}
#' finds all overlaps between the \linkS4class{SpatialPoints} objects \code{query} and \code{subject}.
#' A point is considered to overlap another if they lie within \code{maxgap} of each other;
#' the default non-zero \code{maxgap} means that this function effectively serves as an error-tolerant version of \code{\link{match}}.
#' 
#' By default, it returns a \linkS4class{Hits} object specifying pairs of overlapping entries between \code{query} and \code{subject};
#' for other values of \code{select}, it instead returns an integer vector of length equal to \code{query},
#' containing the entry with the specified type of overlap in \code{subject}.
#'
#' \code{subject} may also be missing, in which case overlaps are found between points in \code{query}.
#' In this case, a \linkS4class{SelfHits} object is returned when \code{select="all"}.
#'
#' \code{minoverlap} and \code{type} are currently ignored.
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
#' findOverlaps,SpatialPoints,missing-method
#' findOverlaps,SpatialPoints,SpatialPolygons-method
#' findOverlaps,SpatialPoints,SpatialPolygons-method
#' findOverlaps,SpatialPolygons,SpatialPoints-method
#' findOverlaps,SpatialPolygons,SpatialPolygons-method
NULL

########################################

#' @importFrom S4Vectors SelfHits Hits selectHits
.process_search_output <- function(searched, select, nsubjects) {
    nqueries <- length(searched)

    if (select=="all") {
        LEFT <- rep(seq_along(searched), lengths(searched))
        RIGHT <- unlist(searched)

        if (is.na(nsubjects)) {
            hits <- SelfHits(from=LEFT, to=RIGHT, nnode=nqueries)
        } else {
            hits <- Hits(from=LEFT, to=RIGHT, nLnode=nqueries, nRnode=nsubjects)
        }
        return(selectHits(hits, select=select))
    }

    if (select=="first") {
        FUN <- min
    } else if (select=="last") {
        FUN <- max
    } else {
        FUN <- function(x) x[1]
    }

    has.any <- lengths(searched) > 0
    output <- rep(NA_integer_, nqueries)
    output[has.any] <- vapply(searched[has.any], FUN, 0L)
    output
}

#' @export
#' @importFrom S4Vectors Hits selectHits
#' @importFrom BiocNeighbors queryNeighbors VptreeParam
setMethod("findOverlaps", c("SpatialPoints", "SpatialPoints"), 
    function(query, subject, maxgap = 1e-8, minoverlap = NULL, type = NULL, 
        select = c("all", "first", "last", "arbitrary"), ...) 
{
    searched <- queryNeighbors(query=coordinates(query), X=coordinates(subject), 
        threshold=maxgap, BNPARAM=VptreeParam(), get.distance=FALSE) # VP-trees are pretty fast at low dimensions.
    .process_search_output(searched$index, select=match.arg(select), nsubjects=length(subject))
})

#' @export
#' @importFrom BiocNeighbors findNeighbors VptreeParam
#' @importFrom S4Vectors SelfHits selectHits
setMethod("findOverlaps", c("SpatialPoints", "missing"), 
    function(query, subject, maxgap = 1e-8, minoverlap = NULL, type = NULL, 
        select = c("all", "first", "last", "arbitrary"), ...) 
{
    searched <- findNeighbors(coordinates(query), threshold=maxgap, BNPARAM=VptreeParam(), get.distance=FALSE)
    .process_search_output(searched$index, select=match.arg(select), nsubjects=NA)
})

########################################

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
