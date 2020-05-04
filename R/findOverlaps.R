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
#' By default, it returns a \linkS4class{Hits} object specifying pairs of overlapping entries between \code{query} and \code{subject}.
#' for other values of \code{select}, it instead returns an integer vector of length equal to \code{query},
#' containing the entry with the specified type of overlap in \code{subject}.
#'
#' \code{subject} may also be missing, in which case overlaps are found between points in \code{query}.
#' In this case, a \linkS4class{SelfHits} object is returned when \code{select="all"}.
#'
#' \code{minoverlap} and \code{type} are currently ignored.
#'
#' @section Finding overlaps between points and polygons:
#' \code{findOverlaps(query, subject, maxgap=0, minoverlap=NULL, 
#' type=NULL, select=c("all", "first", "last", "arbitrary"), ...)}
#' finds the specified overlaps between \code{query} and \code{subject},
#' where one is a \linkS4class{SpatialPoints} and the other is a \linkS4class{SpatialPolygons}.
#'
#' By default, it returns a \linkS4class{Hits} object specifying pairs of overlapping entries between \code{query} and \code{subject};
#' for other values of \code{select}, it instead returns an integer vector of length equal to \code{query},
#' containing the entry with the specified type of overlap in \code{subject}.
#' 
#' If \code{maxgap} is specified, overlaps are reported between points and polygons 
#' if the former lies within \code{maxgap} of the closest edge/vertex of the latter.
#' Note that this requires the additional installation of the \pkg{rgeos} package.
#'
#' \code{minoverlap} and \code{type} are currently ignored.
#'
#' @section Finding overlaps between polygons:
#' \code{findOverlaps(query, subject, maxgap=0, minoverlap=0, 
#' type=NULL, select=c("all", "first", "last", "arbitrary"), ...)}
#' finds the specified overlaps between the \linkS4class{SpatialPolygons} objects \code{query} and \code{subject}.
#' This requires the additional installation of the \pkg{rgeos} package.
#'
#' By default, it returns a \linkS4class{Hits} object specifying pairs of overlapping entries between \code{query} and \code{subject}.
#' For other values of \code{select}, it instead returns an integer vector of length equal to \code{query},
#' containing the entry with the specified type of overlap in \code{subject}.
#' 
#' \code{subject} may also be missing, in which case overlaps are found between points in \code{query}.
#' In this case, a \linkS4class{SelfHits} object is returned when \code{select="all"}.
#'
#' If \code{maxgap} is specified, overlaps are reported between polygons if their edges or vertices lie within \code{maxgap} of each other.
#' If \code{minoverlap} is specified, overlaps are only reported if the area of the intersection between two polygons is greater than \code{minoverlap}.
#'
#' \code{type} is currently ignored.
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
#' findOverlaps
#' findOverlaps,SpatialPoints,missing-method
#' findOverlaps,SpatialPoints,SpatialPoints-method
#' findOverlaps,SpatialPoints,SpatialPolygons-method
#' findOverlaps,SpatialPolygons,SpatialPoints-method
#' findOverlaps,SpatialPolygons,SpatialPolygons-method
#' findOverlaps,SpatialPolygons,missing-method
NULL

########################################

#' @importFrom S4Vectors SelfHits Hits selectHits
.process_search_output <- function(searched, select, nsubjects) {
    nqueries <- length(searched)

    if (select=="all") {
        LEFT <- rep(seq_along(searched), lengths(searched))
        RIGHT <- as.integer(unlist(searched))
        o <- order(LEFT, RIGHT)
        LEFT <- LEFT[o]
        RIGHT <- RIGHT[o]

        if (is.null(nsubjects)) {
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
    .process_search_output(searched$index, select=match.arg(select), nsubjects=NULL)
})

########################################

#' @importFrom sp over geometry
#' @importFrom S4Vectors Hits selectHits SelfHits
.overlapper <- function(query, subject, maxgap, select) {
    # Clean out any 'DataFrame' nonsense.
    query <- geometry(query)
    if (self <- is.null(subject)) {
        subject <- query
    } else {
        subject <- geometry(subject)
    }

    if (!length(query) || !length(subject)) { 
        # because rgeos can't handle zero-length queries.
        LEFT <- RIGHT <- integer(0)
    } else if (maxgap==0) {
        if (select %in% c("first", "arbitrary")) {
            idx <- over(query, subject, returnList=FALSE)
            return(unname(idx))
        } else {
            ov <- over(query, subject, returnList=TRUE)
            LEFT <- rep(seq_along(query), lengths(ov))
            RIGHT <- as.integer(unlist(ov, use.names=FALSE))
        }
    } else {
        # TODO: do this in chunks, so as to avoid blowing up memory.
        collected <- rgeos::gWithinDistance(query, subject, dist=maxgap, byid=TRUE)
        indices <- arrayInd(which(collected), dim(collected))
        LEFT <- indices[,2]
        RIGHT <- indices[,1]
    }

    if (self) {
        hits <- SelfHits(LEFT, RIGHT, nnode=length(query))
    } else {
        hits <- Hits(LEFT, RIGHT, nLnode=length(query), nRnode=length(subject))
    }
    selectHits(hits, select=select)
}

#' @export
#' @importFrom IRanges findOverlaps
setMethod("findOverlaps", c("SpatialPoints", "SpatialPolygons"), 
    function(query, subject, maxgap = 0, minoverlap = NULL, type = NULL, 
        select = c("all", "first", "last", "arbitrary"), ...) 
{
    .overlapper(query, subject, select=match.arg(select), maxgap=maxgap)
})

#' @export
#' @importFrom IRanges findOverlaps
setMethod("findOverlaps", c("SpatialPolygons", "SpatialPoints"), 
    function(query, subject, maxgap = 0, minoverlap = NULL, type = NULL, 
        select = c("all", "first", "last", "arbitrary"), ...) 
{
    .overlapper(query, subject, select=match.arg(select), maxgap=maxgap)
})

########################################

#' @importFrom S4Vectors queryHits subjectHits extractROWS
#' @importFrom IRanges pintersect
.apply_minoverlap <- function(olap, query, subject, minoverlap) {
    inter <- pintersect(extractROWS(query, queryHits(olap)), 
        extractROWS(subject, subjectHits(olap)))
    olap[.get_areas(inter) >= minoverlap]
}

#' @export
#' @importFrom S4Vectors selectHits
#' @importFrom IRanges findOverlaps
setMethod("findOverlaps", c("SpatialPolygons", "SpatialPolygons"), 
    function(query, subject, maxgap = 0, minoverlap = 0, type = NULL, 
        select = c("all", "first", "last", "arbitrary"), ...) 
{
    select <- match.arg(select)
    if (minoverlap==0) {
        .overlapper(query, subject, select=select, maxgap=maxgap)
    } else {
        out <- .overlapper(query, subject, select="all", maxgap=0)
        out <- .apply_minoverlap(out, query=query, subject=subject, minoverlap=minoverlap)
        selectHits(out, select=select)
    }
})

#' @export
#' @importFrom S4Vectors selectHits
#' @importFrom IRanges findOverlaps
setMethod("findOverlaps", c("SpatialPolygons", "missing"), 
    function(query, subject, maxgap = 0, minoverlap = 0, type = NULL, 
        select = c("all", "first", "last", "arbitrary"), ...) 
{
    select <- match.arg(select)
    if (minoverlap==0) {
        .overlapper(query, subject=NULL, select=match.arg(select), maxgap=maxgap)
    } else {
        out <- .overlapper(query, subject=NULL, select="all", maxgap=0)
        out <- .apply_minoverlap(out, query=query, subject=query, minoverlap=minoverlap)
        selectHits(out, select=select)
    }
})

#' @export
IRanges::findOverlaps
