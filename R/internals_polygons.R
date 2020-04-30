.PolygonsVector <- function(x) new(".PolygonsVector", spatial=x)

#' @importFrom S4Vectors vertical_slot_names
setMethod("vertical_slot_names", ".PolygonsVector", function(x) {
    c("spatial", callNextMethod())
})

# UGH! Need raw access to SpatialPolygons' `polygon` field.
# The obvious getter `polygons()` doesn't do what you might expect!
.get_polygons <- function(sp) sp@polygons

.get_ids <- function(pl) {
    vapply(pl, slot, name="ID", "")
}

.uniquify_ids <- function(pl) {
    ids <- .get_ids(pl)
    if (anyDuplicated(ids)) {
        ids <- make.names(ids, unique=TRUE)
        for (i in seq_along(pl)) {
            pl[[i]]@ID <- ids[i]
        }
    }
    pl 
}

#' @importFrom S4Vectors bindROWS
#' @importFrom sp SpatialPolygons SpatialPolygonsDataFrame
setMethod("bindROWS", ".PolygonsVector", function(x, objects=list(), use.names=TRUE, ignore.mcols=FALSE, check=TRUE) {
    ref <- .as_spatial(x)
    others <- lapply(objects, .as_spatial)

    obj.poly <- lapply(others, .get_polygons)
    obj.poly <- unlist(obj.poly, recursive=FALSE)
    ref.poly <- .get_polygons(ref)
    all.poly <- c(ref.poly, obj.poly)

    all.poly <- .uniquify_ids(all.poly)
    polys <- SpatialPolygons(all.poly, proj4string=.get_proj4string(ref))
    if (is(ref, "SpatialPolygonsDataFrame")) {
        df <- .combine_df(ref, others, length(all.poly), ignore.mcols=ignore.mcols)
        rownames(df) <- .get_ids(all.poly)
        polys <- SpatialPolygonsDataFrame(polys, df, match.ID=FALSE)
    }

    initialize(x, spatial=polys)
})

.wipe_id <- function(x) {
    x@ID <- ""
    x
}

#' @importFrom S4Vectors sameAsPreviousROW
setMethod("sameAsPreviousROW", ".PolygonsVector", function(x) {
    ref<- .as_spatial(x)
    out <- .mat2list(coordinates(ref))
    collected <- lapply(out, sameAsPreviousROW)
    tmp <- Reduce("&", collected)

    # A more rigorous check based on exact coordinates 
    # and other attributes (except the name).
    if (any(tmp)) {
        raw <- .get_polygons(ref)
        for (r in which(tmp)) {
            left <- .wipe_id(raw[[r]])
            right <- .wipe_id(raw[[r-1]])
            tmp[r] <- identical(left, right)
        }
    }

    unname(tmp)
})

#' @importFrom sp coordinates
#' @importFrom S4Vectors order
setMethod("order", ".PolygonsVector", function(..., na.last = TRUE, 
    decreasing = FALSE, method = c("auto", "shell", "radix")) 
{
    everything <- lapply(list(...), .as_spatial)
    coords.out <- lapply(everything, FUN=function(y) {
        .mat2list(coordinates(y))
    })

    coords.out2 <- unlist(coords.out, recursive=FALSE)
    args <- list(na.last=na.last, decreasing=decreasing, method=match.arg(method))
    o <- do.call(order, c(coords.out2, args))

    # Tie breaking. This could probably be done in a better way
    # but I simply could not be bothered at this point. Very 
    # unlikely that we'll get two polygons with the same centroid.
    has.ties <- lapply(coords.out[[1]], function(x) sameAsPreviousROW(x[o]))
    has.ties <- Reduce("&", has.ties)

    if (any(has.ties)) {
        deparsed <- everything
        for (i in seq_along(everything)) {
            current <- .get_polygons(everything[[i]])
            deparsed[[i]] <- vapply(current, FUN=function(x) {
                x <- .wipe_id(x)
                paste(deparse(x), collapse="")
            }, FUN.VALUE="")
        }

        o <- do.call(order, c(coords.out2, deparsed, args))
    }

    o
})
