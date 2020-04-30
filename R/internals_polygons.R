.PolygonsVector <- function(x) {
    X <- new(".PolygonsVector", spatial=x)

    # What on earth is going on here? SpatialPolygonsDF
    # gets coerced to a SpatialPolygons by new()!
    X@spatial <- x

    X
}

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

.get_areas <- function(pl) {
    vapply(pl, slot, name="area", 0)
}

.uniquify_ids <- function(pl) {
    ids <- .get_ids(pl)
    if (anyDuplicated(ids)) {
        ids <- make.unique(ids)
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

    x@spatial <- polys
    x
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
setMethod("xtfrm", ".PolygonsVector", function(x) {
    ref <- .as_spatial(x)
    coords <- .mat2list(coordinates(ref))
    all.polys <- .get_polygons(ref)
    metrics <- c(coords, list(.get_areas(all.polys)))

    o <- do.call(order, metrics)
    output <- seq_along(o)
    output[o] <- output

    has.ties <- lapply(metrics, function(x) sameAsPreviousROW(x[o]))
    has.ties <- Reduce("&", has.ties)

    if (any(has.ties)) {
        # Iterating through the runs of ties to rearrange them. This could
        # probably be done in a better way with some kind of base64 encoding,
        # but I didn't want to introduce another dependency, so WHATEVER.
        tie.start <- which(!has.ties)
        tie.end <- c(tie.start[-1]-1L, length(has.ties))
        keep <- tie.start!=tie.end

        for (i in which(keep)) {
            curi <- tie.start[i]:tie.end[i]
            curo <- o[curi]
            curp <- all.polys[curo]

            # AW GROSS!
            deparsed <- vapply(curp, FUN=function(x) {
                x <- .wipe_id(x)
                paste(deparse(x), collapse="")
            }, FUN.VALUE="")

            dep.ranks <- rank(deparsed, ties.method="min")
            rank.dex <- selfmatch(dep.ranks)
            output[curo] <- output[curo][rank.dex]
        }
    }

    output
})
