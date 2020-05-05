# This tests the various overlapping functionality.
# library(testthat); library(spatula); source("setup.R"); source("test-findOverlaps.R")

set.seed(100000)
test_that("findOverlaps works between two sets of SpatialPoints", {
    for (threshold in c(0.1, 0.2, 0.5)) {
        hits <- findOverlaps(points1, points2, maxgap=threshold)
        tmp <- coordinates(points1)[queryHits(hits),,drop=FALSE] - coordinates(points2)[subjectHits(hits),,drop=FALSE]
        dists <- sqrt(rowSums(tmp^2))
        expect_true(all(dists <= threshold))
    }

    # Works with other 'select=' options.
    hits <- findOverlaps(points1, points2, maxgap=0.2)
    expect_identical(selectHits(hits, "first"), findOverlaps(points1, points2, maxgap=0.2, select="first"))
    expect_identical(selectHits(hits, "last"), findOverlaps(points1, points2, maxgap=0.2, select="last"))

    self <- findOverlaps(points1, maxgap=0.5)
    copy <- findOverlaps(points1, points1, maxgap=0.5)
    expect_identical(as.data.frame(self), as.data.frame(copy))

    # Serves as a match() equivalent.
    out <- findOverlaps(points1)
    expect_s4_class(out, "SelfHits")
    expect_identical(queryHits(out), seq_along(out))
    expect_identical(subjectHits(out), seq_along(out))

    # Works with no matches
    hits <- findOverlaps(points1[0])
    expect_identical(length(hits), 0L)
})

set.seed(100001)
test_that("findOverlaps works between SpatialPoints and SpatialPolygons", {
    coords <- matrix(runif(100), ncol=2)
    points <- SpatialPoints(coords)

    poly <- makeSpatialPolygons(
        rbind(c(0,0), c(0.5, 0), c(0.5, 0.5), c(0, 0.5)),
        rbind(c(1,1), c(0.5, 1), c(0.5, 0.5), c(1, 0.5)),
        rbind(c(0.5,0), c(1, 0), c(1, 0.5), c(0.5, 0.5)),
        rbind(c(0,1), c(0.5, 1), c(0.5, 0.5), c(0, 0.5))
    )

    # Checking that over() does its job.
    hits <- findOverlaps(points, poly)
    tmp <- coordinates(points)[queryHits(hits),,drop=FALSE]  - coordinates(poly)[subjectHits(hits),,drop=FALSE]
    expect_true(all(apply(abs(tmp), 1, max) < 0.25))

    # Works for other select=.
    hits <- findOverlaps(points, poly)
    expect_identical(selectHits(hits, "first"), findOverlaps(points, poly, select="first"))
    expect_identical(selectHits(hits, "last"), findOverlaps(points, poly, select="last"))

    # Works symmetrically.
    hits2 <- findOverlaps(poly, points)
    o <- order(subjectHits(hits2), queryHits(hits2))
    expect_identical(queryHits(hits), subjectHits(hits2)[o])
    expect_identical(subjectHits(hits), queryHits(hits2)[o])

    # Works with no matches
    hits <- findOverlaps(points[0], poly)
    expect_identical(length(hits), 0L)
})

set.seed(100002)
test_that("findOverlaps works between SpatialPoints and SpatialPolygons with maxgap=", {
    coords <- matrix(runif(1000), ncol=2)
    points <- SpatialPoints(coords)
    poly <- makeSpatialPolygons(
        rbind(c(0.25,0.25), c(0.75, 0.25), c(0.75, 0.75), c(0.25, 0.75))
    )

    # Approximate way of checking.
    fromcenter <- coordinates(points) - 0.5
    maxdist <- apply(abs(fromcenter), 1, max)
    for (limit in c(0, 0.1, 0.25)) {
        hits <- findOverlaps(points, poly, maxgap=limit)
        expect_true(all(maxdist[queryHits(hits)] <= limit + 0.25))
    }

    # Works for other select=.
    hits <- findOverlaps(points, poly, maxgap=0.2)
    expect_identical(selectHits(hits, "first"), findOverlaps(points, poly, maxgap=0.2, select="first"))
    expect_identical(selectHits(hits, "last"), findOverlaps(points, poly, maxgap=0.2, select="last"))

    # Works symmetrically.
    hits2 <- findOverlaps(poly, points, maxgap=0.2)
    o <- order(subjectHits(hits2), queryHits(hits2))
    expect_identical(queryHits(hits), subjectHits(hits2)[o])
    expect_identical(subjectHits(hits), queryHits(hits2)[o])

    # Works with no matches
    hits <- findOverlaps(points[0], poly, maxgap=0.2)
    expect_identical(length(hits), 0L)
})

set.seed(100002)
test_that("findOverlaps works between two sets of SpatialPolygons", {
    poly1 <- makeSpatialPolygons(
        rbind(c(0,0), c(0.4, 0), c(0.4, 0.4), c(0, 0.4)),
        rbind(c(0.6, 0.6), c(0.6, 1), c(1, 1), c(1, 0.6)),
        rbind(c(0.6, 0), c(1, 0), c(1, 0.4), c(0.6, 0.4)),
        rbind(c(0.4, 1), c(0.4, 0.6), c(0, 0.6), c(0, 1))
    )

    poly2 <- makeSpatialPolygons(
        rbind(c(0.25,0.25), c(0.75, 0.25), c(0.75, 0.75), c(0.25, 0.75))
    )

    # Works for vanilla overlaps.
    hits <- findOverlaps(poly1, poly2)
    expect_identical(queryHits(hits), seq_along(poly1))
    expect_identical(subjectHits(hits), rep(1L, length(hits)))

    # Works for self.
    hits <- findOverlaps(poly1)
    expect_identical(queryHits(hits), seq_along(hits))
    expect_identical(subjectHits(hits), seq_along(hits))

    # Works for maxgap=.
    hits <- findOverlaps(poly1, maxgap=0.2)
    expect_identical(length(hits), 12L) # doesn't get the diagonally opposite box.

    hits <- findOverlaps(poly1, maxgap=0.5)
    expect_identical(queryHits(hits), rep(seq_along(poly1), each=length(poly1)))
    expect_identical(subjectHits(hits), rep(seq_along(poly1), length(poly1)))

    # Works for minoverlap=.
    hits <- findOverlaps(poly1, poly2, minoverlap=1)
    expect_identical(length(hits), 0L) 

    hits <- findOverlaps(poly1, poly2, minoverlap=0.01)
    expect_identical(hits, findOverlaps(poly1, poly2))

    # Works for other select=.
    hits <- findOverlaps(poly1, maxgap=0.2)
    expect_identical(selectHits(hits, "first"), findOverlaps(poly1, maxgap=0.2, select="first"))
    expect_identical(selectHits(hits, "last"), findOverlaps(poly1, maxgap=0.2, select="last"))

    # Works symmetrically.
    hits <- findOverlaps(poly1, poly2)
    hits2 <- findOverlaps(poly2, poly1)
    o <- order(subjectHits(hits2), queryHits(hits2))
    expect_identical(queryHits(hits), subjectHits(hits2)[o])
    expect_identical(subjectHits(hits), queryHits(hits2)[o])

    # Works with no matches
    hits <- findOverlaps(poly1[0], poly2)
    expect_identical(length(hits), 0L)
})


