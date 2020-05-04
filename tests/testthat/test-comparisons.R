# This tests the various comparison machinery.
# library(testthat); library(spatula); source("setup.R"); source("test-comparisons.R")

test_that("SpatialPoints comparisons work as expected", {
    # Equality comparisons.
    expect_true(all(points1==points1))
    expect_true(!any(points1==points2))

    # Matching.
    chosen <- 2:5
    expect_identical(chosen, match(points1[chosen], points1))

    m <- match(points1, points1[chosen])
    expect_identical(m[chosen], seq_along(chosen))
    expect_true(all(is.na(m[-chosen])))

    expect_identical(points1 %in% points1[chosen], !is.na(m))

    # Duplication.
    expect_identical(BiocGenerics::unique(rbind(points1, points1)), points1)
    expect_identical(BiocGenerics::unique(rbind(points1, points2)), rbind(points1, points2))

    # Ordering.
    o <- order(points1)
    expect_false(is.unsorted(coordinates(points1)[o,1]))
    expect_identical(BiocGenerics::sort(points1), points1[o])
})

test_that("SpatialPointsDataFrame comparisons work as expected", {
    # Equality comparisons.
    expect_true(all(pointsdf1==pointsdf1))
    expect_true(!any(pointsdf1==pointsdf2))

    # Matching.
    chosen <- 2:5
    expect_identical(chosen, match(pointsdf1[chosen,], pointsdf1))

    m <- match(pointsdf1, pointsdf1[chosen,])
    expect_identical(m[chosen], seq_along(chosen))
    expect_true(all(is.na(m[-chosen])))

    expect_identical(pointsdf1 %in% pointsdf1[chosen,], !is.na(m))

    # Duplication.
    expect_identical(BiocGenerics::unique(rbind(pointsdf1, pointsdf1)), pointsdf1)
    expect_identical(BiocGenerics::unique(rbind(pointsdf1, pointsdf2)), rbind(pointsdf1, pointsdf2))

    # Ordering.
    o <- order(pointsdf1)
    expect_false(is.unsorted(coordinates(pointsdf1)[o,1]))
    expect_identical(BiocGenerics::sort(pointsdf1), pointsdf1[o,])
})

test_that("SpatialPolygons comparisons work as expected", {
    # Equality comparisons.
    expect_true(all(polys1==polys1))
    expect_true(!any(polys1==polys2))

    # Matching.
    chosen <- 2:3
    expect_identical(chosen, match(polys1[chosen], polys1))

    m <- match(polys1, polys1[chosen])
    expect_identical(m[chosen], seq_along(chosen))
    expect_true(all(is.na(m[-chosen])))

    expect_identical(polys1 %in% polys1[chosen], !is.na(m))

    # Duplication.
    self <- BiocGenerics::unique(bindROWS(polys1, list(polys1)))
    expect_identical(coordinates(self), coordinates(polys1))
    combined <- bindROWS(polys1, list(polys2))
    expect_identical(BiocGenerics::unique(combined), combined)

    # Ordering.
    o <- order(polys1)
    expect_false(is.unsorted(coordinates(polys1)[o,1]))
    sorted <- BiocGenerics::sort(polys1)
    expect_identical(coordinates(sorted), coordinates(polys1)[o,])
})

test_that("SpatialPolygons comparisons work with tie breaking", {
    X <- matrix(0, nrow=10, ncol=2)

    # Manufacturing a clone with the same center but different value.
    polys <- makeSpatialPolygons(
        rbind(X, X),
        X,
        X,
        rbind(X, X, X),
        matrix(runif(20), ncol=2),
        rbind(X, X)
    )

    # Naive comparison based on the center doesn't work, 
    # but we're a little smarter than that.
    naive <- DataFrame(coordinates(polys))
    expect_identical(sameAsPreviousROW(naive),
         c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))

    expect_identical(sameAsPreviousROW(spatula:::.PolygonsVector(polys)), 
         c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))

    # Ordering is similarly intelligent.
    ordered <- order(polys)
    is.same <- sameAsPreviousROW(spatula:::.PolygonsVector(polys[ordered]))
    expect_identical(sum(is.same), 2L)

    naiveo <- order(naive)
    naive.same <- sameAsPreviousROW(spatula:::.PolygonsVector(polys[naiveo]))
    expect_identical(sum(naive.same), 1L) # for comparison.
})

test_that("SpatialPolygonsDataFrame comparisons work as expected", {
    # Equality comparisons.
    expect_true(all(polysdf1==polysdf1))
    expect_true(!any(polysdf1==polysdf2))

    # Matching.
    chosen <- 2:3
    expect_identical(chosen, match(polysdf1[chosen,], polysdf1))

    m <- match(polysdf1, polysdf1[chosen,])
    expect_identical(m[chosen], seq_along(chosen))
    expect_true(all(is.na(m[-chosen])))

    expect_identical(polysdf1 %in% polysdf1[chosen,], !is.na(m))

    # Duplication.
    expect_equivalent(BiocGenerics::unique(rbind(polysdf1, polysdf1)), polysdf1)
    combined <- bindROWS(polysdf1, list(polysdf2))
    expect_equivalent(BiocGenerics::unique(combined), combined)

    # Ordering.
    o <- order(polysdf1)
    expect_false(is.unsorted(coordinates(polysdf1)[o,1]))
    expect_equivalent(BiocGenerics::sort(polysdf1), polysdf1[o,])
})
