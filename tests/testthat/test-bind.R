# This tests the new binding functionality of Spatial classes.
# library(testthat); library(spatula); source("setup.R"); source("test-bind.R")

test_that("c() works for SpatialPoints", {
    expect_identical(points1, c(points1))

    out <- c(points1, points2)
    expect_s4_class(out, "SpatialPoints")
    expect_identical(coordinates(out), rbind(coordinates(points1), coordinates(points2)))

    out2 <- c(out, points1)
    expect_s4_class(out2, "SpatialPoints")
    expect_identical(coordinates(out2), rbind(coordinates(out), coordinates(points1)))
})

test_that("c() works for SpatialPointsDataFrame", {
    expect_identical(pointsdf1, c(pointsdf1))

    out <- c(pointsdf1, pointsdf2)
    expect_s4_class(out, "SpatialPointsDataFrame")
    expect_identical(coordinates(out), rbind(coordinates(pointsdf1), coordinates(pointsdf2)))
    expect_identical(out$X, c(pointsdf1$X, pointsdf2$X))

    out2 <- c(out, pointsdf1)
    expect_s4_class(out2, "SpatialPointsDataFrame")
    expect_identical(coordinates(out2), rbind(coordinates(out), coordinates(pointsdf1)))
    expect_identical(out2$X, c(out$X, pointsdf1$X))
})

test_that("c() works for SpatialPolygons", {
    expect_identical(polys1, c(polys1))

    out <- c(polys1, polys2)
    expect_s4_class(out, "SpatialPolygons")
    expect_equivalent(coordinates(out), rbind(coordinates(polys1), coordinates(polys2)))
    expect_identical(0L, anyDuplicated(rownames(coordinates(out))))

    out2 <- c(out, polys1)
    expect_s4_class(out2, "SpatialPolygons")
    expect_equivalent(coordinates(out2), rbind(coordinates(out), coordinates(polys1)))
    expect_identical(0L, anyDuplicated(rownames(coordinates(out2))))
})

test_that("c() works for SpatialPolygonsDataFrame", {
    expect_equivalent(polysdf1, c(polysdf1))

    out <- c(polysdf1, polysdf2)
    expect_s4_class(out, "SpatialPolygonsDataFrame")
    expect_equivalent(coordinates(out), rbind(coordinates(polysdf1), coordinates(polysdf2)))
    expect_identical(out$X, c(polysdf1$X, polysdf2$X))

    out2 <- c(out, polysdf1)
    expect_s4_class(out2, "SpatialPolygonsDataFrame")
    expect_equivalent(coordinates(out2), rbind(coordinates(out), coordinates(polysdf1)))
    expect_identical(out2$X, c(out$X, polysdf1$X))
})


