# This tests the new binding functionality of Spatial classes.
# library(testthat); library(spatula); source("setup.R"); source("test-bind.R")

points1 <- DataFrame(loc=I(points1))
points2 <- DataFrame(loc=I(points2))
pointsdf1 <- DataFrame(loc=I(pointsdf1))
pointsdf2 <- DataFrame(loc=I(pointsdf2))
polys1 <- DataFrame(loc=I(polys1))
polys2 <- DataFrame(loc=I(polys2))
polysdf1 <- DataFrame(loc=I(polysdf1))
polysdf2 <- DataFrame(loc=I(polysdf2))

test_that("rbind() works for SpatialPoints columns", {
    expect_equivalent(points1, rbind(points1))

    out <- rbind(points1, points2)
    expect_s4_class(out$loc, "SpatialPoints")
    expect_identical(coordinates(out$loc), rbind(coordinates(points1$loc), coordinates(points2$loc)))

    out2 <- rbind(out, points1)
    expect_s4_class(out2$loc, "SpatialPoints")
    expect_identical(coordinates(out2$loc), rbind(coordinates(out$loc), coordinates(points1$loc)))
})

test_that("rbind() works for SpatialPointsDataFrame columns", {
    expect_equivalent(pointsdf1, rbind(pointsdf1))

    out <- rbind(pointsdf1, pointsdf2)
    expect_s4_class(out$loc, "SpatialPointsDataFrame")
    expect_identical(coordinates(out$loc), rbind(coordinates(pointsdf1$loc), coordinates(pointsdf2$loc)))
    expect_identical(out$loc$X, c(pointsdf1$loc$X, pointsdf2$loc$X))

    out2 <- rbind(out, pointsdf1)
    expect_s4_class(out2$loc, "SpatialPointsDataFrame")
    expect_identical(coordinates(out2$loc), rbind(coordinates(out$loc), coordinates(pointsdf1$loc)))
    expect_identical(out2$loc$X, c(out$loc$X, pointsdf1$loc$X))
})

test_that("rbind() works for SpatialPolygons columns", {
    expect_equivalent(polys1, rbind(polys1))

    out <- rbind(polys1, polys2)
    expect_s4_class(out$loc, "SpatialPolygons")
    expect_equivalent(coordinates(out$loc), rbind(coordinates(polys1$loc), coordinates(polys2$loc)))
    expect_identical(0L, anyDuplicated(rownames(coordinates(out$loc))))

    out2 <- rbind(out, polys1)
    expect_s4_class(out2$loc, "SpatialPolygons")
    expect_equivalent(coordinates(out2$loc), rbind(coordinates(out$loc), coordinates(polys1$loc)))
    expect_identical(0L, anyDuplicated(rownames(coordinates(out2$loc))))
})

test_that("rbind() works for SpatialPolygonsDataFrame columns", {
    expect_equivalent(polysdf1, rbind(polysdf1))

    out <- rbind(polysdf1, polysdf2)
    expect_s4_class(out$loc, "SpatialPolygonsDataFrame")
    expect_equivalent(coordinates(out$loc), rbind(coordinates(polysdf1$loc), coordinates(polysdf2$loc)))
    expect_identical(out$loc$X, c(polysdf1$loc$X, polysdf2$loc$X))

    out2 <- rbind(out, polysdf1)
    expect_s4_class(out2$loc, "SpatialPolygonsDataFrame")
    expect_equivalent(coordinates(out2$loc), rbind(coordinates(out$loc), coordinates(polysdf1$loc)))
    expect_identical(out2$loc$X, c(out$loc$X, polysdf1$loc$X))
})
