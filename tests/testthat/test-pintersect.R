# Parallel intersection and union operations.
# library(testthat); library(spatula); source("setup.R"); source("test-pintersect.R")

poly <- makeSpatialPolygons(
    rbind(c(0,0), c(0.5, 0), c(0.5, 0.5), c(0, 0.5)),
    rbind(c(1,1), c(0.5, 1), c(0.5, 0.5), c(1, 0.5)),
    rbind(c(0.5,0), c(1, 0), c(1, 0.5), c(0.5, 0.5)),
    rbind(c(0,1), c(0.5, 1), c(0.5, 0.5), c(0, 0.5))
)

other <- makeSpatialPolygons(
    rbind(c(0.25, 0.25), c(0.25, 0.75), c(0.75, 0.75), c(0.75, 0.25))
) 

faraway <- makeSpatialPolygons(
    rbind(c(2,2), c(2,3), c(3,3), c(3,2))
)

test_that("pintersect works as expected", {
    out <- pintersect(poly, other)
    expect_identical(length(out), length(poly))

    originals <- spatula:::.get_areas(poly)
    areas <- spatula:::.get_areas(out)
    expect_true(all(areas < originals))
    expect_true(all(areas==areas[1]))

    X <- coordinates(out)
    expect_false(any(sameAsPreviousROW(X[,1]) & sameAsPreviousROW(X[,2])))
 
    # Mostly symmetric.
    expect_identical(coordinates(out), coordinates(pintersect(other, poly)))

    # Works when there is no intersection.
    out <- pintersect(poly, faraway)
    expect_identical(length(out), length(poly))
    expect_true(all(spatula:::.get_areas(out)==0))
})

test_that("punion works as expected", {
    out <- punion(poly, other)
    expect_identical(length(out), length(poly))

    originals <- spatula:::.get_areas(poly)
    areas <- spatula:::.get_areas(out)
    expect_true(all(areas > originals))
    expect_true(all(areas==areas[1]))

    X <- coordinates(out)
    expect_false(any(sameAsPreviousROW(X[,1]) & sameAsPreviousROW(X[,2])))

    # Mostly symmetric.
    expect_identical(coordinates(out), coordinates(punion(other, poly)))

    # Works when there is no intersection.
    out <- punion(poly, faraway)
    expect_identical(length(out), length(poly))
    expect_identical(spatula:::.get_areas(out), originals + 1)
}) 

test_that("psetdiff works as expected", {
    out <- psetdiff(poly, other)
    expect_identical(length(out), length(poly))

    originals <- spatula:::.get_areas(poly)
    areas <- spatula:::.get_areas(out)
    expect_true(all(areas < originals))
    expect_true(all(areas==areas[1]))

    X <- coordinates(out)
    expect_false(any(sameAsPreviousROW(X[,1]) & sameAsPreviousROW(X[,2])))

    # Asymmetric!
    expect_false(isTRUE(all.equal(coordinates(out), coordinates(psetdiff(other, poly)))))

    # Works when there is no intersection.
    out <- psetdiff(poly, faraway)
    expect_identical(length(out), length(poly))
    expect_identical(spatula:::.get_areas(out), originals)
}) 
