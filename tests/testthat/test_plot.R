context("plotting and related utils")

test_that("coordinate utilities work as expected", {
    x <- dv_read(dv_example_file(1), insert_technical_timeouts=FALSE)

    xyl <- dv_xy(1:6, end="lower")
    xyu <- dv_xy(1:6, end="upper")
    expect_identical(xyu, dv_flip_xy(xyl))

    xserves <- subset(plays(x), skill=="Serve")
    ## add fake coords
    coords <- dv_fake_coordinates("serve", xserves$evaluation)
    xserves[, c("start_coordinate", "start_coordinate_x", "start_coordinate_y",
                "end_coordinate", "end_coordinate_x", "end_coordinate_y")] <- coords
    
    i0 <- 1:10100
    xy0 <- dv_index2xy(i0)
    expect_identical(rownames(xy0), as.character(1:10100))
    ## xy to index and back again should be identity
    temp <- dv_index2xy(dv_xy2index(xy0))
    expect_identical(xy0, temp)

    ## flip xy and back again should be identity
    expect_equal(xy0, dv_flip_xy(dv_flip_xy(xy0))) ## "equal but not identical"??

    ## (flip xy then convert to index) should be same as flipping index directly
    expect_equal(dv_xy2index(dv_flip_xy(xy0)), dv_flip_index(i0))

    expect_equal(xy0, dv_index2xy(dv_flip_index(dv_xy2index(dv_flip_xy(xy0)))))

    
    temp <- dv_xy(zones = 1:9, end = "lower", as_for_serve = FALSE)
    expect_identical(temp$x, c(3, 3, 2, 1, 1, 2, 1, 2, 3))
    expect_identical(temp$y, c(1, 3, 3, 3, 1, 1, 2, 2, 2))

    temp <- dv_xy(zones = 1:9, end = "upper", as_for_serve = FALSE)
    expect_identical(temp$x, c(1, 1, 2, 3, 3, 2, 3, 2, 1))
    expect_identical(temp$y, c(6, 4, 4, 4, 6, 6, 5, 5, 5))

    temp <- dv_xy(zones = 1:9, end = "lower", as_for_serve = TRUE)
    expect_equal(temp$x, c(3.2, NA, NA, NA, 0.8, 2, 1.4, NA, 2.6))
    expect_identical(temp$y, c(0.5, NA, NA, NA, 0.5, 0.5, 0.5, NA, 0.5))

    temp <- dv_xy(zones = 1:9, end = "upper", as_for_serve = TRUE)
    expect_equal(temp$x, c(0.8, NA, NA, NA, 3.2, 2, 2.6, NA, 1.4))
    expect_identical(temp$y, c(6.5, NA, NA, NA, 6.5, 6.5, 6.5, NA, 6.5))

    tempz <- setNames(as.data.frame(expand.grid(1:9, c("A", "B", "C", "D"))), c("zone", "subzone"))
    tempz$subzone <- as.character(tempz$subzone)
    temp <- dv_xy(zones = tempz$zone, end = "lower", as_for_serve = FALSE, subzone = tempz$subzone)
    expect_identical(temp$x, rowSums(expand.grid(c(3, 3, 2, 1, 1, 2, 1, 2, 3), c(0.25, 0.25, -0.25, -0.25))))
    expect_identical(temp$y, rowSums(expand.grid(c(1, 3, 3, 3, 1, 1, 2, 2, 2), c(-0.25, 0.25, 0.25, -0.25))))

    temp <- dv_xy(zones = tempz$zone, end = "upper", as_for_serve = FALSE, subzone = tempz$subzone)
    expect_identical(temp$x, rowSums(expand.grid(c(1, 1, 2, 3, 3, 2, 3, 2, 1), c(-0.25, -0.25, 0.25, 0.25))))
    expect_identical(temp$y, rowSums(expand.grid(c(6, 4, 4, 4, 6, 6, 5, 5, 5), c(0.25, -0.25, -0.25, 0.25))))

    temp <- dv_xy(zones = tempz$zone, end = "lower", as_for_serve = TRUE, subzone = tempz$subzone)
    expect_equal(temp$x, rowSums(expand.grid(c(3.2, NA, NA, NA, 0.8, 2, 1.4, NA, 2.6), c(0.25, 0.25, -0.25, -0.25))))
    expect_identical(temp$y, rowSums(expand.grid(c(0.5, NA, NA, NA, 0.5, 0.5, 0.5, NA, 0.5), c(-0.25, 0.25, 0.25, -0.25))))

    temp <- dv_xy(zones = tempz$zone, end = "upper", as_for_serve = TRUE, subzone = tempz$subzone)
    expect_equal(temp$x, rowSums(expand.grid(c(0.8, NA, NA, NA, 3.2, 2, 2.6, NA, 1.4), c(-0.25, -0.25, 0.25, 0.25))))
    expect_identical(temp$y, rowSums(expand.grid(c(6.5, NA, NA, NA, 6.5, 6.5, 6.5, NA, 6.5), c(0.25, -0.25, -0.25, 0.25))))

    ## serving zones to coords
    expect_equal(dv_xy2zone(x = c(0.95, 1.15, 1.4, 2), y = 6.5, as_for_serve = TRUE), c(1L, 9L, 9L, 6L))

    ## finding coordinates to flip
    tst <- tibble(start_zone = c(4, 4, 4, 4, 3, 3, 3, 5, 7), start_coordinate_x = c(1, 1, 3, 3, 2, 2, 2, 1, 3), start_coordinate_y = c(3, 3.5, 4, 3.5, 3, 4, 3.5, 1, 5), skill = c(rep("Attack", 7), rep("Serve", 2)), end_coordinate_y = 2)
    expect_equal(dv_find_to_flip_coordinates(tst, target_start_end = "lower"), c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE))
    expect_equal(dv_find_to_flip_coordinates(tst, target_start_end = "upper"), !c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE))

    ## cones
    cxy <- dv_cone2xy(4, 1:7)
    expect_equal(dv_xy2cone(cxy$ex, cxy$ey, start_zone = 4), 1:7)
})
