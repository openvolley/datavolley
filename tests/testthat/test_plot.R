context("plotting and related utils")

test_that("coordinate utilities work as expected", {
    x <- read_dv(dv_example_file(1), insert_technical_timeouts=FALSE)

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
})
