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
    
    xy0 <- dv_index2xy(c(550, 2000))
    ##expect_identical(dv_xy2index(xy0), dv_flip_index(dv_xy2index(dv_flip_xy(xy0))))
    ## second value is off by 1, needs checking

})
