context("test reading of sq files")

test_that("sq files can be read", {
    f <- system.file("extdata/99.sq", package = "datavolley")
    sx <- dv_read_sq(f)
    expect_identical(names(sx), c("team", "players"))
})
