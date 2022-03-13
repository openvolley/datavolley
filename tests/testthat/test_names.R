context("name checking and cleaning")

test_that("check_player_names works", {
    x <- dv_read(dv_example_file(1), insert_technical_timeouts=FALSE)
    cpn <- check_player_names(x)
    ## expect only two similar names here, i.e. one row
    expect_equal(nrow(cpn), 1)
    expect_identical(sort(c(cpn$player1, cpn$player2)), c("ANJA HRIBERNIK 96", "ANJA HRIBERNIK 97"))
})
