test_that("file repair works", {
    x2 <- dv_read(dv_example_file())
    ## no issues, x should be returned unchanged
    xr2 <- dv_repair(x2)
    expect_identical(x2, xr2)
    ## force a repairable duplicate ID
    x2$meta$players_h$player_id[14] <- x2$meta$players_h$player_id[4]
    xr2 <- dv_repair(x2)
    expect_equal(nrow(xr2$messages), (nrow(x2$messages) + 1L))
    expect_true(grepl("did not take to the court, removing from roster", tail(xr2$messages$message, 1)))
    expect_equal(nrow(xr2$meta$players_h), (nrow(x2$meta$players_h) - 1L))
    ## an unrepairable duplicate ID
    x2$meta$players_v$player_id[1] <- x2$meta$players_h$player_id[1]
    expect_error(xr2 <- dv_repair(x2))

    x2 <- dv_read(dv_example_file())
    ## repairable duplicate numbers
    x2$meta$players_h$number[14] <- x2$meta$players_h$number[1]
    x2$meta$players_v$number[2] <- x2$meta$players_v$number[1]
    x2$meta$players_v$number[8] <- x2$meta$players_v$number[3]
    xr2 <- dv_repair(x2)
    expect_equal(nrow(xr2$messages), (nrow(x2$messages) + 3L))
    expect_true(all(grepl("did not take to the court, removing from roster", tail(xr2$messages$message, 3))))
    expect_equal(nrow(xr2$meta$players_h), (nrow(x2$meta$players_h) - 1L))
    expect_equal(nrow(xr2$meta$players_v), (nrow(x2$meta$players_v) - 2L))
    ## unrepairable duplicate numbers
    x2$meta$players_v$number[3] <- x2$meta$players_v$number[1]
    expect_error(xr2 <- dv_repair(x2))

})
