context("dvw file reader")

test_that("text encoding is guessed correctly", {
    expect_message(read_dv(dv_example_file(1), insert_technical_timeouts=FALSE, verbose=TRUE), "Using text encoding: UTF-8")
    tmp <- capture_messages(read_dv(dv_example_file(2),insert_technical_timeouts=FALSE, verbose=TRUE))
    expect_true(any(grepl("Using text encoding: windows-1250", tmp)))
})

test_that("file reading handles files with truncated lines", {
    x1 <- read_dv(dv_example_file(1))
    xt <- read_dv(system.file("extdata/testing_truncated.dvw",package="datavolley"))
    expect_equal(nrow(x1$plays), nrow(xt$plays))
})

test_that("supplying edited_meta works", {
    x <- read_dv(dv_example_file(1), insert_technical_timeouts=FALSE)
    edited_meta <- x$meta
    edited_meta$players_h$player_id[1] <- "warglewarglewargle"
    x1 <- read_dv(dv_example_file(1), insert_technical_timeouts=FALSE, edited_meta=edited_meta)
    blah <- all.equal(x, x1)
    ## player 1 is a libero, so expect a difference in meta$players_h and plays$player_id only
    expect_equal(length(blah), 2)
    expect_equal(sum(grepl("Component .meta.: Component .players_h.: Component .player_id.: 1 string mismatch", blah)), 1)
    expect_equal(sum(grepl("Component .plays.: Component .player_id.: 43 string mismatches", blah)), 1)
    ## now change a non-lib player
    edited_meta <- x$meta
    edited_meta$players_h$player_id[13] <- "warglewarglewargle"
    x1 <- read_dv(dv_example_file(1), insert_technical_timeouts=FALSE, edited_meta=edited_meta)
    blah <- all.equal(x, x1)
    ## expect differences in home_player_idxx cols
    expect_equal(length(blah), 8)
    expect_equal(sum(grepl("home_player_id[123456]", blah)), 6)
})
