context("dvw file reader")

test_that("text encoding is guessed correctly", {
    expect_message(dv_read(dv_example_file(1), insert_technical_timeouts=FALSE, verbose=TRUE), "Using text encoding: UTF-8")
    expect_message(dv_read(dv_example_file(2), insert_technical_timeouts=FALSE, verbose=TRUE), "Using text encoding: windows-1250")
})

test_that("file reading handles files with truncated lines", {
    x1 <- dv_read(dv_example_file(1))
    xt <- dv_read(system.file("extdata/testing_truncated.dvw",package="datavolley"))
    expect_equal(nrow(x1$plays), nrow(xt$plays))
})

test_that("supplying edited_meta works", {
    x <- dv_read(dv_example_file(1), insert_technical_timeouts=FALSE)
    edited_meta <- x$meta
    edited_meta$players_h$player_id[1] <- "warglewarglewargle"
    x1 <- dv_read(dv_example_file(1), insert_technical_timeouts=FALSE, edited_meta=edited_meta)
    blah <- all.equal(x, x1)
    ## player 1 is a libero, so expect a difference in meta$players_h and plays$player_id only
    expect_equal(length(blah), 2)
    expect_equal(sum(grepl("Component .meta.: Component .players_h.: Component .player_id.: 1 string mismatch", blah)), 1)
    expect_equal(sum(grepl("Component .plays.: Component .player_id.: 43 string mismatches", blah)), 1)
    ## now change a non-lib player
    edited_meta <- x$meta
    edited_meta$players_h$player_id[13] <- "warglewarglewargle"
    x1 <- dv_read(dv_example_file(1), insert_technical_timeouts=FALSE, edited_meta=edited_meta)
    blah <- all.equal(x, x1)
    ## expect differences in home_player_idxx cols
    expect_equal(length(blah), 8)
    expect_equal(sum(grepl("home_player_id[123456]", blah)), 6)
})

test_that("supplying a preferred date format works", {
    ## first generate a modified example file
    x <- dv_read(dv_example_file(1))
    x$raw <- sub("01/25/2015;;2014/2015", "08/12/2015;;2014/2015", x$raw, fixed = TRUE)
    tf <- tempfile()
    writeLines(x$raw, tf)
    chk <- dv_read(tf)
    ## the last-generated date in this file is unambiguously mdy, so that will be used unless a date format has been specified
    expect_equal(chk$meta$match$date, lubridate::ymd("2015-08-12")) ## by default
    chk <- dv_read(tf, date_format = "mdy")
    expect_equal(chk$meta$match$date, lubridate::ymd("2015-08-12"))
    chk <- dv_read(tf, date_format = "dmy")
    expect_equal(chk$meta$match$date, lubridate::ymd("2015-12-08"))
    chk <- dv_read(tf, date_format = "ymd")
    ## can't be this format, so it's ignored
    expect_equal(chk$meta$match$date, lubridate::ymd("2015-08-12"))
    unlink(tf)

    ## now with ambiguous last-generated date
    x$raw <- sub("01/25/2015 15.44", "01/10/2015 15.44", x$raw, fixed = TRUE)
    tf <- tempfile()
    writeLines(x$raw, tf)
    chk <- dv_read(tf)
    expect_equal(chk$meta$match$date, lubridate::ymd("2015-12-08")) ## by default
    chk <- dv_read(tf, date_format = "mdy")
    expect_equal(chk$meta$match$date, lubridate::ymd("2015-08-12"))
    chk <- dv_read(tf, date_format = "dmy")
    expect_equal(chk$meta$match$date, lubridate::ymd("2015-12-08"))
    chk <- dv_read(tf, date_format = "ymd")
    expect_equal(chk$meta$match$date, lubridate::ymd("2015-12-08"))
    unlink(tf)
})
