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
