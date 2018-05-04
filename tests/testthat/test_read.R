context("dvw file reader")

test_that("text encoding is guessed correctly", {
    expect_output(read_dv(dv_example_file(1), insert_technical_timeouts=FALSE, verbose=TRUE), "Using text encoding: UTF-8$")
    tmp <- capture_output(read_dv(dv_example_file(2),insert_technical_timeouts=FALSE, verbose=TRUE))
    expect_true(grepl("Using text encoding: windows-1250", tmp))
})
