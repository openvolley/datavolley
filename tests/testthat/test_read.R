context("dvw file reader")

test_that("text encoding is guessed correctly", {
    expect_output(read_dv(system.file("extdata/example_data.dvw",package="datavolley"),insert_technical_timeouts=FALSE,verbose=TRUE),"Using text encoding: UTF-8$")
    tmp <- capture_output(read_dv(system.file("extdata/PM06.dvw",package="datavolley"),insert_technical_timeouts=FALSE,verbose=TRUE))
    expect_true(grepl("Using text encoding: windows-1250",tmp))
})
