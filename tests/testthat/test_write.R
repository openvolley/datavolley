context("dvw file writer")

test_that("dv_write behaves", {
    expect_dvws_equal <- function(dvw1, dvw2) {
        ## dvw2 should be identical to dvw1 except for some known differences
        ## fix the things that we know will be different
        if (!any(dvw1$raw %eq% "[3VIDEO]")) {
            ## dvw2 will have an (empty) [3VIDEO] section inserted if dvw1 did not have one
            vididx <- which(dvw2$raw %eq% "[3VIDEO]")
            dvw2$raw <- dvw2$raw[-vididx]
            ## that doesn't really matter but the file line numbers will be off by one because of it
            dvw2$plays$file_line_number[which(dvw2$plays$file_line_number >= vididx)] <- dvw2$plays$file_line_number[which(dvw2$plays$file_line_number >= vididx)]-1L
            dvw2$messages$file_line_number[which(dvw2$messages$file_line_number >= vididx)] <- dvw2$messages$file_line_number[which(dvw2$messages$file_line_number >= vididx)]-1L
        }
        if (TRUE) {
            ## don't compare raw yet
            dvw1$raw <- NULL
            dvw2$raw <- NULL
        } else {
            ## drop trailing empty line in dvw1 if it exists
            if (!nzchar(dvw1$raw[length(dvw1$raw)])) dvw1$raw <- dvw1$raw[-length(dvw1$raw)]
        }
        dvw2$file_meta$lastchange_day <- dvw1$file_meta$lastchange_day
        dvw2$file_meta$lastchange_idp <- dvw1$file_meta$lastchange_idp
        dvw2$file_meta$lastchange_prg <- dvw1$file_meta$lastchange_prg
        dvw2$file_meta$lastchange_rel <- dvw1$file_meta$lastchange_rel
        dvw2$file_meta$lastchange_ver <- dvw1$file_meta$lastchange_ver
        dvw2$file_meta$lastchange_nam <- dvw1$file_meta$lastchange_nam

        dvw1$meta$match$text_encoding <- NA_character_
        dvw2$meta$match$text_encoding <- NA_character_

        dvw2$meta$filename <- dvw1$meta$filename
        expect_equal(dvw1, dvw2)
    }
    test_read_write_dvw <- function(filename) {
        ## read a file
        x0 <- read_dv(filename)
        ## write it
        outfile <- tempfile()
        dv_write(x0, file = outfile)
        ## read the one we just wrote
        x1 <- read_dv(outfile)
        expect_dvws_equal(x0, x1)
    }
    test_read_write_dvw(dv_example_file(1))
    expect_warning(test_read_write_dvw(dv_example_file(2)), "missing the meta\\$(attacks|sets) component", all = TRUE)
})
