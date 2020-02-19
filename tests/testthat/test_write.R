context("dvw file writer")

test_that("dv_write behaves", {
    ## read a file
    x0 <- read_dv(dv_example_file(1))
    ## write it
    outfile <- tempfile()
    dv_write(x0, file = outfile)
    ## read the one we just wrote
    x1 <- read_dv(outfile)

    ## it should be identical to the original one
    ##  except for some known differences
    ## fix the things that we know will be different
    x0$raw <- NULL
    x1$raw <- NULL
    x1$file_meta$lastchange_day <- x0$file_meta$lastchange_day
    x1$file_meta$lastchange_idp <- x0$file_meta$lastchange_idp
    x1$file_meta$lastchange_prg <- x0$file_meta$lastchange_prg
    x1$file_meta$lastchange_rel <- x0$file_meta$lastchange_rel
    x1$file_meta$lastchange_ver <- x0$file_meta$lastchange_ver
    x1$file_meta$lastchange_nam <- x0$file_meta$lastchange_nam

    x0$messages$file_line <- NA_character_
    x1$messages$file_line <- NA_character_

    x0$meta$match$text_encoding <- NA_character_
    x1$meta$match$text_encoding <- NA_character_

    x1$meta$filename <- x0$meta$filename
    ## x1 will have had an (empty) [3VIDEO] section inserted, but x0 does not have it
    ## that doesn't really matter but the file line numbers will be off by one because of it
    x1$plays$file_line_number[which(x1$plays$file_line_number >= 118)] <- x1$plays$file_line_number[which(x1$plays$file_line_number >= 118)]-1L
    x1$messages$file_line_number[which(x1$messages$file_line_number >= 118)] <- x1$messages$file_line_number[which(x1$messages$file_line_number >= 118)]-1L
    expect_equal(x0, x1)
})
