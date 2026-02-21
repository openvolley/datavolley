context("remapping of attack codes")

test_that("dv_remap_attack_codes works", {
    x <- dv_read(dv_example_file())
    ac0 <- dv_meta_attack_codes(x)

    ## and their usage in the play-by-play data
    tbl0 <- table(plays(x)$attack_code)

    ## relabel V5 and V6 as Z5 and Z6
    ## (leaving their other details unchanged, because Z5 and Z6 do not exist in the attack codes table)
    x2 <- dv_remap_attack_codes(x, from_codes = c("V5", "V6"), to_codes = c("Z5", "Z6"))

    ## check that the table has changed
    expect_true(!any(dv_meta_attack_codes(x2)$code %in% c("V5", "V6")))
    ## and their usage in the play-by-play data
    expect_true(!any(plays(x2)$attack_code %in% c("V5", "V6")))

    ## relabel "C5" and "JJ" to "X5" (they are all fast sets to position 4)
    x2 <- dv_remap_attack_codes(x, from_codes = c("C5", "JJ"), to_codes = "X5")
    expect_true(!any(dv_meta_attack_codes(x2)$code %in% c("C5", "JJ")))
    expect_true(!any(plays(x2)$attack_code %in% c("C5", "JJ")))
    expect_equal(sum(dv_meta_attack_codes(x2)$code == "X5"), 1)
    expect_false(!any(plays(x2)$attack_code == "X5", na.rm = TRUE))

    ## remove an attack code
    expect_warning(x2 <- dv_remap_attack_codes(x, from_codes = c("C5", "JJ"), to_codes = ""))
    expect_identical(x, x2) ## should have made no change

    expect_warning(x2 <- dv_remap_attack_codes(x, from_codes = c("C5", "JJ"), to_codes = NA))
    expect_identical(x, x2) ## should have made no change

    x2 <- dv_remap_attack_codes(x, from_codes = c("C5", "JJ"), to_codes = "", allow_remove = TRUE)
    expect_true(!any(dv_meta_attack_codes(x2)$code %in% c("C5", "JJ")))
    expect_true(!any(plays(x2)$attack_code %in% c("C5", "JJ")))
    ## but this time they have just been removed, not remapped
    expect_equal(sum(!is.na(plays(x)$attack_code)), sum(!is.na(plays(x2)$attack_code)) + sum(plays(x)$attack_code %in% c("C5", "JJ")))

    ## remove an attack code that appears in the plays but not in the table
    x2 <- x
    x2$meta$attacks <- x2$meta$attacks[x2$meta$attacks$code != "V5", ]
    x3 <- dv_remap_attack_codes(x2, from_codes = "V5", to_codes = "", allow_remove = TRUE)
    ## should be no warning
    ## but if we remap it, we get a warning
    expect_warning(x3 <- dv_remap_attack_codes(x2, from_codes = "V5", to_codes = "ZZ", allow_remove = TRUE), "is not defined in the metadata")

    ## remove an attack code that does not appear at all
    x2 <- dv_remap_attack_codes(x, from_codes = "ZZ", to_codes = "", allow_remove = TRUE)
    ## should be no warning, but also should have no effect
    expect_identical(x, x2)

    ## provide a non-two-character string input code
    expect_error(x2 <- dv_remap_attack_codes(x, from_codes = "V55", to_codes = "", allow_remove = TRUE), "two-character string")

    ## provide a non-two-character string replacement code
    expect_error(x2 <- dv_remap_attack_codes(x, from_codes = "V5", to_codes = "ZZZ", allow_remove = TRUE), "two-character string")
    expect_error(x2 <- dv_remap_attack_codes(x, from_codes = "V5", to_codes = 66, allow_remove = TRUE), "not a character vector")

    ## check that the scout code changes
    c1a <- sum(plays(x)$attack_code == "V5", na.rm = TRUE)
    c1b <- length(grep("^......V5", plays(x)$code))
    expect_equal(c1a, c1b)
    expect_equal(length(grep("^......ZZ", plays(x)$code)), 0)
    x2 <- dv_remap_attack_codes(x, from_codes = "V5", to_codes = "ZZ")
    expect_equal(length(grep("^......ZZ", plays(x2)$code)), c1b)
    expect_equal(sum(plays(x2)$attack_code == "V5", na.rm = TRUE), 0)
    expect_equal(sum(plays(x2)$attack_code == "ZZ", na.rm = TRUE), c1a)

    ## provide an invalid replacement code
    expect_warning(x2 <- dv_remap_attack_codes(x2, from_codes = "V5", to_codes = "QQ"), "must start with")
})
