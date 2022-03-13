context("click n scout utils")

test_that("click n scout utilities work as expected", {
    expect_error(dv_attack_code_map())
    expect_error(dv_attack_code_map(c()))
    expect_error(dv_attack_code_map("H", c()))
    expect_error(dv_attack_code_map(c(), 1L))
    expect_identical(dv_attack_code_map("H", "4"), dv_attack_code_map("H", 4L))
    expect_identical(dv_attack_code_map("H", c(4, 3, 2)), c("V5", "V3", "V6"))
    expect_error(dv_attack_code_map(c("H", "Q"), c(4, 3, 2)), "different lengths")
    expect_identical(dv_attack_code_map(c("H", "Q", "T"), c(4, 3, 2)), c("V5", "X1", "X6"))
    expect_identical(dv_attack_code_map("H", NA), "Unknown high ball attack from NA")
    expect_identical(dv_attack_code_map("Z", 4), "Unknown attack tempo from 4")

    expect_error(dv_attack_code2desc())
    expect_error(dv_attack_code2desc(c()))
    expect_identical(dv_attack_code2desc("X5"), c(X5 = "Shoot in 4"))
    expect_identical(dv_attack_code2desc(c("X5", "X8", "XYZ", NA_character_)), setNames(c("Shoot in 4", "Shoot in 1", NA_character_, NA_character_), c("X5", "X8", "XYZ", NA_character_)))

    expect_error(dv_attack_code2loc())
    expect_error(dv_attack_code2loc(c()))
    expect_identical(dv_attack_code2loc("CB"), c(CB = 4970))
    expect_identical(dv_attack_code2loc(c("CB", "CD", "PP", "XYZ", NA_character_)), setNames(c(4970, 4970, 4964, NA_real_, NA_real_), c("CB", "CD", "PP", "XYZ", NA_character_)))

    expect_error(dv_attack_code2side())
    expect_error(dv_attack_code2side(c()))
    expect_identical(dv_attack_code2side("CB"), c(CB = "L"))
    expect_identical(dv_attack_code2side(c("CB", "CD", "P2", "XYZ", NA_character_)), setNames(c("L", "L", "C", NA_character_, NA_character_), c("CB", "CD", "P2", "XYZ", NA_character_)))

    expect_error(dv_attack_code2set_type())
    expect_error(dv_attack_code2set_type(c()))
    expect_identical(dv_attack_code2set_type("V0"), c(V0 = "F"))
    expect_identical(dv_attack_code2set_type(c("V0", "X0", "XP", "X1", "PR", "XYZ", NA_character_)), setNames(c("F", "F", "P", "C", "-", NA_character_, NA_character_), c("V0", "X0", "XP", "X1", "PR", "XYZ", NA_character_)))

    x <- dv_read(dv_example_file(1), insert_technical_timeouts = FALSE)
    ma <- dv_create_meta_attacks(plays(x))
    expect_true(is.data.frame(ma))
    expect_equal(nrow(ma), 28)
})
