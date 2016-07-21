context("Skill evaluation decoder")

test_that("skill_evaluation_decoder generally functions as expected", {
    expect_error(skill_evaluation_decoder())
    expect_error(skill_evaluation_decoder("X"))
    expect_error(skill_evaluation_decoder("X","$"))

    expect_equal(skill_evaluation_decoder("E","#"),"Perfect")
    expect_equal(skill_evaluation_decoder("E","x"),"unknown set evaluation")    
})

