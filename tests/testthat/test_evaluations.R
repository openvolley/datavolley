context("Skill evaluation decoder")

test_that("skill_evaluation_decoder generally functions as expected", {
    sed <- skill_evaluation_decoder()
    expect_error(sed())
    expect_error(sed("X"))
    expect_equal(sed("XX","$"),"Unknown skill: XX")
    expect_equal(sed("X","$"),"Cannot decode evaluation for unknown skill X")

    expect_equal(sed("E","#"),"Perfect")
    expect_equal(sed("E","x"),"Unknown set evaluation")    
})

