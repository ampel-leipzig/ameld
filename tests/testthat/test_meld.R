test_that("meld", {
    expect_equal(
        meld(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, cause = "other"),
        20.0391626
    )
    expect_equal(
        meld(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, cause = "other",
             round = TRUE), 20
    )

    ## laboratory values below 1 are set to 1
    expect_equal(
        meld(creatinine = 0.9, bilirubin = 0.9, inr = 0.9),
        meld(creatinine = 1, bilirubin = 1, inr = 1)
    )
    ## creatinine above 4 is set to 4
    expect_equal(
        meld(creatinine = 4.1, bilirubin = 1, inr = 1),
        meld(creatinine = 4, bilirubin = 1, inr = 1)
    )
    ## dialysis
    expect_equal(
        meld(creatinine = 1.0, bilirubin = 1, inr = 1, dialysis = TRUE),
        meld(creatinine = 4, bilirubin = 1, inr = 1)
    )
})

test_that("meld_na", {
    ## NA 125-140
    expect_equal(
        meld_na(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, sodium = 140),
        meld_na(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, sodium = 141)
    )
    expect_equal(
        meld_na(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, sodium = 125),
        meld_na(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, sodium = 124)
    )
    ## NA 125-137
    expect_equal(
        meld_na(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, sodium = 137,
                type = "UNOS"),
        meld_na(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, sodium = 138,
                type = "UNOS")
    )
    expect_equal(
        meld_na(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, sodium = 125,
                type = "UNOS"),
        meld_na(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, sodium = 124,
                type = "UNOS")
    )

    expect_equal(
        meld_na(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, sodium = 125,
                type = "UNOS", round = TRUE), 28
    )
    ## below 11
    expect_equal(
        meld_na(creatinine = c(1.9, 1.0), bilirubin = c(4.2, 1.0),
                c(inr = 1.2, 1.0), sodium = c(137, 125), type = "UNOS"),
        c(meld_na(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, sodium = 137,
                  type = "UNOS"),
          meld_na(creatinine = 1.0, bilirubin = 1.0, inr = 1.0, sodium = 125,
                  type = "UNOS"))
    )
    expect_equal(
        meld_na(creatinine = 1.0, bilirubin = 1.0, inr = 1.0, sodium = 137,
                type = "UNOS"),
        meld_na(creatinine = 1.0, bilirubin = 1.0, inr = 1.0, sodium = 125,
                type = "UNOS")
    )
    ## NA
    expect_equal(
        meld_na(creatinine = 1.0, bilirubin = 1.0, inr = 1.0, sodium = NA),
        NA_real_
    )
    expect_equal(
        meld_na(creatinine = 1.0, bilirubin = 1.0, inr = 1.0, sodium = NA,
                type = "UNOS"), 6.43 # below 11
    )
    expect_equal(
        meld_na(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, sodium = NA,
                type = "UNOS"), NA_real_
    )
    expect_equal(
        meld_na(creatinine = 1.9, bilirubin = 4.2, inr = NA, sodium = 125,
                type = "UNOS"), NA_real_
    )
    expect_equal(
        meld_na(creatinine = 1.0, bilirubin = 1.0, inr = 1.0, sodium = 137,
                dialysis = TRUE, type = "UNOS"),
        meld_na(creatinine = 4.0, bilirubin = 1.0, inr = 1.0, sodium = 137,
                type = "UNOS")
    )
})

test_that("meld_plus7", {
    expect_equal(
        meld_plus7(
            creatinine = 1.37, bilirubin = 2.49, inr = 1.5, sodium = 136.5,
            albumin = 2.89, wbc = 6.67, age = 60, round = TRUE
        ), 0.16
    )
})
