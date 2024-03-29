test_that("meld", {
    expect_equal(
        meld(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, cause = "ethyltoxic"),
        13.6091626
    )
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
    ## vectorized arguments
    expect_equal(
        meld(
            creatinine = c(0.9, 1.9), bilirubin = c(0.9, 4.2), inr = c(1, 1.2),
            cause = c("other", "ethyltoxic")
        ), c(6.43, 13.6091626)
    )
    ## unkown cause argument
    expect_warning(
        meld(
            creatinine = c(0.9, 1.9), bilirubin = c(0.9, 4.2), inr = c(1, 1.2),
            cause = c("other", "foobar")
        ), "unknown cause"
    )
    expect_equal(
        suppressWarnings(
            meld(creatinine = 0.9, bilirubin = 0.9, inr = 0.9, cause = "foobar")
        ),
        meld(creatinine = 0.9, bilirubin = 0.9, inr = 0.9, cause = "OTHER")
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
                type = "unos"),
        meld_na(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, sodium = 138,
                type = "unos")
    )
    expect_equal(
        meld_na(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, sodium = 125,
                type = "unos"),
        meld_na(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, sodium = 124,
                type = "unos")
    )

    expect_equal(
        meld_na(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, sodium = 125,
                type = "unos", round = TRUE), 28
    )
    ## below 11
    expect_equal(
        meld_na(creatinine = c(1.9, 1.0), bilirubin = c(4.2, 1.0),
                c(inr = 1.2, 1.0), sodium = c(137, 125), type = "unos"),
        c(meld_na(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, sodium = 137,
                  type = "unos"),
          meld_na(creatinine = 1.0, bilirubin = 1.0, inr = 1.0, sodium = 125,
                  type = "unos"))
    )
    expect_equal(
        meld_na(creatinine = 1.0, bilirubin = 1.0, inr = 1.0, sodium = 137,
                type = "unos"),
        meld_na(creatinine = 1.0, bilirubin = 1.0, inr = 1.0, sodium = 125,
                type = "unos")
    )
    ## NA
    expect_equal(
        meld_na(creatinine = 1.0, bilirubin = 1.0, inr = 1.0, sodium = NA),
        NA_real_
    )
    expect_equal(
        meld_na(creatinine = 1.0, bilirubin = 1.0, inr = 1.0, sodium = NA,
                type = "unos"), 6.43 # below 11
    )
    expect_equal(
        meld_na(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, sodium = NA,
                type = "unos"), NA_real_
    )
    expect_equal(
        meld_na(creatinine = 1.9, bilirubin = 4.2, inr = NA, sodium = 125,
                type = "unos"), NA_real_
    )
    expect_equal(
        meld_na(creatinine = 1.0, bilirubin = 1.0, inr = 1.0, sodium = 137,
                dialysis = TRUE, type = "unos"),
        meld_na(creatinine = 4.0, bilirubin = 1.0, inr = 1.0, sodium = 137,
                type = "unos")
    )
})

test_that("meld3", {
    expect_equal(
        meld3(
            creatinine = 1.9, bilirubin = 4.2, inr = 1.2, albumin = 3.5,
            sodium = 140, female = TRUE, round = TRUE
        ), 23
    )
    expect_equal(
        meld3(
            creatinine = 1.9, bilirubin = 4.2, inr = 1.2, albumin = 3.5,
            sodium = 140, female = FALSE, round = TRUE
        ), 21
    )
    expect_equal(
        meld3(
            creatinine = 1.9, bilirubin = 4.2, inr = 1.2, albumin = 1.5,
            sodium = 140, female = TRUE, round = TRUE
        ), 24
    )
    ## Kim 2021, Table 4
    expect_equal(
        meld3(
            female = c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE),
            bilirubin = rep(c(2.5, 6, 12), c(2, 3, 3)),
            sodium = rep(c(131, 128), c(5, 3)),
            inr = rep(c(1, 1.5, 2.2), c(2, 3, 3)),
            creatinine = rep(c(1.2, 1.5, 1.8, 2.8), c(2, 3, 1, 2)),
            albumin = rep(c(3.8, 3.5, 2.2, 2.0), c(2, 1, 2, 3)),
            round = TRUE
        ),
        c(16, 17, 25:27, 34, 38, 39)
    )
})

test_that("pmeld", {
    expect_equal(pmeld(20), 0.9237, tolerance = 1e-4)
    expect_equal(
        pmeld(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, cause = "other"),
        0.9232, tolerance = 1e-4
    )
})

test_that("pmeld3", {
    ## Kim 2021, Supplementary Table 5:
    expect_equal(
        pmeld3(c(14, 15, 26, 28)),
        c(0.981, 0.977, 0.855, 0.791),
        tolerance = 1e-2
    )
})

test_that("pmeld_plus7", {
    expect_equal(
        pmeld_plus7(
            creatinine = 1.37, bilirubin = 2.49, inr = 1.5, sodium = 136.5,
            albumin = 2.89, wbc = 6.67, age = 60, round = TRUE
        ), 1 - 0.16
    )
})
