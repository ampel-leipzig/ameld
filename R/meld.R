#' Calculate MELD scores
#'
#' Calculate MELD, MELD-Na scores and MELD or MELD-Plus score based survival
#' probabilities.
#'
#' @param creatinine `numeric`, \[mg/dl\]
#' @param bilirubin `numeric`, \[mg/dl\]
#' @param inr `numeric`
#' @param dialysis `logical`, had dialysis twice, or 24 hours of CVVHD, within
#' a week prior to the serum creatinine test?
#' @param cause `character`, cause of cirrhosis. Has to be of the same length as
#' `creatinine`, `bilirubin`, `inr` and `dialysis`. Just `"ethyltoxic"`,
#' `"cholestatic"`, `"unos"` and `"other"` are supported.
#' Use `"unos"` for the United Network for Organ Sharing definition of the score.
#' @param round `logical`, round to nearest integer?
#' @return `numeric`, MELD or MELD-Na score for `meld` and `meld_na`
#' respectively. `pmeld` and `pmeld_plus7` return the survival probability based
#' on the MELD and MELD-Plus7 score.
#'
#' @details
#' Laboratory values (creatinine, bilirubin, INR) below 1.0 are set to 1.0 and
#' creatinine above 4.0 is set to 4.0 as described in Wiesner et al. 2003.
#'
#' @references
#' Michael Malinchoc et al. 2000.
#' "A model to predict poor survival in patients undergoing transjugular
#' intrahepatic portosystemic shunts"
#' Hepatology 31 (4): 864-871.
#' \doi{10.1053/he.2000.5852}
#'
#' United Network for Organ Sharing (UNOS). Policy notice 11/2015:
#' OPTN executive committee actions. Available at:
#' https://optn.transplant.hrsa.gov/media/1575/policynotice_20151101.pdf
#' Accessed: September 11, 2019.
#' @export
#' @examples
#' meld(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, cause = "other")
meld <- function(creatinine, bilirubin, inr, dialysis = FALSE, cause = "other",
                 round = FALSE) {
    cause <- tolower(cause)
    is_valid <- cause %in% c("other", "unos", "ethyltoxic", "cholestatic")
    if (any(!is_valid)) {
        warning(
            "Found unknown cause(s): ", enum(cause[!is_valid]), ".\n",
            "Using \"other\" instead."
        )
        cause[!is_valid] <- "other"
    }
    cause <- as.integer(cause == "other" | cause == "unos")
    ## if dialysis == TRUE => creatinine == 4.0 mg/dl
    creatinine <- creatinine + as.integer(dialysis) * 4.0
    score <- (0.957 * log(pmax(pmin(creatinine, 4), 1)) +
              0.378 * log(pmax(bilirubin, 1)) +
              1.12 * log(pmax(inr, 1)) + 0.643 * cause)
    if (round)
        score <- round(score, 1L)

    unname(score * 10)
}

#' @rdname meld
#'
#' @param sodium `numeric`, \[mmol/l\]
#' @param type `character(1)`, type of implementation
#'
#' @references
#' Kim et al. 2008.
#' "Hyponatremia and Mortality among Patients on the Liver-Transplant Waiting
#' List"
#' N Engl J Med 2008; 359: 1018-1026
#' \doi{10.1056/NEJMoa0801209}
#' @export
#' @examples
#'
#' meld_na(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, sodium = 135)
#' meld_na(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, sodium = 135,
#'         type = "unos")
meld_na <- function(creatinine, bilirubin, inr, sodium, dialysis = FALSE,
                    type = c("kim2008", "unos"),
                    cause = "other", round = FALSE) {
    type <- match.arg(type)
    if (type == "kim2008") {
        sodium <- pmax(pmin(sodium, 140), 125)
        score <- meld(
            creatinine = creatinine, bilirubin = bilirubin, inr = inr,
            dialysis = dialysis, cause = cause, round = FALSE
        )
        score <- score - sodium - (0.025 * score * (140 - sodium)) + 140
    } else {
        sodium <- pmax(pmin(sodium, 137), 125)
        score <- meld(
            creatinine = creatinine, bilirubin = bilirubin, inr = inr,
            dialysis = dialysis, cause = "other", round = FALSE
        )
        score <- ifelse(
            !is.na(score) & score > 11,
            score + 1.32 * (137 - sodium) - (0.033 * score * (137 - sodium)),
            score
        )
    }
    if (round)
        score <- round(score, 0L)

    unname(score)
}

#' @rdname meld
#'
#' @param albumin `numeric`, \[g/dl\]
#' @param female `logical`
#' @references
#' Kim et al. 2021.
#' "MELD 3.0: The Model for End-Stage Liver Disease Updated for the Modern Era"
#' Gastroenterology 2021; 161: 1887-1895.e4
#' \doi{10.1053/j.gastro.2021.08.050}
#' @export
#' @examples
#'
#' meld3(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, sodium = 135, albumin = 3.5)
meld3 <- function(creatinine, bilirubin, inr, sodium, albumin,
                  female = TRUE, round = FALSE) {

    lcreatinine <- log(pmax(creatinine, 1))
    lbilirubin <- log(pmax(bilirubin, 1))
    linr <- log(pmax(inr, 1))
    sodium <- 137 - pmax(pmin(sodium, 137), 125)
    albumin <- 3.5 - pmax(pmin(albumin, 3.5), 1.5)

    score <- 1.33 * as.integer(female) +
             4.56 * lbilirubin +
             0.82 * sodium - 0.24 * sodium * lbilirubin +
             9.09 * linr +
             11.14 * lcreatinine +
             1.85 * albumin - 1.83 * albumin * lcreatinine + 6

    if (round)
        score <- round(score, 0L)

    unname(score)
}

#' @rdname meld
#'
#' @param meld `numeric`, MELD/MELD-Na score as calculated by `meld` or
#' `meld-na`.
#'
#' @references
#' Wiesner et al. 2003.
#' "Model for end-stage liver disease (MELD) and allocation of donor livers"
#' Gastroentrology, 124 (1): 91-96
#' \doi{10.1053/gast.2003.50016}
#' @export
#' @examples
#'
#' pmeld(creatinine = 1.9, bilirubin = 4.2, inr = 1.2, cause = "other")
#' pmeld(meld = 20)
pmeld <- function(meld = NULL, creatinine, bilirubin, inr,
                  dialysis = FALSE, cause = "other") {
    if (is.null(meld))
        meld <- meld(
            creatinine = creatinine, bilirubin = bilirubin, inr = inr,
            dialysis = dialysis, cause = cause, round = FALSE
        )

    0.98465^exp((meld - 10) * 0.1635)
}

#' @rdname meld
#'
#' @param wbc `numeric`, \[Gpt/l\]
#' @param age `numeric`, \[years\]
#'
#' @details
#' The original MELD-Plus risk score reports a mortality probability. To provide
#' a common interface with `pmeld` `pmeld_plus7` returns a survival probabilitiy
#' instead.
#'
#' @references
#' Kartoun et al. 2017.
#' "The MELD-Plus: A generalizable prediction risk score in cirrhosis"
#' PloS one, 12 (10): e0186301
#' \doi{10.1371/journal.pone.0186301}
#' @export
#' @examples
#'
#' pmeld_plus7(2.5, 4.1, 1.2, 137, 24, 6.7, 56)
pmeld_plus7 <- function(creatinine, bilirubin, inr, sodium, albumin, wbc, age,
                       round = FALSE) {
    score <- 8.53499496 + 2.59679650 * log10(1 + creatinine) +
        2.06503238 * log10(1 + bilirubin) + 2.99724802 * log10(1 + inr) -
        6.47834101 * log10(1 + sodium) - 6.34990436 * log10(1 + albumin) +
        1.92811726 * log10(1 + wbc) + 0.04070442 * age
    score <- exp(score) / (1 + exp(score))

    if (round)
        score <- round(score, 2L)

    unname(1 - score)
}
