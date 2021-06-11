#' Calculate MELD scores
#'
#' Calculate MELD, MELD-Na and MELD-Plus scores.
#'
#' @param creatinine `numeric`, \[mg/dl\]
#' @param bilirubin `numeric`, \[mg/dl\]
#' @param inr `numeric`
#' @param dialysis `logical`, had dialysis twice, or 24 hours of CVVHD, within
#' a week prior to the serum creatinine test?
#' @param cause `character`
#' @param round `logical`, round to nearest integer?
#' @return `numeric`
#'
#' @details
#' Laboratory values (creatinine, bilirubin, INR) below 1.0 are set to 1.0 and
#' creatinine above 4.0 is set to 4.0.
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
meld <- function(creatinine, bilirubin, inr, dialysis = FALSE,
                 cause = "other", round = FALSE) {
    cause <- as.integer(cause == "other" | cause == "unos")
    ## if dialysis == TRUE => creatinine == 4.0 mg/dl
    creatinine <- creatinine + as.integer(dialysis) * 4.0
    creatinine <- pmax(pmin(creatinine, 4), 1)
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
#'         type = "UNOS")
meld_na <- function(creatinine, bilirubin, inr, sodium, dialysis = FALSE,
                    type = c("Kim2008", "UNOS"),
                    cause = "other", round = FALSE) {
    type <- match.arg(type)
    if (type == "Kim2008") {
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
#' @param wbc `numeric`, \[Gpt/l\]
#' @param age `numeric`, \[years\]
#'
#' @details
#' In contrast to `meld` and `meldNa`, `meldPlus7` returns a probability.
#'
#' @references
#' Kartoun et al. 2017.
#' "The MELD-Plus: A generalizable prediction risk score in cirrhosis"
#' PloS one, 12 (10): e0186301
#' \doi{10.1371/journal.pone.0186301}
#' @export
#' @examples
#' meld_plus7(2.5, 4.1, 1.2, 137, 24, 6.7, 56)
meld_plus7 <- function(creatinine, bilirubin, inr, sodium, albumin, wbc, age,
                       round = FALSE) {
    score <- 8.53499496 + 2.59679650 * log10(1 + creatinine) +
        2.06503238 * log10(1 + bilirubin) + 2.99724802 * log10(1 + inr) -
        6.47834101 * log10(1 + sodium) - 6.34990436 * log10(1 + albumin) +
        1.92811726 * log10(1 + wbc) + 0.04070442 * age
    score <- exp(score) / (1 + exp(score))

    if (round)
        score <- round(score, 2L)

    unname(score)
}
