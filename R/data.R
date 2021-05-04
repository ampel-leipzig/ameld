#' End-stage Liver Disease Data
#'
#' A dataset of patients evaluated for liver transplantation at the University
#' Hospital of Leipzig from November 2012 to June 2015.
#'
#' @format A `data.frame` with 654 rows and 48 columns:
#' \describe{
#' \item{Age}{`integer`, patient's age in years.}
#' \item{Sex}{`factor`, patient's sex encoded as `"M"` for male and `"W"` for
#' female.}
#' \item{DaysAtRisk}{`integer`, number of days from the time when the
#' laboratory measurements were taken to the last follow up.}
#' \item{Deceased}{`integer`, was the patient alive (`0`) or deceased (`1`)
#' on the last follow up.}
#' \item{LTx}{`integer`, `1` if the patient got a liver transplantation else
#' `0`.}
#' \item{Cirrhosis}{`integer`, `1` if the patient had a cirrhosis else `0`.}
#' \item{ALF}{`integer`, `1` if the patient had acute liver failure else `0`.}
#' \item{Ethyltoxic}{`integer`, `1` if the patient had a ethyltoxic liver
#' disease else `0`.}
#' \item{HBV}{`integer`, `1` if the patient had a liver
#' disease caused by an infection with the hepatitis B virus else `0`.}
#' \item{HCV}{`integer`, `1` if the patient had a liver
#' disease caused by an infection with the hepatitis C virus else `0`.}
#' \item{AIH}{`integer`, `1` if the patient had an autoimmune hepatitis else
#' `0`.}
#' \item{PBC}{`integer`, `1` if the patient had a primary sclerosing cirrhosis
#' else `0`.}
#' \item{PSC}{`integer`, `1` if the patient had a primary biliary cirrhosis else
#' `0`.}
#' \item{NASH}{`integer`, `1` if the patient had a non-alcoholic steatohepatitis
#' else `0`.}
#' \item{Cryptogenic}{`integer`, `1` if the patient had cirrhosis of unknown
#' origin else `0`.}
#' \item{Dialysis}{`integer`, `1` if the patient required renal replacement
#' therapy else `0`.}
#' \item{GIB}{`integer`, `1` if the patient suffered gastrointestinal bleedings
#' else `0`.}
#' \item{SBP}{`integer`, `1` if the patient suffered spontaneous bacterial
#' peritonitis else `0`.}
#' \item{HCC}{`integer`, `1` if the patient suffered hepatocellular carcinoma
#' else `0`.}
#' \item{ALAT_S}{`numeric`, serum level of ALAT, alanine aminotransferase in
#' µkat/l.}
#' \item{ALB_S}{`numeric`, serum level of albumin in g/l.}
#' \item{AP_S}{`numeric`, serum level of alkaline phosphatase in µkat/l.}
#' \item{ASAT_S}{`numeric`, serum level of ASAT, aspartate aminotransferase in
#' µkat/l.}
#' \item{B_MPV_E}{`numeric`, mean platelet volume in fl.}
#' \item{B_PLT_E}{`numeric`, platelet cell count in exp 9/l.}
#' \item{B_WBC_E}{`numeric`, white blood cell count in exp 9/l.}
#' \item{BILI_S}{`numeric`, total bilirubin in µmol/l.}
#' \item{BILID_S}{`numeric`, direct bilirubin in µmol/l.}
#' \item{CA_S}{`numeric`, calcium in mmol/l.}
#' \item{CHE_S}{`numeric`, cholinesterase in µkat/l.}
#' \item{CHOLF_Q}{`numeric`, TODO.}
#' \item{CHOLF_S}{`numeric`, TODO.}
#' \item{CHOLG_S}{`numeric`, total cholesterine in mmol/l.}
#' \item{CL_S}{`numeric`, chloride in mmol/l.}
#' \item{CRE_S}{`numeric`, creatinine in µmol/l.}
#' \item{CRP_S}{`numeric`, C-reactive protein in mg/l.}
#' \item{CYSC_S}{`numeric`, cystatin C in mg/l.}
#' \item{GGT_S}{`numeric`, gamma-glutamyltransferase in µkat/l.}
#' \item{IL6_S}{`numeric`, interleukin 6 in pg/ml.}
#' \item{INR_C}{`numeric`, international normalized ratio.}
#' \item{NA_S}{`numeric`, sodium in mmol/l.}
#' \item{P_S}{`numeric`, phosphate in mmol/l.}
#' \item{PALB_S}{`numeric`, pre-albumin in g/l.}
#' \item{PROT_S}{`numeric`, total protein in g/l.}
#' \item{PTH_S}{`numeric`, parathyreoid hormone in pmol/l.}
#' \item{VDT_OH_S}{`numeric`, vitamin D 25-OH in ng/l.}
#' \item{`attr(eldd, "excluded")`}{`integer(3)`, number of patients that
#' were excluded because they were younger than 18 years, already had a
#' liver transplantation before or their follow-up information was
#' missing/invalid.}
#' \item{`attr(eldd, "period")`}{`POSIXct(2)`, date and times of sampling period
#' (first and last entry).}
#' }
#' @source
#' Institute of Laboratory Medicine,
#' Clinical Chemistry and Molecular Diagnostics,
#' University Hospital Leipzig,
#' Paul-List-Str. 13-15, D-04103 Leipzig, Germany.
"eldd"

#' End-stage Liver Disease Reference
#'
#' A dataset of reference limits of the University Hospital Leipzig for
#' laboratory measurements used in [`eldd`].
#'
#' @format A `data.frame` with 36 rows and 8 columns. Each row describes a
#' laboratory measurement/method.
#' \describe{
#' \item{Code}{`character`, name.}
#' \item{Unit}{`character`, unit.}
#' \item{LongDescription}{`character`, full name.}
#' \item{ShortDescription}{`character`, abbreviation.}
#' \item{LowerLimit}{`numeric`, lower limit.}
#' \item{UpperLimit}{`numeric`, upper limit.}
#' \item{AgeDays}{`integer`, the method is only valid for patients older than
#' `AgeDays` days.}
#' \item{Sex}{`factor`, the method is only valid for "male" or "female"
#' patients. Or the sex doesn't matter ("both").}
#' }
#' @source
#' Institute of Laboratory Medicine,
#' Clinical Chemistry and Molecular Diagnostics,
#' University Hospital Leipzig,
#' Paul-List-Str. 13-15, D-04103 Leipzig, Germany.
"eldr"
