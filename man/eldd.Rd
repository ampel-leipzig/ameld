% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{eldd}
\alias{eldd}
\title{End-stage Liver Disease Data}
\format{
A \code{data.frame} with 654 rows and 48 columns:
\describe{
\item{Age}{\code{integer}, patient's age in years.}
\item{Sex}{\code{factor}, patient's sex encoded as \code{"M"} for male and \code{"W"} for
female.}
\item{DaysAtRisk}{\code{integer}, number of days from the time when the
laboratory measurements were taken to the last follow up.}
\item{Deceased}{\code{integer}, was the patient alive (\code{0}) or deceased (\code{1})
on the last follow up.}
\item{LTx}{\code{integer}, \code{1} if the patient got a liver transplantation else
\code{0}.}
\item{Cirrhosis}{\code{integer}, \code{1} if the patient had a cirrhosis else \code{0}.}
\item{ALF}{\code{integer}, \code{1} if the patient had acute liver failure else \code{0}.}
\item{Ethyltoxic}{\code{integer}, \code{1} if the patient had a ethyltoxic liver
disease else \code{0}.}
\item{HBV}{\code{integer}, \code{1} if the patient had a liver
disease caused by an infection with the hepatitis B virus else \code{0}.}
\item{HCV}{\code{integer}, \code{1} if the patient had a liver
disease caused by an infection with the hepatitis C virus else \code{0}.}
\item{AIH}{\code{integer}, \code{1} if the patient had an autoimmune hepatitis else
\code{0}.}
\item{PBC}{\code{integer}, \code{1} if the patient had a primary sclerosing cirrhosis
else \code{0}.}
\item{PSC}{\code{integer}, \code{1} if the patient had a primary biliary cirrhosis else
\code{0}.}
\item{NASH}{\code{integer}, \code{1} if the patient had a non-alcoholic steatohepatitis
else \code{0}.}
\item{Cryptogenic}{\code{integer}, \code{1} if the patient had cirrhosis of unknown
origin else \code{0}.}
\item{Dialysis}{\code{integer}, \code{1} if the patient required renal replacement
therapy else \code{0}.}
\item{GIB}{\code{integer}, \code{1} if the patient suffered gastrointestinal bleedings
else \code{0}.}
\item{SBP}{\code{integer}, \code{1} if the patient suffered spontaneous bacterial
peritonitis else \code{0}.}
\item{HCC}{\code{integer}, \code{1} if the patient suffered hepatocellular carcinoma
else \code{0}.}
\item{ALAT_S}{\code{numeric}, serum level of ALAT, alanine aminotransferase in
µkat/l.}
\item{ALB_S}{\code{numeric}, serum level of albumin in g/l.}
\item{AP_S}{\code{numeric}, serum level of alkaline phosphatase in µkat/l.}
\item{ASAT_S}{\code{numeric}, serum level of ASAT, aspartate aminotransferase in
µkat/l.}
\item{B_MPV_E}{\code{numeric}, mean platelet volume in fl.}
\item{B_PLT_E}{\code{numeric}, platelet cell count in exp 9/l.}
\item{B_WBC_E}{\code{numeric}, white blood cell count in exp 9/l.}
\item{BILI_S}{\code{numeric}, total bilirubin in µmol/l.}
\item{BILID_S}{\code{numeric}, direct bilirubin in µmol/l.}
\item{CA_S}{\code{numeric}, calcium in mmol/l.}
\item{CHE_S}{\code{numeric}, cholinesterase in µkat/l.}
\item{CHOLF_Q}{\code{numeric}, TODO.}
\item{CHOLF_S}{\code{numeric}, TODO.}
\item{CHOLG_S}{\code{numeric}, total cholesterine in mmol/l.}
\item{CL_S}{\code{numeric}, chloride in mmol/l.}
\item{CRE_S}{\code{numeric}, creatinine in µmol/l.}
\item{CRP_S}{\code{numeric}, C-reactive protein in mg/l.}
\item{CYSC_S}{\code{numeric}, cystatin C in mg/l.}
\item{GGT_S}{\code{numeric}, gamma-glutamyltransferase in µkat/l.}
\item{IL6_S}{\code{numeric}, interleukin 6 in pg/ml.}
\item{INR_C}{\code{numeric}, international normalized ratio.}
\item{NA_S}{\code{numeric}, sodium in mmol/l.}
\item{P_S}{\code{numeric}, phosphate in mmol/l.}
\item{PALB_S}{\code{numeric}, pre-albumin in g/l.}
\item{PROT_S}{\code{numeric}, total protein in g/l.}
\item{PTH_S}{\code{numeric}, parathyreoid hormone in pmol/l.}
\item{VDT_OH_S}{\code{numeric}, vitamin D 25-OH in ng/l.}
\item{\code{attr(eldd, "excluded")}}{\code{integer(3)}, number of patients that
were excluded because they were younger than 18 years, already had a
liver transplantation before or their follow-up information was
missing/invalid.}
\item{\code{attr(eldd, "period")}}{\code{POSIXct(2)}, date and times of sampling period
(first and last entry).}
}
}
\source{
Institute of Laboratory Medicine,
Clinical Chemistry and Molecular Diagnostics,
University Hospital Leipzig,
Paul-List-Str. 13-15, D-04103 Leipzig, Germany.
}
\usage{
eldd
}
\description{
A dataset of patients evaluated for liver transplantation at the University
Hospital of Leipzig from November 2012 to June 2015.
}
\details{
The Ethics Committee at the Leipzig University Faculty of
Medicine approved the retrospective usage of the data for our study
(reference number: 039/14ff).
}
\keyword{datasets}
