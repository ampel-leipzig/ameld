library("readxl")
library("lubridate")

devtools::load_all()

eldd <- as.data.frame(read_xlsx("190820_Abfrage_alleZeitpunkte.xlsx"))

# old names <-> new names, missing names are dropped
map <- read.csv("map-columns.csv", stringsAsFactors = FALSE)
keep <- as.logical(nchar(map$to))
eldd <- eldd[map$from[keep]]
names(eldd) <- map$to[keep]

eldd$Age <- age(make_date(eldd$YearOfBirth), eldd$DateTime)
eldd$Deceased <- ifelse(eldd$Deceased & is.na(eldd$LTxDate), 1L, 0L)
eldd$DaysAtRisk <- daysAtRisk(
    eldd$DateTime, eldd$FollowUpMaxDate, eldd$LTxDate, eldd$ReLTxDate
)

## we exclude this column later because many dates are invalid (before 1900,
## maybe a Excel conversion problem, original dates could not be reproduced
## anymore)
eldd$DaysSinceCirrhosis <- daysAtRisk(eldd$CirrhosisDate, eldd$DateTime)
## if the first diagnosis of cirrhosis was after the visit where the laboratory
## measurements are taken, set cirrhosis to zero.
hadCirrhosisAfterVisit <- !is.na(eldd$DaysSinceCirrhosis) &
    eldd$DaysSinceCirrhosis < 0
eldd$Cirrhosis[hadCirrhosisAfterVisit] <- 0L
eldd$DaysSinceCirrhosis[eldd$Cirrhosis == 0] <- NA

## we exclude this column later because many dates are invalid (before 1900,
## maybe a Excel conversion problem, original dates could not be reproduced
## anymore)
eldd$DaysSinceHCC <- daysAtRisk(eldd$HCCDate, eldd$DateTime)

eldd$Dialysis <- ifelse(eldd$Dialysis == "ja", 1L, 0L)

periodAttr <- range(eldd$DateTime)

## keep only last visit
eldd <- eldd[order(eldd$PatientId, eldd$DateTime, decreasing = TRUE),]
eldd <- eldd[!duplicated(eldd$PatientId),]
eldd <- eldd[order(eldd$PatientId, eldd$DateTime),]

## exclude children
exclude <- eldd$Age < 18
excludeAttr <- list(Children = sum(exclude))
eldd <- eldd[!exclude, ]

## exclude patients which already had a liver transplantation before
exclude <- !is.na(eldd$LTxDate) & eldd$LTxDate < eldd$DateTime
excludeAttr$LiverTransplantation <- sum(exclude)
eldd <- eldd[!exclude, ]

## exclude patients with missing or invalid follow up dates
exclude <- is.na(eldd$DaysAtRisk) | eldd$DaysAtRisk < 1L
excludeAttr$LostFollowUp <- sum(exclude)
eldd <- eldd[!exclude, ]

## remove identification data
eldd <- eldd[
    !names(eldd) %in%
        c(
          "PatientId", "CaseId", "YearOfBirth",
          "OrderId", "LabOrderId",
          "DateTime", "FollowUpMaxDate",
          "CirrhosisDate"
         )
]
rownames(eldd) <- NULL

## remove columns with more missing data than cases
keep <- colMeans(is.na(eldd)) < mean(eldd$Deceased) # 0.16

names(eldd)[!keep]
#  [1] "LTxDate"              "ReLTxDate"            "DeceasedAfterLTxDate"
#  [4] "HCCDate"              "HCCInfo"              "BILIN_S"
#  [7] "K_S"                  "AFP_S"                "HBA1C_E"
# [10] "FE_S"                 "FERR_S"               "LDH_S"
# [13] "LDLC_S"               "HDLC_S"               "DaysSinceHCC"
eldd <- eldd[keep]

## exclude CHOL_S because it is the same as CHOLG_S
eldd$CHOL_S <- NULL

## exclude research laboratory measurement "free cholesterine" (because there
## are no reference measurements available); CHOLF_Q was the quotient of
## CHOLF_S to CHOLG_S
eldd$CHOLF_S <- eldd$CHOLF_Q <- NULL

## reorder columns
eldd <- eldd[match(
    c(
        "Age", "Sex", "DaysAtRisk", "Deceased", "LTx",
        ## entity
        "Cirrhosis", "ALF",
        ## etiology
        "Ethyltoxic", "HBV", "HCV", "AIH", "PBC", "PSC",
        "NASH", "Cryptogenic",
        ## complications
        "Dialysis", "GIB", "HCC", "SBP",
        ## laboratory measurements
        sort(names(eldd)[grep("_.$", names(eldd))])
    ),
    names(eldd)
)]

## reclass columns
eldd$Sex <- factor(
    as.integer(eldd$Sex == "M"), levels = c(0, 1), labels = c("female", "male")
)
toInt <- names(eldd) %in% c(
    "LTx", "Cirrhosis", "ALF", "Ethyltoxic", "HBV", "HBC", "AIH", "PBC", "PSC",
    "NASH", "Cryptogenic", "GIB", "HCC", "SBP"
)
eldd[toInt] <- lapply(eldd[toInt], as.integer)

attr(eldd, "excluded") <- unlist(excludeAttr)
attr(eldd, "period") <- periodAttr
save(eldd, file = file.path("..", "data", "eldd.rda"), compress = "xz")
