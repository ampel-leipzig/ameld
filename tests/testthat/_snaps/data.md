# eldd

    Code
      head(eldd)
    Output
        Age    Sex DaysAtRisk Deceased LTx Cirrhosis ALF Ethyltoxic HBV HCV AIH PBC
      1  68   male        200        0   0         1   0          1   1   0   0   0
      2  64   male          3        1   0         1   0          1   0   0   0   0
      3  67 female        208        0   0         1   0          1   0   0   0   0
      4  32 female         17        1   0         0   1          0   0   0   0   0
      5  64 female        189        0   0         1   0          1   0   0   0   0
      6  79   male        674        0   0         1   0          1   0   0   0   0
        PSC NASH Cryptogenic Dialysis GIB HCC SBP ALAT_S ALB_S  AP_S ASAT_S B_MPV_E
      1   0    0           0        0   0   1   0   0.29  40.9  1.17   0.56    11.0
      2   0    0           0        0   1   0   0   0.22  28.3  3.68   0.66      NA
      3   0    0           0        0   0   0   0   0.19  41.7  1.50   0.68    10.8
      4   0    0           0        0   0   0   0   0.78  23.8 52.97   3.24      NA
      5   0    0           0        0   1   0   0   0.75  36.3  2.79   1.33    13.9
      6   0    0           0        0   0   1   0   0.57  43.2  2.88   1.02    11.1
        B_PLT_E B_WBC_E BILI_S BILID_S CA_S CHE_S CHOLG_S  CL_S CRE_S CRP_S CYSC_S
      1     160     7.4    7.9     3.5 2.17  74.6    3.08 100.3   104  8.20   1.79
      2      10     8.1   43.2    26.0 2.04  14.7    2.21 101.9   304 43.54   4.87
      3     123     4.9   16.9     6.9 2.33  59.5    5.02  93.8    95  9.88   2.23
      4      NA      NA  266.5   208.3 2.23  15.4    4.55  95.7    61 90.29   4.73
      5      65     6.3   37.2    16.9 2.28  63.8    4.78 100.8    73  8.65   1.51
      6     109     6.2   11.3     6.5 2.15  39.6    3.12 101.6   127  5.83   1.60
        GGT_S  IL6_S INR_C  NA_S  P_S PALB_S PROT_S PTH_S VDT_OH_S
      1  1.97  22.87  1.11 135.4 1.49   0.19   69.6  2.39     12.7
      2  2.43 336.50  1.77 133.1 0.96   0.05   62.5 19.39      5.5
      3  1.84  16.74  1.09 137.4 1.14   0.17   80.5  7.39     18.8
      4 24.35 709.80  2.29 130.5 1.71   0.15   48.9  2.21      4.5
      5  2.45   7.90  1.10 142.6 1.07   0.11   67.6  4.17     34.1
      6  3.24  12.57  1.09 137.0 0.66   0.22   69.4 15.10     17.5

---

    Code
      tail(eldd)
    Output
          Age    Sex DaysAtRisk Deceased LTx Cirrhosis ALF Ethyltoxic HBV HCV AIH PBC
      649  55   male        235        0   0         1   0          1   0   0   0   0
      650  64   male         43        1   0         0   1          0   0   0   0   0
      651  56 female         41        0   1         1   0          1   0   0   0   0
      652  24 female         20        0   0         1   0          0   1   0   0   0
      653  46   male        139        0   0         1   0          1   0   0   0   0
      654  47 female          2        0   1         1   0          1   0   0   0   0
          PSC NASH Cryptogenic Dialysis GIB HCC SBP ALAT_S ALB_S AP_S ASAT_S B_MPV_E
      649   0    0           0        0   1   0   0   0.34  27.7 1.77   0.85    10.6
      650   0    0           0        1   0   0   0   0.73  28.0 9.99   1.85    10.8
      651   0    0           0        0   0   0   0   0.73  26.9 3.29   2.06     9.8
      652   0    0           0        0   0   0   1  11.31  28.3 2.27  32.69    12.6
      653   0    0           0        0   0   0   0   0.36  35.9 4.34   1.84    11.6
      654   0    0           0        0   1   0   1   0.49  31.1 4.30   1.11      NA
          B_PLT_E B_WBC_E BILI_S BILID_S CA_S CHE_S CHOLG_S  CL_S CRE_S CRP_S CYSC_S
      649     181     9.6   63.9    42.9 2.00  30.9    3.61 110.1   178 35.98   3.44
      650     274    12.0  544.8   487.6 1.83  38.0    8.32 102.3   137 37.85   3.40
      651     109     8.1  179.7   101.7 2.31  42.6    3.78  98.5   101  8.11   2.13
      652     186    12.8  240.9   170.0 1.97  65.7    3.50  96.4    44 12.33   1.03
      653     199    11.9  411.3   358.1 2.34  50.1    6.22 106.9   108 42.21   1.91
      654      56     2.8   42.4    24.5 2.19  41.0    3.15 103.4    77 22.20   1.64
          GGT_S  IL6_S INR_C  NA_S  P_S PALB_S PROT_S PTH_S VDT_OH_S
      649  1.29 198.70  1.66 135.6 1.64   0.07   75.8  4.62      6.5
      650 13.01 136.00  1.31 137.2 0.60   0.32   43.4 16.67      3.2
      651  0.65  31.36  2.31 128.2 1.40   0.06   82.6  3.73     30.6
      652  0.22  81.07  3.03 133.7 0.58   0.06   50.2  3.26     11.6
      653 10.99  19.54  2.02 139.5 1.12   0.11   68.9  4.43     13.0
      654  4.73  23.12  1.70 137.8 1.20   0.07   63.5  3.44      3.5

---

    Code
      summary(eldd)
    Output
            Age            Sex        DaysAtRisk        Deceased     
       Min.   :18.00   female:240   Min.   :   1.0   Min.   :0.0000  
       1st Qu.:52.00   male  :414   1st Qu.:  38.0   1st Qu.:0.0000  
       Median :58.00                Median : 191.0   Median :0.0000  
       Mean   :56.79                Mean   : 265.1   Mean   :0.1636  
       3rd Qu.:64.00                3rd Qu.: 383.8   3rd Qu.:0.0000  
       Max.   :81.00                Max.   :1080.0   Max.   :1.0000  
                                                                     
            LTx            Cirrhosis           ALF            Ethyltoxic    
       Min.   :0.00000   Min.   :0.0000   Min.   :0.00000   Min.   :0.0000  
       1st Qu.:0.00000   1st Qu.:1.0000   1st Qu.:0.00000   1st Qu.:0.0000  
       Median :0.00000   Median :1.0000   Median :0.00000   Median :1.0000  
       Mean   :0.09174   Mean   :0.9096   Mean   :0.01223   Mean   :0.6284  
       3rd Qu.:0.00000   3rd Qu.:1.0000   3rd Qu.:0.00000   3rd Qu.:1.0000  
       Max.   :1.00000   Max.   :1.0000   Max.   :1.00000   Max.   :1.0000  
                         NA's   :1                                          
            HBV               HCV               AIH              PBC         
       Min.   :0.00000   Min.   :0.00000   Min.   :0.0000   Min.   :0.00000  
       1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.00000  
       Median :0.00000   Median :0.00000   Median :0.0000   Median :0.00000  
       Mean   :0.03058   Mean   :0.06881   Mean   :0.0474   Mean   :0.02599  
       3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.0000   3rd Qu.:0.00000  
       Max.   :1.00000   Max.   :1.00000   Max.   :1.0000   Max.   :1.00000  
                                                                             
            PSC               NASH          Cryptogenic       Dialysis      
       Min.   :0.00000   Min.   :0.00000   Min.   :0.000   Min.   :0.00000  
       1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.000   1st Qu.:0.00000  
       Median :0.00000   Median :0.00000   Median :0.000   Median :0.00000  
       Mean   :0.02446   Mean   :0.07339   Mean   :0.104   Mean   :0.05061  
       3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.000   3rd Qu.:0.00000  
       Max.   :1.00000   Max.   :1.00000   Max.   :1.000   Max.   :1.00000  
                                                           NA's   :2        
            GIB              HCC             SBP             ALAT_S       
       Min.   :0.0000   Min.   :0.000   Min.   :0.0000   Min.   : 0.0800  
       1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.0000   1st Qu.: 0.2200  
       Median :0.0000   Median :0.000   Median :0.0000   Median : 0.3300  
       Mean   :0.2477   Mean   :0.185   Mean   :0.1394   Mean   : 0.6638  
       3rd Qu.:0.0000   3rd Qu.:0.000   3rd Qu.:0.0000   3rd Qu.: 0.4825  
       Max.   :1.0000   Max.   :1.000   Max.   :1.0000   Max.   :56.0300  
                                        NA's   :1        NA's   :10       
           ALB_S            AP_S            ASAT_S           B_MPV_E     
       Min.   :16.10   Min.   : 0.350   Min.   : 0.2700   Min.   : 8.40  
       1st Qu.:33.50   1st Qu.: 1.460   1st Qu.: 0.5525   1st Qu.:10.30  
       Median :39.25   Median : 1.950   Median : 0.7700   Median :11.00  
       Mean   :38.54   Mean   : 2.483   Mean   : 1.2324   Mean   :11.06  
       3rd Qu.:44.62   3rd Qu.: 2.825   3rd Qu.: 1.1600   3rd Qu.:11.80  
       Max.   :52.40   Max.   :52.970   Max.   :32.6900   Max.   :14.50  
       NA's   :6       NA's   :7        NA's   :8         NA's   :64     
          B_PLT_E          B_WBC_E           BILI_S           BILID_S      
       Min.   : 10.00   Min.   : 1.500   Min.   :   2.20   Min.   :  3.00  
       1st Qu.: 72.75   1st Qu.: 4.650   1st Qu.:  12.80   1st Qu.:  6.20  
       Median :118.00   Median : 6.300   Median :  24.45   Median : 12.55  
       Mean   :136.51   Mean   : 7.117   Mean   :  59.30   Mean   : 40.59  
       3rd Qu.:183.00   3rd Qu.: 8.300   3rd Qu.:  50.38   3rd Qu.: 30.82  
       Max.   :742.00   Max.   :30.200   Max.   :1096.80   Max.   :497.90  
       NA's   :26       NA's   :23                         NA's   :82      
            CA_S           CHE_S           CHOLG_S            CL_S      
       Min.   :1.730   Min.   :  4.50   Min.   : 0.880   Min.   : 61.7  
       1st Qu.:2.160   1st Qu.: 39.70   1st Qu.: 3.340   1st Qu.: 96.5  
       Median :2.300   Median : 67.65   Median : 4.405   Median : 99.9  
       Mean   :2.275   Mean   : 72.74   Mean   : 4.412   Mean   : 99.2  
       3rd Qu.:2.400   3rd Qu.:102.55   3rd Qu.: 5.310   3rd Qu.:102.9  
       Max.   :2.960   Max.   :191.90   Max.   :20.530   Max.   :118.5  
       NA's   :1       NA's   :6                         NA's   :1      
           CRE_S            CRP_S            CYSC_S          GGT_S        
       Min.   : 26.00   Min.   :  0.30   Min.   :0.610   Min.   : 0.1500  
       1st Qu.: 65.00   1st Qu.:  2.19   1st Qu.:1.070   1st Qu.: 0.8475  
       Median : 80.00   Median :  5.80   Median :1.320   Median : 1.6400  
       Mean   : 99.07   Mean   : 15.93   Mean   :1.581   Mean   : 2.8735  
       3rd Qu.:104.00   3rd Qu.: 16.73   3rd Qu.:1.790   3rd Qu.: 3.1550  
       Max.   :604.00   Max.   :210.07   Max.   :6.720   Max.   :51.0700  
                        NA's   :14       NA's   :7       NA's   :6        
           IL6_S              INR_C            NA_S            P_S       
       Min.   :    1.50   Min.   :0.840   Min.   :100.8   Min.   :0.170  
       1st Qu.:    5.66   1st Qu.:1.090   1st Qu.:135.2   1st Qu.:0.880  
       Median :   12.53   Median :1.210   Median :138.1   Median :1.030  
       Mean   :  228.80   Mean   :1.399   Mean   :137.3   Mean   :1.047  
       3rd Qu.:   41.83   3rd Qu.:1.455   3rd Qu.:140.4   3rd Qu.:1.170  
       Max.   :39474.00   Max.   :6.660   Max.   :157.9   Max.   :2.940  
       NA's   :56         NA's   :2       NA's   :9       NA's   :1      
           PALB_S           PROT_S          PTH_S            VDT_OH_S     
       Min.   :0.0300   Min.   :29.10   Min.   :  0.480   Min.   :  3.20  
       1st Qu.:0.0800   1st Qu.:64.80   1st Qu.:  2.840   1st Qu.: 11.15  
       Median :0.1200   Median :70.95   Median :  3.885   Median : 19.30  
       Mean   :0.1433   Mean   :69.45   Mean   :  5.490   Mean   : 22.69  
       3rd Qu.:0.1900   3rd Qu.:76.10   3rd Qu.:  5.447   3rd Qu.: 30.40  
       Max.   :0.6800   Max.   :91.50   Max.   :180.500   Max.   :111.80  
       NA's   :5        NA's   :6       NA's   :8         NA's   :47      

# eldr

    Code
      head(eldr)
    Output
          Code   Unit            LongDescription ShortDescription LowerLimit
      1 ALAT_S µkat/l   alanine aminotransferase             ALAT       0.17
      2 ALAT_S µkat/l   alanine aminotransferase             ALAT       0.17
      3  ALB_S    g/l                    albumin              Alb      35.00
      4   AP_S µkat/l       alkaline phosphatase               AP       0.67
      5   AP_S µkat/l       alkaline phosphatase               AP       0.58
      6 ASAT_S µkat/l aspartate aminotransferase             ASAT       0.17
        UpperLimit AgeDays    Sex
      1       0.85    6574   male
      2       0.58    6574 female
      3      52.00    6574   both
      4       2.15    6574   male
      5       1.74    6574 female
      6       0.85    6574   male

---

    Code
      tail(eldr)
    Output
             Code   Unit      LongDescription ShortDescription LowerLimit UpperLimit
      31      P_S mmol/l            phosphate                P       0.84       1.45
      32   PALB_S    g/l          pre-albumin             PAlb       0.20       0.40
      33   PROT_S    g/l        total protein             Prot      64.00      83.00
      34    PTH_S pmol/l parathyreoid hormone              PTH       1.60       6.90
      35 VDT_OH_S  ng/ml      vitamin D 25-OH              VDT       4.90      42.70
      36 VDT_OH_S  ng/ml      vitamin D 25-OH              VDT       6.20      49.90
         AgeDays    Sex
      31    6574   both
      32       0   both
      33    6574   both
      34       0   both
      35    7305   male
      36    7305 female

