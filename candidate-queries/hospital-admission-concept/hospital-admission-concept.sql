-- MAJOR CHANGES:
-- DAOH90/180 are caulcated
-- Days alive and d7/30/90/365 are now counted from ICU admission (not PAR admission as previously) to be more cohorent w.r.t. DAOH measures
-- Add ICU admission details


-- PAR_HADM is a redefined PAR table adding a unique HADM_ID to each admission
-- Keep only admissions where the patient was >= 18 yrs at admission
-- Keep only admissions after 2010/01/01
WITH PAR_HADM AS (
    SELECT *,
           ROW_NUMBER() OVER ( 
               ORDER BY LopNr,
                        INDATUM,
                        UTDATUM,
                        CASE WHEN SJUKHUS NOT IN ('11001', '11003', '51001', '12001', '21001', '64001', '41001', '41002') THEN 0 ELSE 1 END
           ) AS HADM_ID
    FROM PAR
    WHERE 
        ALDER >= 18
        AND INDATUM >= 14610 -- This is 2010/01/01
)
,

PAR_HADM_W_ADM_FLAG AS (
    SELECT
        *,
        INDATUM - LAG(UTDATUM, 1) OVER (PARTITION BY LopNr ORDER BY INDATUM) AS DAYS_SINCE_LAST_ADMISSION
    FROM PAR_HADM
),

PAR_HADM_CONT AS (
    SELECT
        *,
        CASE
            WHEN DAYS_SINCE_LAST_ADMISSION IS NULL OR DAYS_SINCE_LAST_ADMISSION > 1 THEN 1
            ELSE 0
        END AS ADMISSION_FLAG,
        SUM(CASE WHEN DAYS_SINCE_LAST_ADMISSION IS NULL OR DAYS_SINCE_LAST_ADMISSION > 1 THEN 1 ELSE 0 END) 
        OVER (PARTITION BY LopNr ROWS UNBOUNDED PRECEDING) AS SUM_FLAG,
        SUM(CASE WHEN DAYS_SINCE_LAST_ADMISSION IS NULL OR DAYS_SINCE_LAST_ADMISSION > 1 THEN 1 ELSE 0 END) 
        OVER (PARTITION BY LopNr ROWS UNBOUNDED PRECEDING) + LopNr * 1000 AS CONT_HADM_ID
    FROM PAR_HADM_W_ADM_FLAG
),

PAR_HADM_CONT_DATES AS (
    SELECT
        *,
        MIN(INDATUM) OVER (PARTITION BY CONT_HADM_ID) AS CONT_HADM_ADM_DATE,
        MAX(UTDATUM) OVER (PARTITION BY CONT_HADM_ID) AS CONT_HADM_DSC_DATE
    FROM PAR_HADM_CONT
),

-- T_ICU_ADMISSIONS has some basic information about all 
-- ICU admissions to a tertiary ICU

T_ICU_ADMISSIONS AS (
    SELECT
        S.VtfId_LopNr,
        S.LopNr,
        S.InskrTidPunkt,
        S.UtskrTidPunkt,
        S.AvdNamn
    FROM SIR_BASDATA S
    WHERE S.AvdNamn IN (
        'S-CIVA',
        'S-NIVA',
        'KS/THIVA',
        'KS ECMO',
        'Astrid Lindgren',
        'IVA Lund',
        'Lund - BIVA',
        'Lund - NIVA',
        'Linköping',
        'Linköping NIVA',
        'Linköping BRIVA',
        'SU/NIVA',
        'SU/CIVA',
        'SU/TIVA',
        'Umeå IVA',
        'Umeå - Thorax',
        'Uppsala',
        'Uppsala BRIVA',
        'Uppsala TIVA',
        'Uppsala BIVA',
        'Uppsala NIVA'
        )
),

-- In T_ICU_ADMISSIONS_MATCHED_WITH_PAR all ICU admissions in 
-- T_ICU_ADMISSIONS are matched (by left join) with PAR admissions in PAR_HADM_CONT_DATES fulfilling the criteria:
-- PAR admission at T
-- PAR admission starting from 14 days prior to ICU admission up to 14 days after ICU admission
-- If no PAR admission matching the SIR admission the latter will drop out
-- If multiple PAR admissions fulfill matching criteria, multiple rows will be returned for that SIR admission
T_ICU_ADMISSIONS_MATCHED_WITH_PAR AS (
    SELECT 
        T.VtfId_LopNr,
        P.HADM_ID,
        T.LopNr,
        T.InskrTidPunkt,
        T.UtskrTidPunkt,
        T.AvdNamn,
        P.INDATUM,
        P.UTDATUM,
        P.MVO,
        P.SJUKHUS
    FROM T_ICU_ADMISSIONS T
    LEFT JOIN PAR_HADM_CONT_DATES P ON T.LopNr == P.LopNr
    WHERE P.INDATUM - T.InskrTidPunkt / 86400 IN (-14, -13, -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
    AND (
        (P.Sjukhus IN ('11001', '11003') AND T.AvdNamn IN ('S-CIVA', 'S-NIVA', 'KS/THIVA', 'KS ECMO', 'Astrid Lindgren'))
        OR
        (P.Sjukhus = '51001' AND T.AvdNamn IN ('SU/NIVA', 'SU/CIVA', 'SU/TIVA'))
        OR
        (P.Sjukhus = '12001' AND T.AvdNamn IN ('Uppsala', 'Uppsala BRIVA', 'Uppsala TIVA', 'Uppsala BIVA', 'Uppsala NIVA'))
        OR
        (P.Sjukhus = '21001' AND T.AvdNamn IN ('Linköping', 'Linköping NIVA', 'Linköping BRIVA'))
        OR
        (P.Sjukhus = '64001' AND T.AvdNamn IN ('Umeå IVA', 'Umeå - Thorax'))
        OR
        (P.Sjukhus IN ('41001', '41002') AND T.AvdNamn IN ('IVA Lund', 'Lund - BIVA', 'Lund - NIVA'))
    )
),

-- A set of diagnoses will be filtered on specific criteria
-- aSAH
asah AS (
    SELECT
        P.HADM_ID,
        P.LopNr,
        P.Alder
    FROM
        PAR_HADM_CONT_DATES P
    LEFT JOIN
        DORS D ON P.LopNr = D.LopNr
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
          AND (
            (
                (P.Diagnos LIKE "I60%"
                OR P.Diagnos LIKE "I671%"
                OR P.Op LIKE "%AAC00%" OR P.Op LIKE "%AAL00%")  -- I671 is included: If you are sick enough to get admitted to an ICU, it is still a relevant dx
                AND P.Diagnos NOT LIKE "%S06%")

          OR (P.Diagnos LIKE "I60%" AND P.Diagnos LIKE "%S06%" AND (P.Op LIKE "%AAC00%" OR P.Op LIKE "%AAL00%"))
           --     AND P.Diagnos NOT LIKE "%Q28%" There are tens of cases where Q28 is a 2nd dx, I60 a first and patient coiled, 
            --    AND P.Diagnos NOT LIKE "I671%" Hey, if you are sick enough to get admitted to an ICU, it is still an emergency
            --)
          OR
            (
                (D.Ulorsak LIKE "I60%")
                AND JULIANDAY(strftime('%Y-%m-%d', substr(D.DODSDAT, 1, 4) || '-' || substr(D.DODSDAT, 5, 2) || '-' || substr(D.DODSDAT, 7, 2) || ' 00:00:00')) - JULIANDAY(date(P.INDATUM * 86400, 'unixepoch')) <= 30
                AND P.Diagnos NOT LIKE "%S06%"
                AND P.Diagnos NOT LIKE "%Q28%"
                AND P.Diagnos NOT LIKE "I671%"
            )
        )
),

-- TBI
tbi AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM_CONT_DATES P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
        AND 
            ((P.Diagnos LIKE "S06%") OR
            ((P.Diagnos LIKE "S020%" OR
             P.Diagnos LIKE "S021%" OR
             P.Diagnos LIKE "S028%" OR
             P.Diagnos LIKE "S029%" OR
             P.Diagnos LIKE "S071%" OR
             P.Diagnos LIKE "S04%" OR
             P.Diagnos LIKE "S12%") AND (P.Diagnos LIKE "%S06%")))
)
,

-- Cerebral venous thrombosis
cvt AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM_CONT_DATES P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
        AND (P.Diagnos LIKE "G08%"
        OR P.Diagnos LIKE "I676%"
        OR P.Diagnos LIKE "I636%"
        OR P.Diagnos LIKE "O225%"
        OR P.Diagnos LIKE "O873%")
        -- Some aSAH that fulfill the above criteria will need to be filtered out as such:
        AND (P.Op NOT LIKE "%AAC00%")
        AND (P.Op NOT LIKE "%AAL00%")
        AND (P.Diagnos NOT LIKE "%I60%")

)
,

-- ICH
ich AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM_CONT_DATES P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
        AND (P.Diagnos LIKE "I61%")
        -- A few likely aSAH that fulfill the above criteria will need to be filtered out as such:
        AND (P.Op NOT LIKE "%AAC00%")
        AND (P.Op NOT LIKE "%AAL00%")
),

-- AVM
avm AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM_CONT_DATES P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
        AND (P.Diagnos LIKE "Q28%")
        AND (P.Op NOT LIKE "%AAL00%")
        AND (P.Op NOT LIKE "%AAC00%")
),

-- Acute ischemic stroke
ais AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM_CONT_DATES P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
        AND (P.Diagnos LIKE "I63%")
        AND (P.Diagnos NOT LIKE "I636%")
        -- a few likely aSAH that fulfill the above criteria will need to be filtered out as such:
        AND (P.Op NOT LIKE "%AAC00%")
        AND (P.Op NOT LIKE "%AAL00%")
),

-- Acute bacterial meningitis
abm AS (
    SELECT
        P.HADM_ID,
        P.LopNr
        FROM
        PAR_HADM_CONT_DATES P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
        AND (
            P.Diagnos LIKE "G00%" OR
            P.Diagnos LIKE "A390%" OR
            -- Also include intracranial abcess and "abscess i skalle eller ryggradskanal"
            P.Diagnos LIKE "G06%" OR
            P.Diagnos LIKE "G039%" -- also include Meningitis uns, again, if they are sick enough to be in the icu...
        )
),

-- Encephalitis
ence AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM_CONT_DATES P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
        AND (
            P.Diagnos LIKE "G04%" OR
            P.Diagnos LIKE "G05%" OR
            P.Diagnos LIKE "B004%" OR
            P.Diagnos LIKE "B020%" OR
            P.Diagnos LIKE "A841%"
        )
),

-- Status epilepticus
se AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM_CONT_DATES P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
        AND (
            P.Diagnos LIKE "G41%" OR
            -- also include epilepsy
            P.Diagnos LIKE "G40%"
        )
),

-- "Isolated" cervical spine frx
cfx AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM_CONT_DATES P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
        AND (
            P.Diagnos LIKE 'S12%' OR
            P.Diagnos LIKE 'S13%' OR
            P.Diagnos LIKE 'S14%'
        )
        AND P.Diagnos NOT LIKE '%S06%'
),

-- Turns out there are loads of I62 (non traumatic SDH), many get evacuated, some have a traumatic sdh as secondary dx... let's pick the "clean"
-- Most get AD005 (evac chron sdh) or AD010 (evac acute sdh). Not sure it's reasonable to "split" on that.
sdh AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM_CONT_DATES P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
        AND (
            P.Diagnos LIKE 'I62%'
        )
        AND P.Diagnos NOT LIKE '%S06%'
        --- exclude a few asah
        AND P.Op NOT LIKE "%AAL00%"
        AND P.Op NOT LIKE "%AAC00%"
),

-- "Isolated" hydrocephalus (shunt dysfunctions et al)
hc AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM_CONT_DATES P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
        AND (
            P.Diagnos LIKE 'G91%'
        )
    -- several  likely aSAH that fulfill the above criteria will need to be filtered out as such:
    AND (P.Op NOT LIKE "%AAC00%")
    AND (P.Op NOT LIKE "%AAL00%")
),

--  tumors
tum AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM_CONT_DATES P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
        AND (
            P.Diagnos LIKE 'D43%'
            OR P.Diagnos LIKE 'C71%'
            OR P.Diagnos LIKE 'C793%'
            OR P.Diagnos LIKE 'D33%'
        )
        --- exclude a few asah
        AND P.Op NOT LIKE "%AAL00%"
        AND P.Op NOT LIKE "%AAC00%"
),

-- DX finds the diagnostic group for each entry in PAR_HADM_CONT_DATES. Note that one HADM
-- can have several diagnostic criteria fulfilled (although it is rare), therefore
-- there can be several rows for same admission
DX AS (
    SELECT
        P.HADM_ID,
        P.LopNr,
        P.Diagnos,
        CASE
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM asah) THEN 'ASAH'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM ich) THEN 'ICH'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM tbi) THEN 'TBI'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM ais) THEN 'AIS'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM abm) THEN 'ABM'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM cvt) THEN 'CVT'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM ence) THEN 'ENC'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM se) THEN 'SEP'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM avm) THEN 'AVM'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM cfx) THEN 'CFX'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM sdh) THEN 'SDH'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM hc) THEN 'HC'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM tum) THEN 'TUM'
            ELSE 'OTHER'
        END AS DX_GROUP
    FROM
        PAR_HADM_CONT_DATES P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
),

-- Create a window where the SIR-PAR matched cohort is joined (on
-- PAR admission ID) with the diagnostic group window (based on PAR dx)
T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX AS (
    SELECT
        T.VtfId_LopNr,
        T.HADM_ID,
        T.LopNr,
        T.InskrTidPunkt,
        T.UtskrTidPunkt,
        T.AvdNamn,
        T.INDATUM,
        T.UTDATUM,
        T.MVO,
        T.SJUKHUS,
        P.DIAGNOS,
        P.OP,
        D.DX_GROUP
    FROM T_ICU_ADMISSIONS_MATCHED_WITH_PAR T
    LEFT JOIN DX D ON T.HADM_ID = D.HADM_ID
    LEFT JOIN PAR_HADM_CONT_DATES P ON T.HADM_ID = P.HADM_ID
)
,

-- T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_TIME_HIERARCHY helps resolving tie situations
-- where one SIR admission is associated with several PAR admissions.
-- In this case the earliest (within the time window) admissions is chosen,
-- if there still is a tie a hierarchical ordering of diagnosis will choose one admission only
T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_TIME_HIERARCHY AS (
    SELECT *,
           ROW_NUMBER() OVER (
               PARTITION BY VtfId_LopNr 
               ORDER BY INDATUM, 
                        CASE DX_GROUP
                            WHEN 'TBI' THEN 1
                            WHEN 'ASAH' THEN 2
                            WHEN 'AIS' THEN 3
                            WHEN 'ICH' THEN 4
                            WHEN 'ABM' THEN 5
                            WHEN 'CFX' THEN 6
                            WHEN 'ENC' THEN 7
                            WHEN 'TUM' THEN 8
                            WHEN 'SEP' THEN 9
                            WHEN 'HC' THEN 10
                            ELSE 11 -- for any other value not specified
                        END
           ) AS DX_ORDER
   FROM T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX
   WHERE DX_GROUP != "OTHER"
)
,

-- T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME helps resolving tie situations
-- where one SIR admission is associated with several PAR admissions.
-- In this case the hierarchical ordering of diagnoses will decide which admission is picked,
-- if there still is a tie the earliest PAR admission is choosen
T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME AS (
    SELECT *,
           ROW_NUMBER() OVER (
               PARTITION BY VtfId_LopNr 
               ORDER BY
                        CASE DX_GROUP
                            WHEN 'TBI' THEN 1
                            WHEN 'ASAH' THEN 2
                            WHEN 'AIS' THEN 3
                            WHEN 'ICH' THEN 4
                            WHEN 'ABM' THEN 5
                            WHEN 'CFX' THEN 6
                            WHEN 'ENC' THEN 7
                            WHEN 'TUM' THEN 8
                            WHEN 'SEP' THEN 9
                            WHEN 'HC' THEN 10
                            ELSE 11 -- for any other value not specified
                        END,
                        INDATUM
           ) AS DX_ORDER
    FROM T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX
    WHERE DX_GROUP != 'OTHER'
)
,

-- DESCRIPTIVE_SIR collects data from several SIR tables (including SAPS, SOFA) joined on SIR admission ID
DESCRIPTIVE_SIR AS (
   SELECT
    -----------------------------------------
    ------------- DEMOGRAHPICS --------------
    -----------------------------------------
        S.VtfId_LopNr,
        S.AvdNamn AS sir_icu_name,
        CASE S.SjukhusTyp
            WHEN 'Länssjukhus' THEN 'Regional Hospital'
            WHEN 'Länsdelssjukhu' THEN 'Community Hospital'
            WHEN 'Regionsjukhus' THEN 'University Hospital'
          END AS sir_hospital_type,
        S.InskrTidPunkt AS sir_adm_time,
        S.UtskrTidPunkt AS sir_dsc_time,
        S.VardTidMinuter AS sir_total_time,
    --- Height, weight, BMI ---
        S.Lengd AS admission_height,
        S.AnkIvaVikt AS admission_weight,
        S.AnkIvaVikt / (S.Lengd * S.Lengd) AS BMI,
    --- DNR orders (in the SIR_BEGRANSNINGAR VtfId_LopNr are only present if there is a DNR order) --- 
        CASE WHEN S.VtfId_LopNr IN (SELECT VtfId_LopNr FROM SIR_BEGRANSNINGAR) THEN 1 ELSE 0 END AS DNR,

    --- After hours discharge ---
        CASE
            WHEN strftime('%H', datetime(S.UtskrTidPunkt, 'unixepoch')) IN ("8","9","10","11","12","13","14","15","16") THEN 1 ELSE 0 END AS icu_discharge_daytime,
        CASE
            WHEN strftime('%H', datetime(S.UtskrTidPunkt, 'unixepoch')) IN ("22","23","00","01","02","03","04","05","06") THEN 1 ELSE 0 END AS icu_discharge_nighttime,
        CASE
            WHEN (
                strftime('%H', datetime(S.UtskrTidPunkt, 'unixepoch')) NOT IN ("8","9","10","11","12","13","14","15","16")
                OR 
                strftime('%w', datetime(S.UtskrTidPunkt, 'unixepoch')) IN ("0","6")
            )
            THEN 1 ELSE 0 END AS icu_discharge_afterhours
        ,

    --- After hours admit ---
        CASE
            WHEN strftime('%H', datetime(S.InskrTidPunkt, 'unixepoch')) IN ("8","9","10","11","12","13","14","15","16") THEN 1 ELSE 0 END AS icu_admit_daytime,
        CASE
            WHEN strftime('%H', datetime(S.InskrTidPunkt, 'unixepoch')) IN ("22","23","00","01","02","03","04","05","06") THEN 1 ELSE 0 END AS icu_admit_nighttime,
        CASE
            WHEN (
                strftime('%H', datetime(S.InskrTidPunkt, 'unixepoch')) NOT IN ("8","9","10","11","12","13","14","15","16")
                OR 
                strftime('%w', datetime(S.InskrTidPunkt, 'unixepoch')) IN ("0","6")
            )
            THEN 1 ELSE 0 END AS icu_admit_afterhours
        ,
    
    -----------------------------------------
    --- PHYSIOLOGY AND SEVERITY OF ILLNESS---
    -----------------------------------------

     --- Conciousness level ---
        -- Get SAPS3 values
        SAPS.SAPS3_GCS AS SAPS_GCS,
        SAPS.SAPS3_GCS_Motorik AS SAPS_GCSm,
        SAPS.SAPS3_RLS85 AS SAPS_RLS85,

        -- Get worst SOFA conciousness values --
        SOFA.MAX_RLS85 as SOFA_worst_RLS85,
        SOFA.MIN_GCS as SOFA_worst_GCS,
        SOFA.MIN_GCS_Motorik as SOFA_worst_GCSm,
    -- Overall worst conciousness recorded in either SOFA or SAPS
        CASE
            WHEN SAPS.SAPS3_RLS85 IS NULL AND SOFA.MAX_RLS85 IS NULL THEN NULL
            WHEN SAPS.SAPS3_RLS85 IS NULL THEN SOFA.MAX_RLS85
            WHEN SOFA.MAX_RLS85 IS NULL THEN SAPS.SAPS3_RLS85
            ELSE MAX(SAPS.SAPS3_RLS85, SOFA.MAX_RLS85)
            END AS overall_worst_RLS85,
        
        CASE
            WHEN SAPS.SAPS3_GCS IS NULL AND SOFA.MIN_GCS IS NULL THEN NULL
            WHEN SAPS.SAPS3_GCS IS NULL THEN SOFA.MIN_GCS
            WHEN SOFA.MIN_GCS IS NULL THEN SAPS.SAPS3_GCS
            ELSE MIN(SAPS.SAPS3_GCS, SOFA.MIN_GCS)
            END AS overall_worst_GCS,

        CASE
            WHEN SAPS.SAPS3_GCS_Motorik IS NULL AND SOFA.MIN_GCS_Motorik IS NULL THEN NULL
            WHEN SAPS.SAPS3_GCS_Motorik IS NULL THEN SOFA.MIN_GCS_Motorik
            WHEN SOFA.MIN_GCS_Motorik IS NULL THEN SAPS.SAPS3_GCS_Motorik
            ELSE MIN(SAPS.SAPS3_GCS_Motorik, SOFA.MIN_GCS_Motorik)
            END AS overall_worst_GCSm,

        -- Define if obtunded as per SAPS3 or SAPS3 and SOFA recording
        CASE 
            WHEN ((SAPS.SAPS3_GCS IS NOT NULL AND SAPS.SAPS3_GCS != 15) OR (SAPS.SAPS3_RLS85 IS NOT NULL AND SAPS.SAPS3_RLS85 != 1)) THEN 1
            WHEN (SAPS.SAPS3_GCS IS NULL AND SAPS.SAPS3_RLS85 IS NULL) THEN NULL
            ELSE 0
            END AS SAPS_obtunded,

        CASE 
            WHEN (
                (SOFA.MIN_GCS IS NOT NULL AND SOFA.MIN_GCS != 15) OR
                (SOFA.MAX_RLS85 IS NOT NULL AND SOFA.MAX_RLS85 != 1) OR
                (SAPS.SAPS3_GCS IS NOT NULL AND SAPS.SAPS3_GCS != 15) OR
                (SAPS.SAPS3_RLS85 IS NOT NULL AND SAPS.SAPS3_RLS85 != 1)
                ) THEN 1
            WHEN (SAPS.SAPS3_GCS IS NULL AND SAPS.SAPS3_RLS85 IS NULL AND SOFA.MIN_GCS IS NULL AND SOFA.MAX_RLS85 IS NULL) THEN NULL
            ELSE 0
            END AS overall_obtunded,
        
        -- Define unconciousness as per SAPS3 or SAPS3 and SOFA recording
        -- Note that a few ICUs report both RLS and GCS for some patients, this means that simple checks
        -- like number SAPS_RLS > 3 + SAPS_GCS <9 != SAPS_UNCONCIOUS will fail
        CASE 
            WHEN ((SAPS.SAPS3_GCS IS NOT NULL AND SAPS.SAPS3_GCS < 9) OR (SAPS.SAPS3_RLS85 IS NOT NULL AND SAPS.SAPS3_RLS85 > 3)) THEN 1
            WHEN (SAPS.SAPS3_GCS IS NULL AND SAPS.SAPS3_RLS85 IS NULL) THEN NULL
            ELSE 0
            END AS SAPS_unconcious,

        CASE 
            WHEN (
                (SOFA.MIN_GCS IS NOT NULL AND SOFA.MIN_GCS < 9) OR
                (SOFA.MAX_RLS85 IS NOT NULL AND SOFA.MAX_RLS85 > 3) OR
                (SAPS.SAPS3_GCS IS NOT NULL AND SAPS.SAPS3_GCS < 9) OR
                (SAPS.SAPS3_RLS85 IS NOT NULL AND SAPS.SAPS3_RLS85 > 3)
                ) THEN 1
            WHEN (SAPS.SAPS3_GCS IS NULL AND SAPS.SAPS3_RLS85 IS NULL AND SOFA.MIN_GCS IS NULL AND SOFA.MAX_RLS85 IS NULL) THEN NULL
            ELSE 0
            END AS overall_unconcious,

        CASE
            WHEN (SAPS.SAPS3_RLS85 IS NULL AND SAPS.SAPS3_GCS IS NULL) THEN NULL
            WHEN ((SAPS.SAPS3_RLS85 IS NOT NULL AND SAPS.SAPS3_RLS85 IN ('1', '2'))
                    OR (SAPS.SAPS3_GCS IS NOT NULL AND SAPS.SAPS3_GCS IN ('13', '14', '15'))) THEN "I (GCS ≥13)"
            WHEN ((SAPS.SAPS3_RLS85 IS NOT NULL AND SAPS.SAPS3_RLS85 IN ('3', '4'))
                    OR (SAPS.SAPS3_GCS IS NOT NULL AND SAPS.SAPS3_GCS IN ('7', '8', '9', '10', '11', '12'))) THEN "II (GCS 7-12)"
            WHEN ((SAPS.SAPS3_RLS85 IS NOT NULL AND SAPS.SAPS3_RLS85 IN ('5'))
                    OR (SAPS.SAPS3_GCS IS NOT NULL AND SAPS.SAPS3_GCS IN ('6'))) THEN "III (GCS 6)"
            WHEN ((SAPS.SAPS3_RLS85 IS NOT NULL AND SAPS.SAPS3_RLS85 IN ('6'))
                    OR (SAPS.SAPS3_GCS IS NOT NULL AND SAPS.SAPS3_GCS IN ('5'))) THEN "IV (GCS 5)"
            WHEN ((SAPS.SAPS3_RLS85 IS NOT NULL AND SAPS.SAPS3_RLS85 IN ('7', '8'))
                    OR (SAPS.SAPS3_GCS IS NOT NULL AND SAPS.SAPS3_GCS IN ('3', '4'))) THEN "V (GCS ≤4)"
            ELSE 0
        END AS sir_consciousness_level,

        --- Assisted ventilation ---
        CASE
            WHEN SAPS.SAPS3_Ventilator = "Ja" THEN 1 
            WHEN SAPS.SAPS3_Ventilator IS NULL THEN NULL
            ELSE 0 END as SAPS_AMV,
        CASE
            WHEN S.VtfId_LopNr IN (SELECT VtfId_LopNr FROM SIR_ATGARDER WHERE KvaKod = "DG021") THEN 1 ELSE 0 END AS KVA_IMV,
        CASE
            WHEN S.VtfId_LopNr IN (SELECT VtfId_LopNr FROM SIR_ATGARDER WHERE KvaKod = "DG023") THEN 1 ELSE 0 END AS KVA_NIV,
        CASE
            WHEN
                SAPS.SAPS3_Ventilator = "Ja"
                OR
                S.VtfId_LopNr IN (SELECT VtfId_LopNr FROM SIR_ATGARDER WHERE KvaKod IN ('DG021', 'DG023'))
            THEN 1 ELSE 0 END AS any_AMV,
        
        --- Respiratory status ---
        --  per SAPS3 --
        SAPS.SAPS3_PaO2 / (SAPS.SAPS3_FiO2 / 100) as SAPS_PFI,
        SAPS.SAPS3_PaO2 as SAPS_PAO2,
        CASE
            WHEN SAPS.SAPS3_PaO2 IS NULL THEN NULL
            WHEN SAPS.SAPS3_PaO2 < 8 THEN 1
            WHEN SAPS.SAPS3_PaO2 >= 8 THEN 0
            END AS SAPS_hypoxia,

        -- ARDS criteria per SAPS3, conditional assisted mechanical ventilation --
        CASE
            WHEN SAPS.SAPS3_PaO2 / (SAPS.SAPS3_FiO2 / 100) < 26.6 AND SAPS.SAPS3_Ventilator = 'Ja' AND SAPS.SAPS3_PaO2 / (SAPS.SAPS3_FiO2 / 100) IS NOT NULL THEN 1
            WHEN SAPS.SAPS3_PaO2 / (SAPS.SAPS3_FiO2 / 100) OR SAPS.SAPS3_Ventilator IS NULL THEN NULL
            ELSE 0
            END AS ARDS,
        -- 
        CASE
            WHEN S.VtfId_LopNr IN (
                SELECT VtfId_LopNr
                FROM SIR_DIAGNOSER
                WHERE ICD10 LIKE 'J96%'
                OR ICD10 LIKE 'J80%')

                OR

                S.VtfId_LopNr IN (
                SELECT VtfId_LopNr
                FROM SIR_ATGARDER
                WHERE KvaKod IN ("GAA10", "TGA35", "TGA30", 'UGC12')
                )
            THEN 1 ELSE 0 END AS respiratory_instability_markers,

        --- Cardiovascular ---
        -- SAPS3 min SBP
        SAPS.SAPS3_SystBTMin as SAPS_min_SBP,

        -- SAPS3 max HR
        SAPS.SAPS3_HjartfrekvMax as SAPS_max_HR,

        -- SAPS3 tachycardia HR > 110 
        CASE
            WHEN SAPS.SAPS3_HjartfrekvMax > 110 THEN 1
            WHEN SAPS.SAPS3_HjartfrekvMax IS NULL THEN NULL
            ELSE 0 END AS SAPS_tachycardia,

        -- SAPS3 bradycardia HR < 50
        CASE
            WHEN SAPS.SAPS3_HjartfrekvMax < 50 THEN 1
            WHEN SAPS.SAPS3_HjartfrekvMax IS NULL THEN NULL
            ELSE 0 END AS SAPS_bradycardia,

        -- SAPS3 hypotension SBP <90 --
        CASE
            WHEN SAPS.SAPS3_SystBTMin < 90 THEN 1
            WHEN SAPS.SAPS3_SystBTMin IS NULL THEN NULL
            ELSE 0 END AS SAPS_hypotension,

         -- SAPS3 hypertension SBP >180 --
        CASE
            WHEN SAPS.SAPS3_SystBTMin > 180 THEN 1
            WHEN SAPS.SAPS3_SystBTMin IS NULL THEN NULL
            ELSE 0 END AS SAPS_hypertension,

        -- ICD10 and KVÅ makers for CV instability -
        CASE
            WHEN S.VtfId_LopNr IN (
                SELECT VtfId_LopNr
                FROM SIR_DIAGNOSER
                WHERE ICD10 LIKE 'I46%'
                OR ICD10 LIKE 'I490%'
                OR ICD10 LIKE 'I47%'
                OR ICD10 LIKE 'I21%'
                OR ICD10 LIKE 'R57%'
                OR ICD10 LIKE 'I71%'
                OR ICD10 LIKE 'I441%'
                OR ICD10 LIKE 'I442%'
                OR ICD10 LIKE 'I26%'
                OR ICD10 LIKE 'I31%'
                OR ICD10 LIKE 'I42%'
                OR ICD10 LIKE 'I50%'
                )

                OR

                S.VtfId_LopNr IN (
                    SELECT VtfId_LopNr
                    FROM SIR_ATGARDER
                    WHERE KvaKod IN ('SQ351','SS199','DF025', 'DF027', 'DF028', 'FPE96', 'TFE00')
                )
            THEN 1
            ELSE 0
            END AS hemodynamic_instability_markers,

        CASE
            WHEN S.VtfId_LopNr IN (
                SELECT VtfId_LopNr
                FROM SIR_SOFA
                WHERE Noradrenalin = "> 0,1"
            ) THEN 1 ELSE 0 END AS SOFA_high_norepi_dose,

        --- General SAPS3 data ---
        SAPS.SAPS3_Score as SAPS_total_score,
        SAPS.SAPS3_pHMin as SAPS_min_pH,
        SAPS.SAPS3_KroppstempMax as SAPS_max_temp,

        --- SAPS 3 acidosis (pH <7.25) ---
        CASE
            WHEN SAPS.SAPS3_pHMin < 7.25 THEN 1 
            WHEN SAPS.SAPS3_pHMin IS NULL THEN NULL
            ELSE 0 END AS SAPS_acidosis,

        --- SAPS 3 hypothermia (t <35)---
        CASE
            WHEN SAPS.SAPS3_KroppstempMax < 35 THEN 1 
            WHEN SAPS.SAPS3_KroppstempMax IS NULL THEN NULL
            ELSE 0 END AS SAPS_hypothermia,

        --- Outcomes ---
    -- Crude 7d, 30d, 90d, 365d mortality from ICU admission
        CASE WHEN
            JULIANDAY(strftime('%Y-%m-%d', substr(DO.DODSDAT, 1, 4) || '-' || substr(DO.DODSDAT, 5, 2) || '-' || substr(DO.DODSDAT, 7, 2) || ' 00:00:00')) - JULIANDAY(date(S.InskrTidPunkt, 'unixepoch')) <= 7 THEN 1 ELSE 0 END AS d7,
        CASE WHEN
            JULIANDAY(strftime('%Y-%m-%d', substr(DO.DODSDAT, 1, 4) || '-' || substr(DO.DODSDAT, 5, 2) || '-' || substr(DO.DODSDAT, 7, 2) || ' 00:00:00')) - JULIANDAY(date(S.InskrTidPunkt, 'unixepoch')) <= 30 THEN 1 ELSE 0 END AS d30,
        CASE WHEN
            JULIANDAY(strftime('%Y-%m-%d', substr(DO.DODSDAT, 1, 4) || '-' || substr(DO.DODSDAT, 5, 2) || '-' || substr(DO.DODSDAT, 7, 2) || ' 00:00:00')) - JULIANDAY(date(S.InskrTidPunkt, 'unixepoch')) <= 90 THEN 1 ELSE 0 END AS d90,
        CASE WHEN
            JULIANDAY(strftime('%Y-%m-%d', substr(DO.DODSDAT, 1, 4) || '-' || substr(DO.DODSDAT, 5, 2) || '-' || substr(DO.DODSDAT, 7, 2) || ' 00:00:00')) - JULIANDAY(date(S.InskrTidPunkt, 'unixepoch')) <= 180 THEN 1 ELSE 0 END AS d180,
        CASE WHEN
            JULIANDAY(strftime('%Y-%m-%d', substr(DO.DODSDAT, 1, 4) || '-' || substr(DO.DODSDAT, 5, 2) || '-' || substr(DO.DODSDAT, 7, 2) || ' 00:00:00')) - JULIANDAY(date(S.InskrTidPunkt, 'unixepoch')) <= 365 THEN 1 ELSE 0 END AS d365,
 ---- Days alive from ICU  admission
        JULIANDAY(strftime('%Y-%m-%d', substr(DO.DODSDAT, 1, 4) || '-' || substr(DO.DODSDAT, 5, 2) || '-' || substr(DO.DODSDAT, 7, 2) || ' 00:00:00')) - JULIANDAY(date(S.InskrTidPunkt, 'unixepoch')) AS days_alive
    FROM SIR_BASDATA S
    LEFT JOIN SIR_SAPS3 SAPS on S.VtfId_LopNr= SAPS.VtfId_LopNr
    LEFT JOIN (
            SELECT
                VtfId_LopNr,
                MAX(RLS85) as MAX_RLS85,
                MIN(GCS_Motorik + GCS_Ogon + GCS_Verbal) as MIN_GCS,
                MIN(GCS_Motorik) as MIN_GCS_Motorik
            FROM SIR_SOFA
            GROUP BY VtfId_LopNr
       ) AS SOFA on S.VtfId_LopNr = SOFA.VtfId_LopNr
    LEFT JOIN DORS DO ON S.LopNr = DO.LopNr
),

-- DESCRIPTIVE_SIR collects data from PAR and DORS (data on death date) on PAR_HADM_CONT_DATES id and in
-- the case of death date data on patient ID + admission date in PAR.
DESCRIPTIVE_PAR AS (
    SELECT
        P.HADM_ID,
        P.CONT_HADM_ID,
        P.CONT_HADM_ADM_DATE,
        P.CONT_HADM_DSC_DATE,
        P.Alder AS age,
        CASE P.Sjukhus
                WHEN '11001' THEN 'Karolinska universitetssjukhuset, Solna'
                WHEN '11003' THEN 'Karolinska universitetssjukhuset, Solna'
                WHEN '51001' THEN 'Sahlgrenska universitetssjukhuset'
                WHEN '12001' THEN 'Akademiska sjukhuset'
                WHEN '21001' THEN 'Universitetssjukhuset i Linköping'
                WHEN '64001' THEN 'Norrlands universitetssjukhus'
                WHEN '41001' THEN 'Universitetssjukhuset i Lund'
                WHEN '41002' THEN 'Universitetssjukhuset i Lund'
                ELSE P.Sjukhus -- If none of the above cases match, keep the original value
        END AS par_tertiary_center,
        CASE WHEN P.Kon = '1' THEN 0 ELSE 1 END AS sex_female
    FROM PAR_HADM_CONT_DATES P
),

-- DAOH_STEP_1_90, DAOH_STEP_2_90, DAOH_STEP_3_90 and finally DAOH_90 calculates "Days Alive and Out of Hospital",
-- i.e. the number of hospital free days alive within a 90 day time period starting from a given ICU admission in SIR
-- The first 3 steps are cumbersome preprocessing steps
-- STEP 1: Joins the PAR admissions on all SIR admissions for a given patient, some "helper columns" such as SIR adm date + 89 days are created
-- PAR admissions that do not overlap the SIR admission date or +89 date are discarded in this step.
-- STEP 2: Works per SIR ICU admission over all joined rows with unique PAR admissions. Here the time-cohorent PAR admissions will be
-- grouped together and given a common index. The code should be able to handle the odd overlapping admissions (eg. cases where there are
-- admissions ranging from Unix epoch days 17000-17030, 17010-17020, 17030-17031). Also if a patient has a PAR discharge on day D and
-- an admission on day D + 1, it will be counted as a continious admission, allowing for some clerical error and clinic transfers around midnight.
-- While it is possible that the patient actually was at her home, the significance of such a short stay at home should not be overestimated.
-- STEP 3: This "caps" the PAR admission dates within the bounds of the SIR admission date + 89 days. Next, all days admitted are summed.
-- DAOH_90: Finally, in DAOH_90 the number of DAOH_90 are calculated. If the patient dies within 90 days, DAOH_90 is set to zero.
DAOH_90_STEP_1 AS (
    SELECT
        S.LopNr,
        S.VtfId_LopNr,
        S.InskrTidPunkt,
        S.InskrTidPunkt / 86400 as S_INDATUM,
        -- Add end date for DAOH-90
        S.InskrTidPunkt / 86400 + 89 as S_INDATUM_90,
        P.LopNr,
        P.INDATUM,
        P.UTDATUM
    FROM SIR_BASDATA S
    LEFT JOIN PAR_HADM P on S.LopNr = P.LopNr
    -- Keep only joined PAR admits with discharge on the same day or later as ICU admit
    WHERE S.InskrTidPunkt / 86400 - P.UTDATUM <= 0
    -- Keep only joined PAR admimts with admission date within the DAOH-90 end date (inclusive of end date)
    AND (S.InskrTidPunkt / 86400 + 89) - P.INDATUM >= 0
),

DAOH_90_STEP_2 AS (
    SELECT *,
        SUM(new_admission) OVER (PARTITION BY VtfId_LopNr ORDER BY INDATUM) AS ADM_GROUP
    FROM (
        SELECT *,
            CASE
            -- "+ 1" allows to group admissions where the first ends at day D and the second starts at D + 1
                WHEN INDATUM <= MAX(prev_UTDATUM) OVER (PARTITION BY VtfId_LopNr ORDER BY INDATUM ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) + 1
                THEN 0
                ELSE 1
            END AS new_admission
-- This part gets the latest discharge date among all previous admissions for a given patient (technically: for a given ICU admit)
-- This is necessary because due to overlapping admissions, the preceding row could represent an overlapping admission that ends well before the one before it
        FROM (
            SELECT *,
                MAX(UTDATUM) OVER (PARTITION BY VtfId_LopNr ORDER BY INDATUM ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS prev_UTDATUM
            FROM DAOH_90_STEP_1
        )
    )
),

-- WITHIN VtfId_LopNr, merge dates for all groups of par admissions
DAOH_90_STEP_3 AS (
    SELECT 
        VtfId_LopNr,
        LopNr,
        InskrTidPunkt,
        S_INDATUM,
        S_INDATUM_90,
        ADM_GROUP,
        MAX(MIN(INDATUM), S_INDATUM) AS min_INDATUM,
        MIN(MAX(UTDATUM), S_INDATUM_90) AS max_UTDATUM,     -- Cap max_UTDATUM at S_INDATUM_90
        MIN(MAX(UTDATUM), S_INDATUM_90) - MAX(MIN(INDATUM), S_INDATUM) + 1 as ADMITTED_DAYS
    FROM DAOH_90_STEP_2
    GROUP BY VtfId_LopNr, ADM_GROUP
),

DAOH_90 AS (
    SELECT 
        VtfId_LopNr,
        -- Calculate days_alive
        JULIANDAY(strftime('%Y-%m-%d', substr(DO.DODSDAT, 1, 4) || '-' || substr(DO.DODSDAT, 5, 2) || '-' || substr(DO.DODSDAT, 7, 2) || ' 00:00:00')) 
        - JULIANDAY(date(D.InskrTidPunkt, 'unixepoch')) AS days_alive,
        
        -- Calculate DAOH_90 using a CASE statement
        CASE 
            WHEN JULIANDAY(strftime('%Y-%m-%d', substr(DO.DODSDAT, 1, 4) || '-' || substr(DO.DODSDAT, 5, 2) || '-' || substr(DO.DODSDAT, 7, 2) || ' 00:00:00')) 
                 - JULIANDAY(date(D.InskrTidPunkt, 'unixepoch')) <= 90
            THEN 0
            ELSE 90 - SUM(ADMITTED_DAYS)
        END AS DAOH_90

    FROM DAOH_90_STEP_3 D
    LEFT JOIN DORS DO ON D.LopNr = DO.LopNr
    GROUP BY VtfId_LopNr
),

-- DAOH_180: repeat the same as above
DAOH_180_STEP_1 AS (
    SELECT
        S.LopNr,
        S.VtfId_LopNr,
        S.InskrTidPunkt,
        S.InskrTidPunkt / 86400 as S_INDATUM,
        -- Add end date for DAOH-180
        S.InskrTidPunkt / 86400 + 179 as S_INDATUM_180,
        P.LopNr,
        P.INDATUM,
        P.UTDATUM
    FROM SIR_BASDATA S
    LEFT JOIN PAR_HADM P on S.LopNr = P.LopNr
    -- Keep only joined PAR admits with discharge on the same day or later as ICU admit
    WHERE S.InskrTidPunkt / 86400 - P.UTDATUM <= 0
    -- Keep only joined PAR admimts with admission date within the DAOH-90 end date (inclusive of end date)
    AND (S.InskrTidPunkt / 86400 + 179) - P.INDATUM >= 0
),

DAOH_180_STEP_2 AS (
    SELECT *,
        SUM(new_admission) OVER (PARTITION BY VtfId_LopNr ORDER BY INDATUM) AS ADM_GROUP
    FROM (
        SELECT *,
            CASE
            -- "+ 1" allows to group admissions where the first ends at day D and the second starts at D + 1
                WHEN INDATUM <= MAX(prev_UTDATUM) OVER (PARTITION BY VtfId_LopNr ORDER BY INDATUM ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) + 1
                THEN 0
                ELSE 1
            END AS new_admission
-- This part gets the latest discharge date among all previous admissions for a given patient (technically: for a given ICU admit)
-- This is necessary because due to overlapping admissions, the preceding row could represent an overlapping admission that ends well before the one before it
        FROM (
            SELECT *,
                MAX(UTDATUM) OVER (PARTITION BY VtfId_LopNr ORDER BY INDATUM ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS prev_UTDATUM
            FROM DAOH_180_STEP_1
        )
    )
),

-- WITHIN VtfId_LopNr, merge dates for all groups of par admissions
DAOH_180_STEP_3 AS (
    SELECT 
        VtfId_LopNr,
        LopNr,
        InskrTidPunkt,
        S_INDATUM,
        S_INDATUM_180,
        ADM_GROUP,
        MAX(MIN(INDATUM), S_INDATUM) AS min_INDATUM,
        MIN(MAX(UTDATUM), S_INDATUM_180) AS max_UTDATUM,     -- Cap max_UTDATUM at S_INDATUM_180
        MIN(MAX(UTDATUM), S_INDATUM_180) - MAX(MIN(INDATUM), S_INDATUM) + 1 as ADMITTED_DAYS
    FROM DAOH_180_STEP_2
    GROUP BY VtfId_LopNr, ADM_GROUP
),

DAOH_180 AS (
    SELECT 
        VtfId_LopNr,
        -- Calculate days_alive
        JULIANDAY(strftime('%Y-%m-%d', substr(DO.DODSDAT, 1, 4) || '-' || substr(DO.DODSDAT, 5, 2) || '-' || substr(DO.DODSDAT, 7, 2) || ' 00:00:00')) 
        - JULIANDAY(date(D.InskrTidPunkt, 'unixepoch')) AS days_alive,
        
        -- Calculate DAOH_90 using a CASE statement
        CASE 
            WHEN JULIANDAY(strftime('%Y-%m-%d', substr(DO.DODSDAT, 1, 4) || '-' || substr(DO.DODSDAT, 5, 2) || '-' || substr(DO.DODSDAT, 7, 2) || ' 00:00:00')) 
                 - JULIANDAY(date(D.InskrTidPunkt, 'unixepoch')) <= 180
            THEN 0
            ELSE 180 - SUM(ADMITTED_DAYS)
        END AS DAOH_180

    FROM DAOH_180_STEP_3 D
    LEFT JOIN DORS DO ON D.LopNr = DO.LopNr
    GROUP BY VtfId_LopNr
),


-- The SUMMARY_TABLE joins T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_TIME_HIERARCHY containing SIR admission ID, PAR data and DX
-- with DESCRIPTIVE_PAR and DESCRIPTIVE_SIR. Finally, only one PAR admission is matched with each
-- ICU admission based on the highest ranking DX (earliest + DX hierarchy). If a patient has multiple ICU admissions only the first is kept.
SUMMARY_TABLE AS (
SELECT
    T.LopNr,
    D.VtfId_LopNr,
    P.HADM_ID,
    P.CONT_HADM_ID,
    P.par_tertiary_center,
    T.INDATUM AS par_adm_date,
    T.UTDATUM AS par_dsc_date,
    P.sex_female,
    P.age,
    T.DX_GROUP,
    T.DX_ORDER,
    D.*,
    P.CONT_HADM_DSC_DATE - D.sir_adm_time/86400 as hospital_los_from_ICU_admit,
    DA90.DAOH_90,
    DA180.DAOH_180
FROM T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_TIME_HIERARCHY T
LEFT JOIN DESCRIPTIVE_PAR P ON T.HADM_ID = P.HADM_ID
LEFT JOIN DESCRIPTIVE_SIR D ON T.VtfId_LopNr = D.VtfId_LopNr
LEFT JOIN DAOH_180 DA180 ON T.VtfId_LopNr = DA180.VtfId_LopNr
LEFT JOIN DAOH_90 DA90 ON T.VtfId_LopNr = DA90.VtfId_LopNr
WHERE DX_ORDER = 1
GROUP BY LopNr HAVING MIN(sir_adm_time)
)