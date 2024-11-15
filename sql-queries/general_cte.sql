------------------------------------------------------------------------------
------------------------------------------------------------------------------
---------------------- EpiNIC-project SQL CTE's ------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Preamble:
-- This .sql-document contains queries for constructing common table expressions
-- central to the EpiNIC-project. When building a final query, this should be
-- the first code to be run and can be extended with additional queries in 
-- various other scripts (see README).
------------------------------------------------------------------------------
-- Table of contents:
-- 1. Hospital admission ID and creation of coherent hospital admissions
--    - PAR_HADM
--    - PAR_HADM_LAST_DSC
--    - PAR_HADM_CONT
--    - PAR_HADM_CONT_DATES
-- 2. Clerical ICU-admission information and creation of continuous
--    ICU-admissions
--    - T_ICU_ADMISSIONS
--    - T_ICU_ADM_LAST_DSC
--    - T_ICU_ADM_CONT
--    - T_ICU_ADMISSIONS_MATCHED_WITH_PAR
-- 3. Allocation of patients to diagnoses
--    - Aneurysmal Subarachnoid Haemorrhage
--    - Traumatic Brain Injury
--    - Cerebral Venous Thrombosis
--    - Intracranial Haemorrhage
--    - Arterio-venous Malformation
--    - Acute Ischaemic Stroke
--    - Acute Bacterial Meningitis
--    - Encephalitis
--    - Status Epilepticus
--    - "Isolated" Cervical Spine Fracture
--    - Subdural Haemorrhage
--    - "Isolated" Hydrocephalus
--    - Tumours
-- 4. Matching of diagnoses to admission
--    - DX
--    - T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX
--    - T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_TIME_HIERARCHY
--    - T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME
------------------------------------------------------------------------------




------------------------------------------------------------------------------
-- HOSPITAL ADMISSION ID -----------------------------------------------------
------------------------------------------------------------------------------
-- CTE PAR_HADM_ID:
-- Keeps admissions for pts >= 18 y.o.
-- Keeps admissions starting 2010-01-01
-- Adds a unique admission identifier PAR_HADM to each admission
------------------------------------------------------------------------------

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
),

------------------------------------------------------------------------------
-- CTE PAR_HADM_LAST_DSC:
-- Calculates days since last discharge for patients who have a previous 
-- admission
------------------------------------------------------------------------------

PAR_HADM_LAST_DSC AS (
    SELECT
        *,
        INDATUM - (MAX(UTDATUM) OVER (PARTITION BY LopNr ORDER BY INDATUM ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)) AS DAYS_SINCE_LAST_DSC
    FROM PAR_HADM
),

------------------------------------------------------------------------------
-- CTE PAR_HADM_CONT:
-- Flags the occurrence of a new admission to hospital if re-admission date is 
-- more than 1 day after previous discharge. 
-- Using this, a unique ID for each coherent hospital admission is created.
------------------------------------------------------------------------------

PAR_HADM_CONT AS (
    SELECT
        *,
        CASE
            WHEN DAYS_SINCE_LAST_DSC IS NULL OR DAYS_SINCE_LAST_DSC > 1 THEN 1
            ELSE 0
        END AS ADMISSION_FLAG,
        -- The following bit sums all "admission flags" and adds it do patient id * 1000 to create a unique hospital admission id
        SUM(CASE WHEN DAYS_SINCE_LAST_DSC IS NULL OR DAYS_SINCE_LAST_DSC > 1 THEN 1 ELSE 0 END) 
        OVER (PARTITION BY LopNr ROWS UNBOUNDED PRECEDING) + LopNr * 1000 AS CONT_HADM_ID
    FROM PAR_HADM_LAST_DSC
),

------------------------------------------------------------------------------
-- CTE PAR_HADM_CONT_DATES:
-- Summarises coherent hospital admissions and shows its constituents (i.e. 
-- the included HADM_ID's for each CONT_HADM_ID)
------------------------------------------------------------------------------

PAR_HADM_CONT_DATES AS (
    SELECT
        LopNr,
        CONT_HADM_ID,
        MIN(INDATUM) OVER (PARTITION BY CONT_HADM_ID) AS CONT_HADM_ADM_DATE,
        MAX(UTDATUM) OVER (PARTITION BY CONT_HADM_ID) AS CONT_HADM_DSC_DATE,
        HADM_ID,
        INDATUM,
        UTDATUM
    FROM PAR_HADM_CONT
),

------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- TERTIARY ICU ADMISSION AND ID ---------------------------------------------
------------------------------------------------------------------------------
-- CTE T_ICU_ADMISSIONS:
-- Contains some basic information about each ICU-admission
------------------------------------------------------------------------------

T_ICU_ADMISSIONS AS (
    SELECT
        S.VtfId_LopNr,
        S.LopNr,
        S.InskrTidPunkt,
        S.UtskrTidPunkt,
        S.AvdNamn,
        S.Sjukhus
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

------------------------------------------------------------------------------
-- CTE T_ICU_ADM_LAST_DSC:
-- Analogous to PAR_HADM_LAST_DSC, calculates seconds since last ICU-discharge
-- in patients who have been previously treated in a tertiary ICU. 
------------------------------------------------------------------------------

T_ICU_ADM_LAST_DSC AS (
    SELECT 
        VtfId_LopNr,
        LopNr,
        InskrTidPunkt,
        UtskrTidPunkt,
        Sjukhus,
        InskrTidPunkt - (MAX(UtskrTidPunkt) OVER (PARTITION BY LopNr ORDER BY InskrTidPunkt ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)) AS SECS_SINCE_LAST_ICU_DSC
    FROM T_ICU_ADMISSIONS
),

------------------------------------------------------------------------------
-- CTE T_ICU_ADM_CONT:
-- Analogous to PAR_HADM_CONT, flags and creates a unique ID for each
-- coherent ICU-admission to a tertiary ICU if no more than 12 hours
-- has passed since a previous tertiary ICU discharge.
------------------------------------------------------------------------------

T_ICU_ADM_CONT AS (
    SELECT
        VtfId_LopNr,
        LopNr,
        InskrTidPunkt,
        UtskrTidPunkt,
        Sjukhus,
        CASE
            WHEN SECS_SINCE_LAST_ICU_DSC IS NULL OR SECS_SINCE_LAST_ICU_DSC > (86400/2) THEN 1
            ELSE 0
        END AS ADMISSION_FLAG,
        SUM(CASE WHEN SECS_SINCE_LAST_ICU_DSC IS NULL OR SECS_SINCE_LAST_ICU_DSC > (86400/2) THEN 1 ELSE 0 END) 
        OVER (PARTITION BY LopNr ROWS UNBOUNDED PRECEDING) + LopNr * 1000 AS CONT_ICU_ID
    FROM T_ICU_ADM_LAST_DSC
),

------------------------------------------------------------------------------
-- CTE T_ICU_ADMISSIONS_MATCHED_WITH_PAR:
-- All ICU admissions in T_ICU_ADMISSIONS are matched (by left join) with 
-- PAR admissions in PAR_HADM_CONT_DATES fulfilling the criteria:
--  - PAR admission at Tertiary Hospital
--  - PAR admission starting from 14 days prior to ICU admission up to 14 days 
--    after ICU admission
--  - If no PAR admission matching the SIR admission the latter will drop out
--  - If multiple PAR admissions fulfill matching criteria, multiple rows will 
--    be returned for that SIR admission
------------------------------------------------------------------------------

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
        P.SJUKHUS,
        TC.CONT_ICU_ID
    FROM T_ICU_ADMISSIONS T
    LEFT JOIN T_ICU_ADM_CONT TC ON T.VtfId_LopNr == TC.VtfId_LopNr
    LEFT JOIN PAR_HADM P ON T.LopNr == P.LopNr
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

------------------------------------------------------------------------------



------------------------------------------------------------------------------
-- CREATION OF DIAGNOSES -----------------------------------------------------
------------------------------------------------------------------------------
-- Creation of CTE's for each diagnosis
------------------------------------------------------------------------------

-- aSAH
asah AS (
    SELECT
        P.HADM_ID,
        P.LopNr,
        P.Alder
    FROM
        PAR_HADM P
    LEFT JOIN
        DORS D ON P.LopNr = D.LopNr
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
          AND (
            (
                (P.Diagnos LIKE "I60%" -- Note the placement of the wildcard, i.e. the regex will search for the main diagnosis
                OR P.Diagnos LIKE "I671%" -- See comment above
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

------------------------------------------------------------------------------

-- TBI
tbi AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM P
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
),

------------------------------------------------------------------------------

-- Cerebral venous thrombosis
cvt AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM P
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
),

------------------------------------------------------------------------------

-- ICH
ich AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
        AND (P.Diagnos LIKE "I61%")
        -- A few likely aSAH that fulfill the above criteria will need to be filtered out as such:
        AND (P.Op NOT LIKE "%AAC00%")
        AND (P.Op NOT LIKE "%AAL00%")
),

------------------------------------------------------------------------------

-- AVM
avm AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
        AND (P.Diagnos LIKE "Q28%")
        AND (P.Op NOT LIKE "%AAL00%")
        AND (P.Op NOT LIKE "%AAC00%")
),

------------------------------------------------------------------------------

-- Acute ischemic stroke
ais AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
        AND (P.Diagnos LIKE "I63%")
        AND (P.Diagnos NOT LIKE "I636%")
        -- a few likely aSAH that fulfill the above criteria will need to be filtered out as such:
        AND (P.Op NOT LIKE "%AAC00%")
        AND (P.Op NOT LIKE "%AAL00%")
),

------------------------------------------------------------------------------

-- Acute bacterial meningitis
abm AS (
    SELECT
        P.HADM_ID,
        P.LopNr
        FROM
        PAR_HADM P
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

------------------------------------------------------------------------------

-- Encephalitis
ence AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM P
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

------------------------------------------------------------------------------

-- Status epilepticus
se AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
        AND (
            P.Diagnos LIKE "G41%" OR
            -- also include epilepsy
            P.Diagnos LIKE "G40%"
        )
),

------------------------------------------------------------------------------

-- "Isolated" cervical spine frx
cfx AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
        AND (
            P.Diagnos LIKE 'S12%' OR
            P.Diagnos LIKE 'S13%' OR
            P.Diagnos LIKE 'S14%'
        )
        AND P.Diagnos NOT LIKE '%S06%'
),

------------------------------------------------------------------------------

-- Turns out there are loads of I62 (non traumatic SDH), many get evacuated, some have a traumatic sdh as secondary dx... let's pick the "clean"
-- Most get AD005 (evac chron sdh) or AD010 (evac acute sdh). Not sure it's reasonable to "split" on that.
sdh AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM P
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

------------------------------------------------------------------------------

-- "Isolated" hydrocephalus (shunt dysfunctions et al)
hc AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
        AND (
            P.Diagnos LIKE 'G91%'
        )
    -- several  likely aSAH that fulfill the above criteria will need to be filtered out as such:
    AND (P.Op NOT LIKE "%AAC00%")
    AND (P.Op NOT LIKE "%AAL00%")
),

------------------------------------------------------------------------------

--  Tumours
tum AS (
    SELECT
        P.HADM_ID,
        P.LopNr
    FROM
        PAR_HADM P
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

------------------------------------------------------------------------------
-- CTE DX:
-- Finds the diagnostic group for each entry in PAR_HADM_CONT_DATES. 
-- Note that one HADM can have several diagnostic criteria fulfilled (although 
-- it is rare), therefore there can be several rows for same admission
------------------------------------------------------------------------------

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
        PAR_HADM P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
),

------------------------------------------------------------------------------
-- CTE T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX:
-- Create a window where the SIR-PAR matched cohort is joined (on
-- PAR admission ID) with the diagnostic group window (based on PAR dx)
------------------------------------------------------------------------------

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
        D.DX_GROUP,
        T.CONT_ICU_ID
    FROM T_ICU_ADMISSIONS_MATCHED_WITH_PAR T
    LEFT JOIN DX D ON T.HADM_ID = D.HADM_ID
    LEFT JOIN PAR_HADM P ON T.HADM_ID = P.HADM_ID
),

------------------------------------------------------------------------------
-- CTE's T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_TIME_HIERARCHY & 
-- T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME:
-- Helps resolving tie situations where one SIR admission is associated with 
-- several PAR admissions. 
-- In the case of T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_TIME_HIERARCHY, 
-- the earliest (within the time window) admissions is chosen, if there still is 
-- a tie a hierarchical ordering of diagnosis will choose one admission only.
-- In the case of T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME, 
-- a hierarchical ordering of diagnosis will choose one admission and if there
-- is still a tie the earliest (within the time window) admissions is chosen.
------------------------------------------------------------------------------

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
),

------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
