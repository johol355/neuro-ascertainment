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
--    - ICU_ADM_LAST_DSC
--    - ICU_ADM_CONT
--    - ICU_ADMISSIONS_MATCHED_WITH_PAR
--    - T_ICU_ADMISSIONS
--    - T_ICU_ADM_LAST_DSC
--    - T_ICU_ADM_CONT
--    - T_ICU_ADMISSIONS_MATCHED_WITH_PAR
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
-- CTE ICU_ADM_LAST_DSC:
-- Analogous to PAR_HADM_LAST_DSC, calculates seconds since last ICU-discharge. 
------------------------------------------------------------------------------

ICU_ADM_LAST_DSC AS (
    SELECT 
        VtfId_LopNr,
        LopNr,
        InskrTidPunkt,
        UtskrTidPunkt,
        AvdNamn,
        Sjukhus,
        InskrTidPunkt - (MAX(UtskrTidPunkt) OVER (PARTITION BY LopNr ORDER BY InskrTidPunkt ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)) AS SECS_SINCE_LAST_ICU_DSC
    FROM SIR_BASDATA
),

------------------------------------------------------------------------------
-- CTE T_ICU_ADM_CONT:
-- Analogous to PAR_HADM_CONT, flags and creates a unique ID for each
-- coherent ICU-admission to an ICU if no more than 12 hours
-- has passed since a previous ICU discharge.
------------------------------------------------------------------------------

ICU_ADM_CONT AS (
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
    FROM ICU_ADM_LAST_DSC
),

ICU_ADM_CONT_DATES AS(
    SELECT
      VtfId_LopNr,
      LopNr,
      InskrTidPunkt,
      UtskrTidPunkt,
      CONT_ICU_ID,
      MIN(InskrTidPunkt) OVER (PARTITION BY CONT_ICU_ID) AS CONT_ICU_ADM_DATE,
      MAX(UtskrTidPunkt) OVER (PARTITION BY CONT_ICU_ID) AS CONT_ICU_DSC_DATE
    FROM ICU_ADM_CONT
),

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
        InskrTidPunkt - (MAX(UtskrTidPunkt) OVER (PARTITION BY LopNr ORDER BY InskrTidPunkt ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)) AS T_SECS_SINCE_LAST_ICU_DSC
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
            WHEN T_SECS_SINCE_LAST_ICU_DSC IS NULL OR T_SECS_SINCE_LAST_ICU_DSC > (86400/2) THEN 1
            ELSE 0
        END AS T_ADMISSION_FLAG,
        SUM(CASE WHEN T_SECS_SINCE_LAST_ICU_DSC IS NULL OR T_SECS_SINCE_LAST_ICU_DSC > (86400/2) THEN 1 ELSE 0 END) 
        OVER (PARTITION BY LopNr ROWS UNBOUNDED PRECEDING) + LopNr * 1000 AS T_CONT_ICU_ID
    FROM T_ICU_ADM_LAST_DSC
),

T_ICU_ADM_CONT_DATES AS(
    SELECT
      VtfId_LopNr,
      LopNr,
      InskrTidPunkt,
      UtskrTidPunkt,
      CONT_ICU_ID,
      MIN(InskrTidPunkt) OVER (PARTITION BY T_CONT_ICU_ID) AS T_CONT_ICU_ADM_DATE,
      MAX(UtskrTidPunkt) OVER (PARTITION BY T_CONT_ICU_ID) AS T_CONT_ICU_DSC_DATE
    FROM T_ICU_ADM_CONT
),

------------------------------------------------------------------------------
-- CTE T_ICU_ADMISSIONS_MATCHED_WITH_PAR & T_ICU_ADMISSIONS_MATCHED_WITH_PAR:
-- All ICU admissions in T_ICU_ADMISSIONS are matched (by left join) with 
-- PAR admissions in PAR_HADM_CONT_DATES fulfilling the criteria:
--  - PAR admission at Tertiary Hospital
--  - PAR admission starting from 14 days prior to ICU admission up to 14 days 
--    after ICU admission
--  - If no PAR admission matching the SIR admission the latter will drop out
--  - If multiple PAR admissions fulfill matching criteria, multiple rows will 
--    be returned for that SIR admission
------------------------------------------------------------------------------
ICU_ADMISSIONS_MATCHED_WITH_PAR AS (
    SELECT 
        T.VtfId_LopNr,
        P.HADM_ID,
        P.CONT_HADM_ID,
        T.LopNr,
        T.InskrTidPunkt,
        T.UtskrTidPunkt,
        T.AvdNamn,
        P.INDATUM,
        P.UTDATUM,
        P.MVO,
        P.SJUKHUS,
        T.SjukhusTyp,
        TC.CONT_ICU_ID
    FROM SIR_BASDATA T
    LEFT JOIN ICU_ADM_CONT TC ON T.VtfId_LopNr == TC.VtfId_LopNr
    LEFT JOIN PAR_HADM_CONT P ON T.LopNr == P.LopNr
    WHERE T.InskrTidPunkt/86400 BETWEEN P.INDATUM - 1 AND P.UTDATUM + 1
),

T_ICU_ADMISSIONS_MATCHED_WITH_PAR AS (
    SELECT 
        T.VtfId_LopNr,
        P.HADM_ID,
        P.CONT_HADM_ID,
        T.LopNr,
        T.InskrTidPunkt,
        T.UtskrTidPunkt,
        T.AvdNamn,
        P.INDATUM,
        P.UTDATUM,
        P.MVO,
        P.SJUKHUS,
        TC.T_CONT_ICU_ID
    FROM T_ICU_ADMISSIONS T
    LEFT JOIN T_ICU_ADM_CONT TC ON T.VtfId_LopNr == TC.VtfId_LopNr
    LEFT JOIN PAR_HADM_CONT P ON T.LopNr == P.LopNr
    WHERE T.InskrTidPunkt/86400 BETWEEN P.INDATUM - 1 AND P.UTDATUM + 1
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
)