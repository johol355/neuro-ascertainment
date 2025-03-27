------------------------------------------------------------------------------
------------------------------------------------------------------------------
---------------------- EpiNIC-project SQL CTE's ------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Preamble:
-- This .sql-document contains queries for constructing Common Table Expressions (CTEs)
-- essential to the EpiNIC project. Run this document first when building a final
-- query. Extendable with additional queries from separate scripts (refer to README).

-- Table of Contents:
-- 1. Hospital Admission IDs and Creation of Continuous Hospital Admissions Concept
--    - PAR_HADM
--    - PAR_HADM_LAST_DSC
--    - PAR_HADM_CONT
--    - PAR_HADM_CONT_DATES
-- 2. Creation of Continuous ICU Admissions Concept
--    - ICU_ADM_LAST_DSC
--    - ICU_ADM_CONT
--    - ICU_ADM_CONT_DATES
--    - RANK_SIR_ADM_WITHIN_CONT_ICU_ADMISSION
--    - FIRST_ADM_WITHIN_CONT_ICU_ADMISSION
--    - ICU_ADMISSIONS_MATCHED_WITH_PAR
-- 3. Creation of Continuous Tertiary ICU Admissions Concept
--    - T_ICU_ADMISSIONS
--    - T_ICU_ADM_LAST_DSC
--    - T_ICU_ADM_CONT
--    - T_ICU_ADM_CONT_DATES
--    - RANK_SIR_ADM_WITHIN_T_CONT_ICU_ADMISSION
--    - FIRST_ADM_WITHIN_T_CONT_ICU_ADMISSION

-- Summary of CTEs:
-- 1. Hospital Admission IDs and Creation of Continuous Hospital Admissions Concept:

-- CTE: PAR_HADM
-- Assigns a unique admission ID (HADM_ID) to each hospital admission.

-- CTE: PAR_HADM_LAST_DSC
-- Calculates the number of days since the patient's previous discharge (DAYS_SINCE_LAST_DSC).

-- CTE: PAR_HADM_CONT
-- Defines coherent hospital admission IDs (CONT_HADM_ID).

-- CTE: PAR_HADM_CONT_DATES
-- Summarizes coherent hospital admissions by providing:
-- - Earliest admission date (CONT_HADM_ADM_DATE).
-- - Latest discharge date (CONT_HADM_DSC_DATE).
-- - Lists constituent hospital admission IDs.

-- 2. Creation of Continuous ICU Admissions Concept

-- CTE: ICU_ADM_LAST_DSC
-- Computes seconds elapsed since the patient's previous ICU discharge (SECS_SINCE_LAST_ICU_DSC).

-- CTE: ICU_ADM_CONT
-- Defines continuous ICU admissions by:
-- - Flagging a new ICU admission when more than 12 hours (43,200 seconds) elapse post-discharge.
-- - Assigning a unique continuous ICU admission ID (CONT_ICU_ID).

-- CTE: ICU_ADM_CONT_DATES
-- Provides dates for each coherent ICU admission period:
-- - Admission start (CONT_ICU_ADM_DATE).
-- - Discharge end (CONT_ICU_DSC_DATE).

-- CTE: RANK_SIR_ADM_WITHIN_CONT_ICU_ADMISSION
-- Ranks individual ICU admissions within each continuous ICU admission.

-- CTE: FIRST_ADM_WITHIN_CONT_ICU_ADMISSION
-- Selects first  ICU admission within each continuous ICU admission period.

-- 3. Creation of Continuous Tertiary ICU Admissions Concept:

-- CTE: T_ICU_ADMISSIONS
-- Filters tertiary ICU admissions based on predefined ICU departments.

-- CTE: T_ICU_ADM_LAST_DSC
-- Calculates seconds elapsed since the last tertiary ICU discharge (T_SECS_SINCE_LAST_ICU_DSC).

-- CTE: T_ICU_ADM_CONT
-- Defines coherent tertiary ICU admissions by:
-- - Flagging new tertiary ICU admissions with a gap exceeding 12 hours post-discharge.
-- - Generating unique tertiary ICU admission IDs (T_CONT_ICU_ID).

-- CTE: T_ICU_ADM_CONT_DATES
-- Provides start and end timestamps of continuous tertiary ICU admissions.

-- CTE: RANK_SIR_ADM_WITHIN_T_CONT_ICU_ADMISSION
-- Orders tertiary ICU admissions within each continuous tertiary admission.

-- CTE: FIRST_ADM_WITHIN_T_CONT_ICU_ADMISSION
-- Selects first tertiary ICU admission within each continuous tertiary ICU admission period.

-- CTE: ICU_ADMISSIONS_MATCHED_WITH_PAR
-- Matches ICU admissions (from SIR_BASDATA) with hospital admissions (from PAR_HADM_CONT):
-- - ICU admissions matched within ±1 day of hospital admission.
-- - Includes continuous ICU admission IDs (CONT_ICU_ID).
-- - Includes continuous tertiary ICU admission IDs (T_CONT_ICU_ID) when applicable.
-- - Expands data rows in case of multiple matching hospital admissions per ICU admission.
-- - Admissions without matching hospital records are excluded.
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
                        CASE WHEN SJUKHUS NOT IN ('11001', '11003', '51001', '12001', '21001', '64001', '41001', '41002', '55010') THEN 0 ELSE 1 END
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
    SELECT *,
        CASE
            WHEN DAYS_SINCE_LAST_DSC IS NULL OR DAYS_SINCE_LAST_DSC > 1 THEN 1
            ELSE 0
        END AS ADMISSION_FLAG,
        SUM(CASE WHEN DAYS_SINCE_LAST_DSC IS NULL OR DAYS_SINCE_LAST_DSC > 1 THEN 1 ELSE 0 END) 
            OVER (PARTITION BY LopNr ORDER BY INDATUM ROWS UNBOUNDED PRECEDING) AS AdmissionSequence,
        -- Explicit identifier combining patient and admission sequence:
        LopNr || '-CONT_PAR_ADM-' || SUM(CASE WHEN DAYS_SINCE_LAST_DSC IS NULL OR DAYS_SINCE_LAST_DSC > 1 THEN 1 ELSE 0 END)
            OVER (PARTITION BY LopNr ORDER BY INDATUM ROWS UNBOUNDED PRECEDING) AS CONT_HADM_ID
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
        InskrTidpunkt,
        UtskrTidpunkt,
        AvdNamn,
        Sjukhus,
        InskrTidpunkt - (MAX(UtskrTidpunkt) OVER (PARTITION BY LopNr ORDER BY InskrTidpunkt ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)) AS SECS_SINCE_LAST_ICU_DSC
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
        *,
        CASE
            WHEN SECS_SINCE_LAST_ICU_DSC IS NULL OR SECS_SINCE_LAST_ICU_DSC > (86400/2) THEN 1
            ELSE 0
        END AS ICU_ADMISSION_FLAG,
        SUM(CASE WHEN SECS_SINCE_LAST_ICU_DSC IS NULL OR SECS_SINCE_LAST_ICU_DSC > (86400/2) THEN 1 ELSE 0 END) 
            OVER (PARTITION BY LopNr ORDER BY InskrTidpunkt ROWS UNBOUNDED PRECEDING) AS ICUAdmissionSequence,
        LopNr || '-CONT_ICU_ADM-' || SUM(CASE WHEN SECS_SINCE_LAST_ICU_DSC IS NULL OR SECS_SINCE_LAST_ICU_DSC > (86400/2) THEN 1 ELSE 0 END)
            OVER (PARTITION BY LopNr ORDER BY InskrTidpunkt ROWS UNBOUNDED PRECEDING) AS CONT_ICU_ID
    FROM ICU_ADM_LAST_DSC
),

ICU_ADM_CONT_DATES AS(
    SELECT
      *,
      MIN(InskrTidpunkt) OVER (PARTITION BY CONT_ICU_ID) AS CONT_ICU_ADM_DATE,
      MAX(UtskrTidpunkt) OVER (PARTITION BY CONT_ICU_ID) AS CONT_ICU_DSC_DATE
    FROM ICU_ADM_CONT
),

-- Get first SIR ADMISSION within a SIR CONT_ICU_ADMISSION

RANK_SIR_ADM_WITHIN_CONT_ICU_ADMISSION AS (
    SELECT *,
           ROW_NUMBER() OVER (
                PARTITION BY CONT_ICU_ID
                ORDER BY InskrTidpunkt
           ) AS SIR_ADM_ORDER
    FROM ICU_ADM_CONT_DATES
),

FIRST_ADM_WITHIN_CONT_ICU_ADMISSION AS (
    SELECT CONT_ICU_ID,
           CONT_ICU_ADM_DATE,
           CONT_ICU_DSC_DATE,
           VtfId_LopNr,
           LopNr,
           InskrTidpunkt,
           UtskrTidpunkt
    FROM RANK_SIR_ADM_WITHIN_CONT_ICU_ADMISSION
    WHERE SIR_ADM_ORDER = 1
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
        S.InskrTidpunkt,
        S.UtskrTidpunkt,
        S.AvdNamn,
        S.Sjukhus
    FROM SIR_BASDATA S
    WHERE ((S.AvdNamn IN (
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
        )) OR
        (S.AvdNamn = 'IVAUSÖ' AND InskrTidpunkt > 1388534400))
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
        InskrTidpunkt,
        UtskrTidpunkt,
        Sjukhus,
        InskrTidpunkt - (MAX(UtskrTidpunkt) OVER (PARTITION BY LopNr ORDER BY InskrTidpunkt ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)) AS T_SECS_SINCE_LAST_ICU_DSC
    FROM T_ICU_ADMISSIONS
),

------------------------------------------------------------------------------
-- CTE T_ICU_ADM_CONT:
-- Analogous to PAR_HADM_CONT, flags and creates a unique ID for each
-- coherent ICU-admission to a tertiary ICU if no more than 12 hours
-- has passed since a previous tertiary ICU discharge.
-- Note that the multiplier for in the assignment if the ID is different
-- than in ICU_ADM_CONT to generate unique identifier series for each patient
------------------------------------------------------------------------------

T_ICU_ADM_CONT AS (
    SELECT
        VtfId_LopNr,
        LopNr,
        InskrTidpunkt,
        UtskrTidpunkt,
        Sjukhus,
        CASE
            WHEN T_SECS_SINCE_LAST_ICU_DSC IS NULL OR T_SECS_SINCE_LAST_ICU_DSC > (86400/2) THEN 1
            ELSE 0
        END AS T_ICU_ADMISSION_FLAG,
        SUM(CASE WHEN T_SECS_SINCE_LAST_ICU_DSC IS NULL OR T_SECS_SINCE_LAST_ICU_DSC > (86400/2) THEN 1 ELSE 0 END) 
            OVER (PARTITION BY LopNr ORDER BY InskrTidpunkt ROWS UNBOUNDED PRECEDING) AS TertiaryICUAdmissionSequence,
        LopNr || '-CONT-TERT_ICU-ADM-' || SUM(CASE WHEN T_SECS_SINCE_LAST_ICU_DSC IS NULL OR T_SECS_SINCE_LAST_ICU_DSC > (86400/2) THEN 1 ELSE 0 END)
            OVER (PARTITION BY LopNr ORDER BY InskrTidpunkt ROWS UNBOUNDED PRECEDING) AS T_CONT_ICU_ID
    FROM T_ICU_ADM_LAST_DSC
),

T_ICU_ADM_CONT_DATES AS(
    SELECT
      VtfId_LopNr,
      LopNr,
      InskrTidpunkt,
      UtskrTidpunkt,
      T_CONT_ICU_ID,
      MIN(InskrTidpunkt) OVER (PARTITION BY T_CONT_ICU_ID) AS T_CONT_ICU_ADM_DATE,
      MAX(UtskrTidpunkt) OVER (PARTITION BY T_CONT_ICU_ID) AS T_CONT_ICU_DSC_DATE
    FROM T_ICU_ADM_CONT
),

-- Get first SIR ADMISSION within a SIR T_CONT_ICU_ADMISSION

RANK_SIR_ADM_WITHIN_T_CONT_ICU_ADMISSION AS (
    SELECT *,
           ROW_NUMBER() OVER (
                PARTITION BY T_CONT_ICU_ID
                ORDER BY InskrTidpunkt
           ) AS SIR_ADM_ORDER
    FROM T_ICU_ADM_CONT_DATES
),

FIRST_ADM_WITHIN_T_CONT_ICU_ADMISSION AS (
    SELECT T_CONT_ICU_ID,
           VtfId_LopNr,
           LopNr,
           T_CONT_ICU_ADM_DATE,
           T_CONT_ICU_DSC_DATE,
           InskrTidpunkt,
           UtskrTidpunkt
    FROM RANK_SIR_ADM_WITHIN_T_CONT_ICU_ADMISSION
    WHERE SIR_ADM_ORDER = 1
),

------------------------------------------------------------------------------
-- CTE ICU_ADMISSIONS_MATCHED_WITH_PAR:
-- All ICU admissions (in SIR_BASDATA) are matched (by left join) with 
-- PAR admissions in PAR_HADM_CONT_DATES fulfilling the criteria:
--  - PAR-vtf starting from 1 days prior to ICU admission up to 1 days 
--    after ICU admission
-- Other notes:
--  - If no PAR admission matching the SIR admission the latter will drop out
--  - If multiple PAR admissions fulfill matching criteria, multiple rows will 
--    be returned for that SIR admission
--  - Each SIR Vtf has an associated "composite" (continuous) ICU admission, here
--    this ID is added to the CTE by left joning ICU_ADM_CONT
--  - In case there is a (continuous) tertiary ICU admit associated with the SIR
--    VtfId_LopNr, this will be added to the resulting table. If the SIR admit
--    is not part of a continuous tertiary ICU admit, the row in the column will be 
--    null.
--  Example:
--  - A patient is admitted to Gävle ICU and the Dept. of Medicine in Gävle, with 
--    an ICH, before being transferred urgently to Uppsala NSICU and the Dept. of
--    Neurology. The same day, the patient is admitted to the Dept. of Neurosurgery
--    due to having an expanding hemotoma evacuated. Two days later the patient is
--    transferred to the Uppsala General ICU and admitted to the Dept. of Neurosurgery.
--    This will first generate 3 rows, one for each ICU admit, before being expanded
--    with each PAR admit with +/-1 of the ICU admits. Here likely 3 admits for the first
--    two ICU stays, and 1 for the third ICU stay. So, we would end up with 7 rows. All
--    of them should have the same "CONT_ICU_ID". The Uppsala ICU rows should all have
--    the same T_CONT_ICU_ID
-- 
--
------------------------------------------------------------------------------
ICU_ADMISSIONS_MATCHED_WITH_PAR AS (
    SELECT 
        S.VtfId_LopNr,
        P.HADM_ID,
        P.CONT_HADM_ID,
        S.LopNr,
        S.InskrTidpunkt,
        S.UtskrTidpunkt,
        S.AvdNamn,
        P.INDATUM,
        P.UTDATUM,
        P.MVO,
        P.SJUKHUS,
        S.SjukhusTyp,
        SC.CONT_ICU_ID,
        STC.T_CONT_ICU_ID
    FROM SIR_BASDATA S
    LEFT JOIN ICU_ADM_CONT SC ON S.VtfId_LopNr == SC.VtfId_LopNr
    LEFT JOIN T_ICU_ADM_CONT_DATES STC ON S.VtfId_LopNr == STC.VtfId_LopNr
    LEFT JOIN PAR_HADM_CONT P ON S.LopNr == P.LopNr
    WHERE S.InskrTidpunkt/86400 BETWEEN P.INDATUM - 1 AND P.UTDATUM + 1
)