------------------------------------------------------------------------------
------------------------------------------------------------------------------
---------------------- CREATION OF DIAGNOSES ---------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Please note: All CTE's in general_cte.sql must be run before this script --
------------------------------------------------------------------------------
-- Table of Contents:
-- 4. Allocation and Matching of Diagnoses
--    - Diagnosis Allocation (asah, ane, avm, ich, tbi, ais, abm, cvt, ence, se, cfx, sdh, hc, tum)
--    - DX
--    - ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX
--    - CONT_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX
--    - T_CONT_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX
--    - ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME
--    - CONT_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_WITH_DX_HIERARCHY_TIME
--    - T_CONT_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_WITH_DX_HIERARCHY_TIME
--    - ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_TIME_HIERARCHY

-- CTEs for Diagnosis Allocation
-- Identifies primary diagnosis groups based on ICD codes and surgical interventions:
-- - asah (Aneurysmal Subarachnoid Haemorrhage)
-- - ane (Non-ruptured Subarachnoid Aneurysm)
-- - avm (Arterio-venous Malformation)
-- - ich (Intracranial Haemorrhage)
-- - tbi (Traumatic Brain Injury)
-- - ais (Acute Ischaemic Stroke)
-- - abm (Acute Bacterial Meningitis)
-- - cvt (Cerebral Venous Thrombosis)
-- - ence (Encephalitis)
-- - se (Status Epilepticus)
-- - cfx ("Isolated" Cervical Spine Fracture)
-- - sdh (Subdural Haemorrhage)
-- - hc ("Isolated" Hydrocephalus)
-- - tum (Tumours)

-- CTE: DX
-- Determines the primary diagnostic group for each coherent hospital admission.
-- Assigns a single primary diagnosis based on predefined hierarchical criteria.

-- CTE: ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX
-- Matches ICU admissions with their corresponding hospital admissions and associated diagnoses.

-- CTEs: CONT_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX, T_CONT_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX
-- Associate the first individual ICU admission of the continuous and continuous tertiary ICU admissions with matched hospital admissions and diagnoses.

-- CTE: ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME
-- Resolves multiple matches by prioritizing hospital type, diagnosis hierarchy, and earliest admission date.

-- CTEs: CONT_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_WITH_DX_HIERARCHY_TIME, T_CONT_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_WITH_DX_HIERARCHY_TIME
-- Similar to above but applied to continuous and tertiary ICU admissions respectively.

-- CTE: ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_TIME_HIERARCHY
-- This one is kept as a legacy CTE. Resolves ties by earliest admission date first, followed by hospital type and diagnosis hierarchy.
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Creation of CTE's for each diagnosis
------------------------------------------------------------------------------
-- Each of the diagnosis-groups is subdivided into a CTE called {dx}
------------------------------------------------------------------------------

-- aSAH ----------------------------------------------------------------------

asah AS (
    SELECT
        P.HADM_ID
    FROM
        PAR_HADM P
    LEFT JOIN
        DORS D ON P.LopNr = D.LopNr
    WHERE (
            (
                (P.Diagnos LIKE "I60%" -- Note the placement of the wildcard, i.e. the regex will search for the main diagnosis
                OR ((P.Op LIKE "%AAC00%" OR P.Op LIKE "%AAL00%") AND P.Diagnos NOT LIKE "I671%"))  -- I671 is NOT included
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
            )
        )
),

-- Non-ruptured aneurysm -----------------------------------------------------
ane AS (
    SELECT
        P.HADM_ID
    FROM
        PAR_HADM P
    WHERE
        P.Diagnos LIKE "I671%"
    AND
        P.Diagnos NOT LIKE "%I60%" -- To decrease risk of mixing in asah
),

------------------------------------------------------------------------------

-- TBI -----------------------------------------------------------------------

tbi AS (
    SELECT
        P.HADM_ID
    FROM
        PAR_HADM P
    WHERE
            ((P.Diagnos LIKE "S06%") OR
            ((P.Diagnos LIKE "S020%" OR
             P.Diagnos LIKE "S021%" OR
             P.Diagnos LIKE "S028%" OR
             P.Diagnos LIKE "S029%" OR
             P.Diagnos LIKE "S071%" OR
             P.Diagnos LIKE "S04%" OR
             P.Diagnos LIKE "S09%" OR
             P.Diagnos LIKE "S12%"))) -- Previously had "AND (P.Diagnos LIKE "%S06%")""
),

------------------------------------------------------------------------------

-- Cerebral venous thrombosis ------------------------------------------------

cvt AS (
    SELECT
        P.HADM_ID
    FROM
        PAR_HADM P
    WHERE
    (P.Diagnos LIKE "G08%"
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

-- ICH -----------------------------------------------------------------------

ich AS (
    SELECT
        P.HADM_ID
    FROM
        PAR_HADM P
    WHERE
        (P.Diagnos LIKE "I61%")
        -- A few likely aSAH that fulfill the above criteria will need to be filtered out as such:
        AND (P.Op NOT LIKE "%AAC00%")
        AND (P.Op NOT LIKE "%AAL00%")
),

------------------------------------------------------------------------------

-- AVM -----------------------------------------------------------------------

avm AS (
    SELECT
        P.HADM_ID
    FROM
        PAR_HADM P
    WHERE
        (P.Diagnos LIKE "Q28%")
        AND (P.Op NOT LIKE "%AAL00%")
        AND (P.Op NOT LIKE "%AAC00%")
),

------------------------------------------------------------------------------

-- Acute ischemic stroke -----------------------------------------------------

ais AS (
    SELECT
        P.HADM_ID
    FROM
        PAR_HADM P
    WHERE
        (P.Diagnos LIKE "I63%")
        AND (P.Diagnos NOT LIKE "I636%")
        -- a few likely aSAH that fulfill the above criteria will need to be filtered out as such:
        AND (P.Op NOT LIKE "%AAC00%")
        AND (P.Op NOT LIKE "%AAL00%")
),

------------------------------------------------------------------------------

-- Acute bacterial meningitis ------------------------------------------------

abm AS (
    SELECT
        P.HADM_ID
        FROM
        PAR_HADM P
    WHERE
        (
            P.Diagnos LIKE "G00%" OR
            P.Diagnos LIKE "A390%" OR
            -- Also include intracranial abcess and "abscess i skalle eller ryggradskanal"
            P.Diagnos LIKE "G06%" OR
            P.Diagnos LIKE "G039%" -- also include Meningitis uns, again, if they are sick enough to be in the icu...
        )
),

------------------------------------------------------------------------------

-- Encephalitis --------------------------------------------------------------

ence AS (
    SELECT
        P.HADM_ID
    FROM
        PAR_HADM P
    WHERE
        (
            P.Diagnos LIKE "G04%" OR
            P.Diagnos LIKE "G05%" OR
            P.Diagnos LIKE "B004%" OR
            P.Diagnos LIKE "B020%" OR
            P.Diagnos LIKE "A841%"
        )
),

------------------------------------------------------------------------------

-- Status epilepticus --------------------------------------------------------

se AS (
    SELECT
        P.HADM_ID
    FROM
        PAR_HADM P
    WHERE
        (
            P.Diagnos LIKE "G41%" OR
            -- also include epilepsy
            P.Diagnos LIKE "G40%"
        )
),

------------------------------------------------------------------------------

-- "Isolated" cervical spine frx ---------------------------------------------

cfx AS (
    SELECT
        P.HADM_ID
    FROM
        PAR_HADM P
    WHERE
        (
            P.Diagnos LIKE 'S12%' OR
            P.Diagnos LIKE 'S13%' OR
            P.Diagnos LIKE 'S14%'
        )
        AND P.Diagnos NOT LIKE '%S06%'
),

------------------------------------------------------------------------------

-- SDH -----------------------------------------------------------------------

-- Turns out there are loads of I62 (non traumatic SDH), many get evacuated, 
-- some have a traumatic sdh as secondary dx... let's pick the "clean"
-- Most get AD005 (evac chron sdh) or AD010 (evac acute sdh). 
-- Not sure it's reasonable to "split" on that.

sdh AS (
    SELECT
        P.HADM_ID
    FROM
        PAR_HADM P
    WHERE
        (
            P.Diagnos LIKE 'I62%'
        )
        AND P.Diagnos NOT LIKE '%S06%'
        --- exclude a few asah
        AND P.Op NOT LIKE "%AAL00%"
        AND P.Op NOT LIKE "%AAC00%"
),

------------------------------------------------------------------------------

-- "Isolated" hydrocephalus (shunt dysfunctions et al) -----------------------

hc AS (
    SELECT
        P.HADM_ID
    FROM
        PAR_HADM P
    WHERE
        (
            P.Diagnos LIKE 'G91%'
        )
    -- several  likely aSAH that fulfill the above criteria will need to be filtered out as such:
    AND (P.Op NOT LIKE "%AAC00%")
    AND (P.Op NOT LIKE "%AAL00%")
),

------------------------------------------------------------------------------

--  Tumours ------------------------------------------------------------------

tum AS (
    SELECT
        P.HADM_ID
    FROM
        PAR_HADM P
    WHERE
        (
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
-- Finds the primary diagnostic group for each entry in PAR_HADM_CONT_DATES. 
-- Note that one HADM could theoretically have several diagnostic criteria fulfilled (although 
-- it is rare since the criteria are designed to define a PRIMARY diagnosis.)
-- In practice, this only happens for patients that are diagnosed with
-- ICH and die within 30 days and have their case of death assigned to ASAH
-- This CTE will try to assign a PAR HADM with one main diagnosis.
-- The CASE WHEN statement will identify the FIRST fulfilled diagnostic criteria only.
-- Also note, that this CTE does not identify ANY secondary diagnosis. 
------------------------------------------------------------------------------

DX AS (
    SELECT DISTINCT
        P.HADM_ID,
        P.CONT_HADM_ID,
        P.LopNr,
        P.Diagnos,
        CASE
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM asah) THEN 'ASAH'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM ane) THEN 'ANE'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM avm) THEN 'AVM'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM ich) THEN 'ICH'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM tbi) THEN 'TBI'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM ais) THEN 'AIS'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM abm) THEN 'ABM'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM cvt) THEN 'CVT'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM ence) THEN 'ENC'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM se) THEN 'SEP'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM cfx) THEN 'CFX'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM sdh) THEN 'SDH'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM hc) THEN 'HC'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM tum) THEN 'TUM'
            ELSE 'OTHER'
        END AS DX_GROUP
    FROM
        PAR_HADM_CONT P
),

------------------------------------------------------------------------------
-- CTE ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX:
-- Create a window where the SIR-PAR matched cohort is joined (on
-- PAR admission ID) with the diagnostic group window (based on PAR dx)
------------------------------------------------------------------------------

ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX AS (
    SELECT
        T.*,
        D.Diagnos,
        D.DX_GROUP
    FROM ICU_ADMISSIONS_MATCHED_WITH_PAR T
    LEFT JOIN DX D ON T.HADM_ID = D.HADM_ID
),

------------------------------------------------------------------------------
-- CTE CONT_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX et al:
-- Create a window where the first SIR admission within a
-- CONT_ICU_ID and its matched PAR admits (with DX_GROUP) are
-- fetched. T_CONT_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX
-- does the same for continious tertiary ICU admissions
------------------------------------------------------------------------------

CONT_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX AS (
    SELECT I.*
    FROM ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX I
    WHERE EXISTS (
        SELECT 1
        FROM FIRST_ADM_WITHIN_CONT_ICU_ADMISSION F
        WHERE F.VtfId_LopNr = I.VtfId_LopNr
    )
),

T_CONT_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX AS (
    SELECT I.*
    FROM ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX I
    WHERE EXISTS (
        SELECT 1
        FROM FIRST_ADM_WITHIN_T_CONT_ICU_ADMISSION F
        WHERE F.VtfId_LopNr = I.VtfId_LopNr
    )
),

------------------------------------------------------------------------------
-- CTE ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME
-- Helps resolving tie situations where one SIR admission is associated with 
-- several PAR admissions. 
-- Örebro is considered a tertiary center from 2014/01/01
------------------------------------------------------------------------------
ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME AS (
    SELECT *,
           ROW_NUMBER() OVER (
               PARTITION BY VtfId_LopNr 
               ORDER BY
                  CASE 
                    WHEN (SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)) OR (SJUKHUS = 55010 AND INDATUM >= 16071) THEN 0 ELSE 1 END,
                  CASE DX_GROUP
                        WHEN 'ASAH' THEN 1
                        WHEN 'ANE' THEN 2
                        WHEN 'AVM' THEN 3
                        WHEN 'ICH' THEN 4
                        WHEN 'TBI' THEN 5
                        WHEN 'AIS' THEN 6
                        WHEN 'ABM' THEN 7
                        WHEN 'CVT' THEN 8
                        WHEN 'ENC' THEN 9
                        WHEN 'SEP' THEN 10
                        WHEN 'CFX' THEN 11
                        WHEN 'SDH' THEN 12
                        WHEN 'HC' THEN 13
                        WHEN 'TUM' THEN 14
                        ELSE 15 -- for any other value not specified
                  END,
                  INDATUM
           ) AS DX_ORDER
    FROM ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX
),

CONT_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_WITH_DX_HIERARCHY_TIME AS (
    SELECT *,
           ROW_NUMBER() OVER (
               PARTITION BY VtfId_LopNr 
               ORDER BY
                  CASE 
                    WHEN SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002) THEN 0 ELSE 1 END,
                  CASE DX_GROUP
                        WHEN 'ASAH' THEN 1
                        WHEN 'ANE' THEN 2
                        WHEN 'AVM' THEN 3
                        WHEN 'ICH' THEN 4
                        WHEN 'TBI' THEN 5
                        WHEN 'AIS' THEN 6
                        WHEN 'ABM' THEN 7
                        WHEN 'CVT' THEN 8
                        WHEN 'ENC' THEN 9
                        WHEN 'SEP' THEN 10
                        WHEN 'CFX' THEN 11
                        WHEN 'SDH' THEN 12
                        WHEN 'HC' THEN 13
                        WHEN 'TUM' THEN 14
                        ELSE 15 -- for any other value not specified
                  END,
                  INDATUM
           ) AS DX_ORDER
    FROM CONT_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX
),

T_CONT_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_WITH_DX_HIERARCHY_TIME AS (
    SELECT *,
           ROW_NUMBER() OVER (
               PARTITION BY VtfId_LopNr 
               ORDER BY
                  CASE 
                    WHEN SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002) THEN 0 ELSE 1 END,
                  CASE DX_GROUP
                        WHEN 'ASAH' THEN 1
                        WHEN 'ANE' THEN 2
                        WHEN 'AVM' THEN 3
                        WHEN 'ICH' THEN 4
                        WHEN 'TBI' THEN 5
                        WHEN 'AIS' THEN 6
                        WHEN 'ABM' THEN 7
                        WHEN 'CVT' THEN 8
                        WHEN 'ENC' THEN 9
                        WHEN 'SEP' THEN 10
                        WHEN 'CFX' THEN 11
                        WHEN 'SDH' THEN 12
                        WHEN 'HC' THEN 13
                        WHEN 'TUM' THEN 14
                        ELSE 15 -- for any other value not specified
                  END,
                  INDATUM
           ) AS DX_ORDER
    FROM T_CONT_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX
),


-- For compatibility reasons and potential future use, the inverse
-- order of priorities is also available (i.e. first ordering based on
-- time and then resolving ties using hierarchy.)
ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_TIME_HIERARCHY AS (
    SELECT *,
           ROW_NUMBER() OVER (
               PARTITION BY CONT_ICU_ID 
               ORDER BY
                  INDATUM,
                  CASE 
                    WHEN SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002) THEN 0 ELSE 1 END,
                  CASE DX_GROUP
                        WHEN 'ASAH' THEN 1
                        WHEN 'ANE' THEN 2
                        WHEN 'AVM' THEN 3
                        WHEN 'ICH' THEN 4
                        WHEN 'TBI' THEN 5
                        WHEN 'AIS' THEN 6
                        WHEN 'ABM' THEN 7
                        WHEN 'CVT' THEN 8
                        WHEN 'ENC' THEN 9
                        WHEN 'SEP' THEN 10
                        WHEN 'CFX' THEN 11
                        WHEN 'SDH' THEN 12
                        WHEN 'HC' THEN 13
                        WHEN 'TUM' THEN 14
                        ELSE 15 -- for any other value not specified
                  END
           ) AS DX_ORDER
    FROM ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX
),

-- Add additional identical CTEs with more appealing (and shorter) names.
ICU_ADM_DX AS(SELECT * FROM ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME),

CONT_ICU_ADM_DX AS(SELECT * FROM CONT_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_WITH_DX_HIERARCHY_TIME),

T_CONT_ICU_ADM_DX AS(SELECT * FROM T_CONT_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_WITH_DX_HIERARCHY_TIME),

-- For each of the three CTEs above, return the identifiers and the didagnosis
ICU_ADM_MAIN_DX AS (SELECT * FROM ICU_ADM_DX WHERE DX_ORDER = 1),

CONT_ICU_ADM_MAIN_DX AS (SELECT * FROM CONT_ICU_ADM_DX WHERE DX_ORDER = 1),

T_CONT_ICU_ADM_MAIN_DX AS (SELECT * FROM T_CONT_ICU_ADM_DX WHERE DX_ORDER = 1)