------------------------------------------------------------------------------
------------------------------------------------------------------------------
---------------------- CREATION OF DIAGNOSES ---------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Please note: All CTE's in general_cte.sql must be run before this script --
------------------------------------------------------------------------------
-- Table of contents (cont'd):
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
--    - T_DX
--    - ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX
--    - ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME
--    - T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX
--    - T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME
-- 5. Resulting CTE's for use
--    - ICU_ADM_DX
--    - TERT_ICU_ADM_DX
------------------------------------------------------------------------------



------------------------------------------------------------------------------
-- Creation of CTE's for each diagnosis
------------------------------------------------------------------------------
-- Each of the diagnosis-groups is subdivided into a CTE called {dx} and a CTE
-- called t_{dx} where the latter only catches admissions to a tertiary centre
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

t_asah AS (
    SELECT
        P.HADM_ID
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
             P.Diagnos LIKE "S12%") AND (P.Diagnos LIKE "%S06%")))
),

t_tbi AS (
    SELECT
        P.HADM_ID
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

t_cvt AS (
    SELECT
        P.HADM_ID
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

t_ich AS (
    SELECT
        P.HADM_ID
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

t_avm AS (
    SELECT
        P.HADM_ID
    FROM
        PAR_HADM P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
        AND (P.Diagnos LIKE "Q28%")
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

t_ais AS (
    SELECT
        P.HADM_ID
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

t_abm AS (
    SELECT
        P.HADM_ID
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

t_ence AS (
    SELECT
        P.HADM_ID
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


t_se AS (
    SELECT
        P.HADM_ID
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

t_cfx AS (
    SELECT
        P.HADM_ID
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

t_sdh AS (
    SELECT
        P.HADM_ID
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

t_hc AS (
    SELECT
        P.HADM_ID
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

t_tum AS (
    SELECT
        P.HADM_ID
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
        P.CONT_HADM_ID,
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
        PAR_HADM_CONT P
),

-- Only for tertiary admissions

T_DX AS (
    SELECT
        P.HADM_ID,
        P.CONT_HADM_ID,
        P.LopNr,
        P.Diagnos,
        CASE
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM t_asah) THEN 'ASAH'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM t_ich) THEN 'ICH'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM t_tbi) THEN 'TBI'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM t_ais) THEN 'AIS'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM t_abm) THEN 'ABM'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM t_cvt) THEN 'CVT'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM t_ence) THEN 'ENC'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM t_se) THEN 'SEP'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM t_avm) THEN 'AVM'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM t_cfx) THEN 'CFX'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM t_sdh) THEN 'SDH'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM t_hc) THEN 'HC'
            WHEN P.HADM_ID IN (SELECT HADM_ID FROM t_tum) THEN 'TUM'
            ELSE 'OTHER'
        END AS DX_GROUP
    FROM
        PAR_HADM_CONT P
    WHERE
        P.SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002)
),

------------------------------------------------------------------------------
-- CTE ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX:
-- Create a window where the SIR-PAR matched cohort is joined (on
-- PAR admission ID) with the diagnostic group window (based on PAR dx)
------------------------------------------------------------------------------
ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX AS (
    SELECT
        T.VtfId_LopNr,
        T.HADM_ID,
        P.CONT_HADM_ID,
        T.LopNr,
        T.InskrTidpunkt,
        T.UtskrTidpunkt,
        T.AvdNamn,
        T.INDATUM,
        T.UTDATUM,
        T.MVO,
        T.SJUKHUS,
        T.SjukhusTyp,
        P.DIAGNOS,
        P.OP,
        D.DX_GROUP,
        T.CONT_ICU_ID
    FROM ICU_ADMISSIONS_MATCHED_WITH_PAR T
    LEFT JOIN DX D ON T.HADM_ID = D.HADM_ID
    LEFT JOIN PAR_HADM_CONT P ON T.HADM_ID = P.HADM_ID
),

-- 
T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX AS (
    SELECT
        T.VtfId_LopNr,
        T.HADM_ID,
        P.CONT_HADM_ID,
        T.LopNr,
        T.InskrTidpunkt,
        T.UtskrTidpunkt,
        T.AvdNamn,
        T.INDATUM,
        T.UTDATUM,
        T.MVO,
        T.SJUKHUS,
        P.DIAGNOS,
        P.OP,
        D.DX_GROUP,
        T.T_CONT_ICU_ID
    FROM T_ICU_ADMISSIONS_MATCHED_WITH_PAR T
    LEFT JOIN T_DX D ON T.HADM_ID = D.HADM_ID
    LEFT JOIN PAR_HADM_CONT P ON T.HADM_ID = P.HADM_ID
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
ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME AS (
    SELECT *,
           ROW_NUMBER() OVER (
               PARTITION BY CONT_ICU_ID 
               ORDER BY
                  CASE 
                    WHEN SJUKHUS IN (11001, 11003, 51001, 21001, 64001, 12001, 41001, 41002) THEN 0 ELSE 1 END,
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
    FROM ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX
),

T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME AS (
    SELECT *,
           ROW_NUMBER() OVER (
               PARTITION BY T_CONT_ICU_ID 
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
    FROM ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX
),

T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_TIME_HIERARCHY AS (
    SELECT *,
           ROW_NUMBER() OVER (
               PARTITION BY T_CONT_ICU_ID 
               ORDER BY
                        INDATUM,
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
),

-- Add additional identical CTE's with more appealing (and shorter) names.
-- The old CTE's are kept to retain backwards compatibility

ICU_ADM_DX AS(SELECT * FROM ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME),

TERT_ICU_ADM_DX AS(SELECT * FROM T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME)