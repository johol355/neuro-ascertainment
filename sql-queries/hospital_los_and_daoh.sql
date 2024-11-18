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
        S.AvdNamn,
        P.INDATUM,
        P.UTDATUM,
        P.HADM_ID,
        P.Diagnos,
        P.Op,
        P.MVO,
        P.Sjukhus
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

-- WITHIN VtfId_LopNr, merge dates for all groups of par admissions. Also HOSPITAL_LOS is calculated for the respective ADM_GROUP.
-- To clean up cases where patients die before PAR discharge, DORS is joined and the MIN(discharge date, dod) is chosen,
-- thus if a patient dies before their discharge their day of death will be chosen for the calculaton of hospital LOS.
DAOH_90_STEP_3 AS (
    SELECT 
        D.VtfId_LopNr,
        D.LopNr,
        D.InskrTidPunkt,
        D.AvdNamn,
        D.S_INDATUM,
        D.S_INDATUM_90,
        D.INDATUM,
        D.UTDATUM,
        D.HADM_ID,
        D.UTDATUM - D.S_INDATUM + 1 as HOSPITAL_LOS,
        MIN(
            D.UTDATUM,
            (julianday(SUBSTR(DO.DODSDAT, 1, 4) || '-' || 
                  SUBSTR(DO.DODSDAT, 5, 2) || '-' || 
                  SUBSTR(DO.DODSDAT, 7, 2)) - julianday('1970-01-01')))
                  AS PROPER_UTDATUM,
        (julianday(SUBSTR(DO.DODSDAT, 1, 4) || '-' || 
                  SUBSTR(DO.DODSDAT, 5, 2) || '-' || 
                  SUBSTR(DO.DODSDAT, 7, 2)) - julianday('1970-01-01')) AS dod,
        MIN(
            D.UTDATUM,
            (julianday(SUBSTR(DO.DODSDAT, 1, 4) || '-' || 
                  SUBSTR(DO.DODSDAT, 5, 2) || '-' || 
                  SUBSTR(DO.DODSDAT, 7, 2)) - julianday('1970-01-01')))
                  AS PROPER_UTDATUM,
        (julianday(SUBSTR(DO.DODSDAT, 1, 4) || '-' || 
                  SUBSTR(DO.DODSDAT, 5, 2) || '-' || 
                  SUBSTR(DO.DODSDAT, 7, 2)) - julianday('1970-01-01')) - D.S_INDATUM + 1 AS HOSPITAL_LOS_ALIVE,
        D.ADM_GROUP,
        MAX(MIN(D.INDATUM), D.S_INDATUM) AS min_INDATUM,
        MIN(MAX(D.UTDATUM), D.S_INDATUM_90) AS max_UTDATUM,     -- Cap max_UTDATUM at S_INDATUM_90
        MIN(MAX(D.UTDATUM), D.S_INDATUM_90) - MAX(MIN(D.INDATUM), D.S_INDATUM) + 1 as ADMITTED_DAYS,
        D.Diagnos,
        D.Op,
        D.MVO,
        D.Sjukhus
    FROM DAOH_90_STEP_2 D
    LEFT JOIN DORS DO ON D.LopNr = DO.LopNr
    GROUP BY VtfId_LopNr, ADM_GROUP
),

-- A CTE that gets the Hospital LOS for the relevant ADM GROUP above, i.e. the Hospital LOS from the ICU admission at hand
H_LOS AS (
    SELECT
        VtfId_LopNr,
        LopNr,
        HOSPITAL_LOS,
        HOSPITAL_LOS_ALIVE
    FROM DAOH_90_STEP_3
    WHERE ADM_GROUP == 1  
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
)