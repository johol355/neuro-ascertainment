-- Filters out ICU admissions from non-NSICU specialized ICUs (mainly CTICUs) and within a timeframe
T_CONT_ICU_ADM_MAIN_DX_PRUNED AS (
    SELECT T.*,
           (ROW_NUMBER() OVER (PARTITION BY T.LopNr ORDER BY T.InskrTidpunkt ASC)) AS ADM_ORDER
    FROM T_CONT_ICU_ADM_MAIN_DX T
    WHERE AvdNamn NOT IN ('KS/THIVA', 'KS ECMO', 'Astrid Lindgren', 'Uppsala BRIVA', 'Uppsala TIVA', 'Uppsala BIVA', 'SU/TIVA', 'Linköping BRIVA', 'Lund - BIVA', 'Umeå - Thorax')
    --AND T.InskrTidpunkt <= 1719791940 -- ICU admits prior 2024-06-30 23:59 (to allow for 180 DAOH)
    --AND T.InskrTidpunkt >= 1483228800 -- ICU admits after 2017-01-01 00:00 (1451606400 would be 1/1/2016)
    AND T.DX_GROUP IN ("ASAH", "TBI", "AIS", "ICH", "ABM", "SDH")
),

T_CONT_ICU_ADM_MAIN_DX_PRUNED_FIRST AS (
    SELECT *
    FROM T_CONT_ICU_ADM_MAIN_DX_PRUNED
    WHERE ADM_ORDER = 1
),

-- Identifies ER visits at hospitals other than the ICU hospital within ±1 day of ICU admission.
OSH_ER AS (
    SELECT I.VtfId_LopNr,
           I.T_CONT_ICU_ID,
           I.CONT_ICU_ID,
           I.LopNr,
           I.AvdNamn,
           I.InskrTidpunkt / 86400 AS IVA_INDATUM,
           datetime(I.InskrTidpunkt, 'unixepoch') as IVA_INTID,
           P.OV_ID,
           P.INDATUM,
           P.SJUKHUS,
           CASE
                WHEN P.SJUKHUS IS NULL THEN NULL
                WHEN ( I.AvdNamn IN ('S-CIVA', 'S-NIVA', 'KS/THIVA', 'KS ECMO', 'Astrid Lindgren') AND P.SJUKHUS NOT IN (11001, 11003) ) OR
                ( I.AvdNamn IN ('Umeå IVA', 'Umeå - Thorax') AND P.SJUKHUS NOT IN (64001) ) OR
                ( I.AvdNamn IN ('Uppsala', 'Uppsala BRIVA', 'Uppsala TIVA', 'Uppsala BIVA','Uppsala NIVA') AND P.SJUKHUS NOT IN (12001) ) OR
                ( I.AvdNamn IN ('SU/NIVA', 'SU/CIVA', 'SU/TIVA') AND P.SJUKHUS NOT IN (51001) ) OR
                ( I.AvdNamn IN ('Linköping', 'Linköping NIVA', 'Linköping BRIVA') AND P.SJUKHUS NOT IN (21001) ) OR
                ( I.AvdNamn IN ('IVA Lund', 'Lund - BIVA', 'Lund - NIVA') AND P.SJUKHUS NOT IN (41001, 41002) ) OR
                ( I.AvdNamn IN ('IVAUSÖ') AND P.SJUKHUS NOT IN (55010) )
                THEN 1 ELSE 0 END AS OSH_ER
    FROM T_CONT_ICU_ADM_MAIN_DX_PRUNED_FIRST I
    LEFT JOIN PAR_OUTPATIENT_UNPLANNED P ON I.LopNr = P.LopNr
    WHERE (
        (I.InskrTidpunkt / 86400 - P.INDATUM IN (-1,0,1)) -- Allow for the ER visit to be the calendar day before or after the ICU admit
    )
),

-- Identifies inpatient hospital admissions (HADM) at hospitals other than the ICU hospital overlapping the ICU admission date.
OSH_HADM AS (
    SELECT 
        I.VtfId_LopNr,
        I.T_CONT_ICU_ID,
        I.CONT_ICU_ID,
        I.LopNr,
        I.AvdNamn,
        I.InskrTidpunkt / 86400 AS IVA_INDATUM,
        datetime(I.InskrTidpunkt, 'unixepoch') as IVA_INTID,
        H.HADM_ID,
        H.INDATUM,
        H.UTDATUM,
        H.SJUKHUS,
        CASE
            WHEN H.SJUKHUS IS NULL THEN NULL
            WHEN (
                (I.AvdNamn IN ('S-CIVA', 'S-NIVA', 'KS/THIVA', 'KS ECMO', 'Astrid Lindgren') AND H.SJUKHUS NOT IN (11001, 11003)) OR
                (I.AvdNamn IN ('Umeå IVA', 'Umeå - Thorax') AND H.SJUKHUS NOT IN (64001)) OR
                (I.AvdNamn IN ('Uppsala', 'Uppsala BRIVA', 'Uppsala TIVA', 'Uppsala BIVA','Uppsala NIVA') AND H.SJUKHUS NOT IN (12001)) OR
                (I.AvdNamn IN ('SU/NIVA', 'SU/CIVA', 'SU/TIVA') AND H.SJUKHUS NOT IN (51001)) OR
                (I.AvdNamn IN ('Linköping', 'Linköping NIVA', 'Linköping BRIVA') AND H.SJUKHUS NOT IN (21001)) OR
                (I.AvdNamn IN ('IVA Lund', 'Lund - BIVA', 'Lund - NIVA') AND H.SJUKHUS NOT IN (41001, 41002)) OR
                (I.AvdNamn IN ('IVAUSÖ') AND H.SJUKHUS NOT IN (55010))
            )
            THEN 1
            ELSE 0
        END AS OSH_HADM
    FROM T_CONT_ICU_ADM_MAIN_DX_PRUNED_FIRST I
    LEFT JOIN PAR_HADM H 
        ON I.LopNr = H.LopNr
    WHERE (
            I.InskrTidpunkt / 86400 BETWEEN (H.INDATUM - 1) AND (H.UTDATUM + 1)
        )
),

-- Flags ICU admissions with ongoing hospital stays at the same hospital beginning more than one calendar day prior to ICU admission.
SAME_HOSP_PREVIOUSLY_ADMITTED AS (
    SELECT 
        I.T_CONT_ICU_ID,
        I.LopNr,
        I.AvdNamn,
        H.SJUKHUS,
        H.INDATUM,
        H.UTDATUM,
        I.InskrTidpunkt / 86400 AS ICU_DAY,
        CASE
            WHEN I.InskrTidpunkt / 86400 BETWEEN (H.INDATUM - 1) AND (H.UTDATUM + 1) -- Check if ongoing PAR admit
                 AND (I.InskrTidpunkt / 86400) - H.INDATUM >= 2 -- Check if the PAR admit started more than 1 calendar day ago
                 AND (
                     (I.AvdNamn IN ('S-CIVA', 'S-NIVA', 'KS/THIVA', 'KS ECMO', 'Astrid Lindgren') AND H.SJUKHUS IN (11001, 11003)) OR
                     (I.AvdNamn IN ('Umeå IVA', 'Umeå - Thorax') AND H.SJUKHUS = 64001) OR
                     (I.AvdNamn IN ('Uppsala', 'Uppsala BRIVA', 'Uppsala TIVA', 'Uppsala BIVA', 'Uppsala NIVA') AND H.SJUKHUS = 12001) OR
                     (I.AvdNamn IN ('SU/NIVA', 'SU/CIVA', 'SU/TIVA') AND H.SJUKHUS = 51001) OR
                     (I.AvdNamn IN ('Linköping', 'Linköping NIVA', 'Linköping BRIVA') AND H.SJUKHUS = 21001) OR
                     (I.AvdNamn IN ('IVA Lund', 'Lund - BIVA', 'Lund - NIVA') AND H.SJUKHUS IN (41001, 41002)) OR
                     (I.AvdNamn IN ('IVAUSÖ') AND H.SJUKHUS = 55010)
                 ) -- check if the PAR admit is at the same hospital
            THEN 1 -- flag 1 if there is a "long ongoing stay"
            ELSE 0 -- else 0
        END AS PRE_ICU_SAME_HOSP_HADM_FLAG
    FROM T_CONT_ICU_ADM_MAIN_DX_PRUNED_FIRST I
    LEFT JOIN PAR_HADM H 
        ON I.LopNr = H.LopNr
),

-- Flags ICU admissions with any ongoing continuous hospital admission beginning at least two days prior and overlapping the ICU admission date.
ANY_LONG_CONT_HADM AS (
    SELECT
     I.T_CONT_ICU_ID,
        I.LopNr,
        I.AvdNamn,
        I.InskrTidpunkt / 86400 AS ICU_DAY,
        P.CONT_HADM_ID,
        P.CONT_HADM_ADM_DATE,
        P.CONT_HADM_DSC_DATE,
        CASE WHEN ( (I.InskrTidpunkt / 86400 BETWEEN (P.CONT_HADM_ADM_DATE - 1) AND (P.CONT_HADM_DSC_DATE + 1))
            AND ((I.InskrTidpunkt / 86400) - CONT_HADM_ADM_DATE >= 2) )
            THEN 1 ELSE 0 END AS ANY_LONG_CONT_HADM_FLAG
    FROM T_CONT_ICU_ADM_MAIN_DX_PRUNED_FIRST I
    LEFT JOIN PAR_HADM_CONT_DATES P ON I.LopNr = P.LopNr
),

-- Flags ICU admissions immediately preceded by another ICU stay from a different ICU within ±24 hours.
OSH_ICU AS (
    SELECT 
        I.T_CONT_ICU_ID,
        I.VtfId_LopNr,
        I.LopNr,
        I.HADM_ID,
        I.AvdNamn AS CURR_AvdNamn,
        I.InskrTidpunkt AS CURR_ICU_IN,
        B.InskrTidpunkt,
        B.UtskrTidpunkt,
        B.AvdNamn,
        B.VtfId_LopNr as Primary_VtfId_LopNr,
        CASE
            WHEN B.UtskrTidpunkt BETWEEN I.InskrTidpunkt - 86400 AND I.InskrTidpunkt + 3600
            AND (
                (I.AvdNamn IN ('S-CIVA', 'S-NIVA', 'KS/THIVA', 'KS ECMO', 'Astrid Lindgren') AND B.AvdNamn NOT IN ('S-CIVA', 'S-NIVA', 'KS/THIVA', 'KS ECMO', 'Astrid Lindgren')) OR
                (I.AvdNamn IN ('Umeå IVA', 'Umeå - Thorax') AND B.AvdNamn NOT IN ('Umeå IVA', 'Umeå - Thorax')) OR
                (I.AvdNamn IN ('Uppsala', 'Uppsala BRIVA', 'Uppsala TIVA', 'Uppsala BIVA','Uppsala NIVA') AND B.AvdNamn NOT IN ('Uppsala', 'Uppsala BRIVA', 'Uppsala TIVA', 'Uppsala BIVA','Uppsala NIVA')) OR
                (I.AvdNamn IN ('SU/NIVA', 'SU/CIVA', 'SU/TIVA') AND B.AvdNamn NOT IN ('SU/NIVA', 'SU/CIVA', 'SU/TIVA')) OR
                (I.AvdNamn IN ('Linköping', 'Linköping NIVA', 'Linköping BRIVA') AND B.AvdNamn NOT IN ('Linköping', 'Linköping NIVA', 'Linköping BRIVA')) OR
                (I.AvdNamn IN ('IVA Lund', 'Lund - BIVA', 'Lund - NIVA') AND B.AvdNamn NOT IN ('IVA Lund', 'Lund - BIVA', 'Lund - NIVA')) OR
                (I.AvdNamn IN ('IVAUSÖ') AND B.AvdNamn NOT IN ('IVAUSÖ'))
            )
            THEN 1 ELSE 0
        END AS OSH_ICU_FLAG
    FROM T_CONT_ICU_ADM_MAIN_DX_PRUNED_FIRST I
    LEFT JOIN SIR_BASDATA B 
        ON I.LopNr = B.LopNr
),

OSH_ICU_RANKED AS (
    SELECT 
        *,
        ROW_NUMBER() OVER (
            PARTITION BY T_CONT_ICU_ID
            ORDER BY 
                OSH_ICU_FLAG DESC,      -- Prioritize rows with OSH_ICU_FLAG = 1
                InskrTidpunkt DESC      -- Among those, choose the latest InskrTidpunkt
        ) AS rn
    FROM OSH_ICU
),

-- Flags if the preceding (conceptually the pre-transfer) ICU admission, in case there is one, is preceded by a long hospital stay
OSH_ICU_LONG_PREVIOUS_HADM AS (
    SELECT I.VtfId_LopNr,
           I.T_CONT_ICU_ID,
           I.LopNr,
           I.AvdNamn,
           I.InskrTidpunkt,
           I.UtskrTidpunkt,
           I.CURR_AvdNamn AS T_ICU,
           I.CURR_ICU_IN AS T_ICU_INTIME
    FROM OSH_ICU I
    WHERE I.OSH_ICU_FLAG = 1
),

-- The FLAG CTEs returns wether or not a T_CONT_ICU_ID is flagged according to a given rule
OSH_ER_FLAG AS (
    SELECT
        T_CONT_ICU_ID,
        MAX(OSH_ER) AS OSH_ER_FLAG
    FROM OSH_ER
    GROUP BY T_CONT_ICU_ID
),

OSH_HADM_FLAG AS (
    SELECT
        T_CONT_ICU_ID,
        MAX(OSH_HADM) AS OSH_HADM_FLAG
    FROM OSH_HADM
    GROUP BY T_CONT_ICU_ID
),

OSH_ICU_FLAG AS (
    SELECT
        T_CONT_ICU_ID,
        OSH_ICU_FLAG,
        Primary_VtfId_LopNr
    FROM OSH_ICU_RANKED
    WHERE rn = 1
),

PRE_ICU_SAME_HOSP_HADM_FLAG AS (
    SELECT
        T_CONT_ICU_ID,
        MAX(PRE_ICU_SAME_HOSP_HADM_FLAG) AS PRE_ICU_SAME_HOSP_HADM_FLAG
    FROM SAME_HOSP_PREVIOUSLY_ADMITTED
    GROUP BY T_CONT_ICU_ID
),

ANY_LONG_CONT_HADM_FLAG AS (
    SELECT
        T_CONT_ICU_ID,
        MAX(ANY_LONG_CONT_HADM_FLAG) AS ANY_LONG_CONT_HADM_FLAG
    FROM ANY_LONG_CONT_HADM
    GROUP BY T_CONT_ICU_ID
),

-- Final aggregation combining all prior flags to classify ICU admissions as direct acute transfers or as having any out-of-system hospitalization (OSH).
Q AS (
    SELECT 
        I.T_CONT_ICU_ID,
        I.LopNr,
        I.DX_GROUP,
        I.AvdNamn as Tertiary_ICU_name,
        I.InskrTidpunkt AS TERTIARY_ICU_ADM_TIME,
        I.InskrTidpunkt / 86400 AS TERTIARY_ICU_ADM_DATE,
        I.VtfId_LopNr,
        I.HADM_ID,
        P.Alder as age,
        CASE WHEN P.Kon = '1' THEN 0 ELSE 1 END AS sex_female,
        P.Sjukhus,
        ER.OSH_ER_FLAG,
        HO.OSH_HADM_FLAG,
        ICU.OSH_ICU_FLAG,
        ICU.Primary_VtfId_LopNr,
        S.InskrTidpunkt AS PRIMARY_ICU_ADM_TIME,
        PRE.PRE_ICU_SAME_HOSP_HADM_FLAG,
        LON.ANY_LONG_CONT_HADM_FLAG,
        ST.sir_consciousness_level AS tertiary_sir_consciousness_level,
        ST.SAPS_unconcious AS tertiary_SAPS_unconcious,
        ST.SAPS_obtunded AS tertiary_SAPS_obtunded,
        ST.SAPS_total_score AS tertiary_SAPS_total_score,
        ST.SAPS_AMV AS tertiary_SAPS_AMV,
        ST.SAPS_hypotension AS tertiary_SAPS_hypotension,
        ST.SAPS_acidosis AS tertiary_SAPS_acidosis,
        ST.SAPS_hypothermia AS tertiary_SAPS_hypothermia,
        ST.SAPS_max_temp AS tertiary_SAPS_max_temp,
        ST.SAPS_hypertension AS tertiary_SAPS_hypertension,
        ST.SAPS_min_SBP as tertiary_SAPS_min_SBP,
        ST.SAPS_max_HR as tertiary_SAPS_max_HR,
        SP.sir_consciousness_level AS primary_sir_consciousness_level,
        SP.SAPS_unconcious AS primary_SAPS_unconcious,
        SP.SAPS_obtunded AS primary_SAPS_obtunded,
        SP.SAPS_total_score AS primary_SAPS_total_score,
        SP.SAPS_AMV AS primary_SAPS_AMV,
        SP.SAPS_hypotension AS primary_SAPS_hypotension,
        SP.SAPS_acidosis AS primary_SAPS_acidosis,
        SP.SAPS_hypothermia AS primary_SAPS_hypothermia,
        SP.SAPS_max_temp AS primary_SAPS_max_temp,
        SP.SAPS_hypertension AS primary_SAPS_hypertension,
        SP.SAPS_min_SBP as primary_SAPS_min_SBP,
        SP.SAPS_max_HR as primary_SAPS_max_HR,
        SAPS.SAPS3_TidPaSjukhus,
        SAPS.SAPS3_Vardplats,
        SAPS.SAPS3_OperationsTyp
        DAOH90.DAOH_90,
        DAOH180.DAOH_180,
        DORS.DODSDAT_ROUND_UP
    FROM T_CONT_ICU_ADM_MAIN_DX_PRUNED_FIRST I
    LEFT JOIN OSH_ER_FLAG ER ON I.T_CONT_ICU_ID = ER.T_CONT_ICU_ID
    LEFT JOIN OSH_HADM_FLAG HO ON I.T_CONT_ICU_ID = HO.T_CONT_ICU_ID
    LEFT JOIN OSH_ICU_FLAG ICU ON I.T_CONT_ICU_ID = ICU.T_CONT_ICU_ID
    LEFT JOIN PRE_ICU_SAME_HOSP_HADM_FLAG PRE ON I.T_CONT_ICU_ID = PRE.T_CONT_ICU_ID
    LEFT JOIN ANY_LONG_CONT_HADM_FLAG LON ON I.T_CONT_ICU_ID = LON.T_CONT_ICU_ID
    LEFT JOIN DESCRIPTIVE_SIR ST ON I.VtfId_LopNr = ST.VtfId_LopNr
    LEFT JOIN DESCRIPTIVE_SIR SP ON ICU.Primary_VtfId_LopNr = SP.VtfId_LopNr
    LEFT JOIN PAR_HADM P ON I.HADM_ID = P.HADM_ID
    LEFT JOIN DAOH_90 DAOH90 ON I.VtfId_LopNr = DAOH90.VtfId_LopNr
    LEFT JOIN DAOH_180 DAOH180 ON I.VtfId_LopNr = DAOH180.VtfId_LopNr
    LEFT JOIN PROCESSED_DORS DORS ON I.LopNr = DORS.LopNr 
    LEFT JOIN SIR_BASDATA S ON ICU.Primary_VtfId_LopNr = S.VtfId_LopNr
    LEFT JOIN SIR_SAPS3 SAPS ON ICU.Primary_VtfId_LopNr = SAPS.VtfId_LopNr
)