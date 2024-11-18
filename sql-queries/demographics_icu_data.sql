--------------------------------------------------------------------------------
-- DESCRIPTIVE_SIR collects data from several SIR tables (including SAPS, SOFA) 
-- joined on SIR admission ID
--------------------------------------------------------------------------------

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
        JULIANDAY(strftime('%Y-%m-%d', substr(DO.DODSDAT, 1, 4) || '-' || substr(DO.DODSDAT, 5, 2) || '-' || substr(DO.DODSDAT, 7, 2) || ' 00:00:00')) - JULIANDAY(date(S.InskrTidPunkt, 'unixepoch')) AS days_alive,
        DO.DODSDAT AS death_date
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

--------------------------------------------------------------------------------
-- DESCRIPTIVE_PAR collects data from PAR and DORS (data on death date) on PAR_HADM id and in
-- the case of death date data on patient ID + admission date in PAR.
--------------------------------------------------------------------------------

DESCRIPTIVE_PAR AS (
    SELECT
        P.HADM_ID,
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
    FROM PAR_HADM P
),

--------------------------------------------------------------------------------
-- CONT_DESCRIPTIVE_SIR condenses DESCRIPTIVE_SIR for administrative 
-- ICU-admissions to "real" continuous admissions
--------------------------------------------------------------------------------


-------------


--DEBUG: T VS ALL!!!



----------
-- For "first" occurences create a CTE with row-nubers ordered by adm-time
CONT_DESCRIPTIVE_SIR_RN AS(
  SELECT 
    T.CONT_ICU_ID,
    S.VtfId_LopNr,
    S.sir_adm_time,
    ROW_NUMBER() OVER (PARTITION BY CONT_ICU_ID ORDER BY sir_adm_time) AS rn
  FROM DESCRIPTIVE_SIR S
  LEFT JOIN T_ICU_ADM_CONT T ON S.VtfId_LopNr = T.VtfId_LopNr
  WHERE CONT_ICU_ID IS NOT NULL
),

-- Use the CTE created to index first occurrence
CONT_DESCRIPTIVE_SIR_FIRST AS(
  SELECT 
    T.CONT_ICU_ID,
    S.sir_adm_time,
    S.admission_height,
    S.admission_weight,
    S.BMI,
    S.sir_consciousness_level,
    S.SAPS_AMV,
    S.SAPS_PFI,
    S.SAPS_hypoxia,
    S.SAPS_PAO2,
    S.ARDS AS SAPS_ARDS,
    S.SAPS_min_SBP,
    S.SAPS_max_HR,
    S.SAPS_tachycardia,
    S.SAPS_bradycardia,
    S.SAPS_hypotension,
    S.SAPS_hypertension,
    S.SAPS_total_score,
    S.SAPS_min_pH,
    S.SAPS_max_temp,
    S.SAPS_acidosis,
    S.SAPS_hypothermia,
    RN.rn
  FROM DESCRIPTIVE_SIR S
  LEFT JOIN T_ICU_ADM_CONT T ON S.VtfId_LopNr = T.VtfId_LopNr
  LEFT JOIN CONT_DESCRIPTIVE_SIR_RN RN ON S.VtfId_LopNr = RN.VtfId_LopNr
  WHERE RN.rn = 1
),

-- Add on last occurrence for time variable
CONT_DESCRIPTIVE_SIR_LAST AS(
  SELECT
    T.CONT_ICU_ID,
    MAX(S.sir_dsc_time) AS sir_dsc_time
  FROM DESCRIPTIVE_SIR S
  LEFT JOIN T_ICU_ADM_CONT T ON S.VtfId_LopNr = T.VtfId_LopNr
  WHERE CONT_ICU_ID IS NOT NULL
  GROUP BY CONT_ICU_ID
),

-- Join the sheets to a single CTE for condensed continuous ICU-admissions
CONT_DESCRIPTIVE_SIR AS(
  SELECT F.*, L.sir_dsc_time
  FROM CONT_DESCRIPTIVE_SIR_FIRST F
  LEFT JOIN CONT_DESCRIPTIVE_SIR_LAST L ON F.CONT_ICU_ID = L.CONT_ICU_ID
)