---------------------------------------------------------
---------------------------------------------------------
--------- SUMMARY TABLES --------------------------------
---------------------------------------------------------
---------------------------------------------------------

-- CONT_ICU_SUMMARY is an ICU-summary where data has been grouped for continuous icu-admissions

CONT_ICU_SUMMARY AS (
SELECT 
    MIN(T.LopNr) AS LopNr,
    T.CONT_ICU_ID,
    MIN(D.sir_adm_time) AS sir_adm_time,
    MAX(D.sir_dsc_time) AS sir_dsc_time,
    MIN(C.CONT_HADM_ID) AS cont_hadm_id,
    MIN(C.CONT_HADM_ADM_DATE) AS cont_hadm_adm_date,
    MAX(C.CONT_HADM_DSC_DATE) AS cont_hadm_dsc_date,
    MIN(T.DX_GROUP) AS dx,
    MIN(L.HOSPITAL_LOS) AS hospital_los
FROM ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_TIME_HIERARCHY T
LEFT JOIN DESCRIPTIVE_SIR D ON T.VtfId_LopNr = D.VtfId_LopNr
LEFT JOIN H_LOS L ON T.VtfId_LopNr = L.VtfId_LopNr
LEFT JOIN PAR_HADM_CONT_DATES C ON C.HADM_ID = T.HADM_ID
WHERE DX_ORDER = 1
GROUP BY CONT_ICU_ID
),

-- The SUMMARY_TABLE joins ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_TIME_HIERARCHY containing SIR admission ID, PAR data and DX
-- with DESCRIPTIVE_PAR and DESCRIPTIVE_SIR. Finally, only one PAR admission is matched with each
-- ICU admission based on the highest ranking DX (earliest + DX hierarchy). If a patient has multiple ICU admissions only the first is kept.
SUMMARY_TABLE AS (
SELECT
    T.LopNr,
    D.VtfId_LopNr,
    P.HADM_ID,
    P.par_tertiary_center,
    T.INDATUM AS par_adm_date,
    T.UTDATUM AS par_dsc_date,
    P.sex_female,
    P.age,
    T.DX_GROUP,
    T.DX_ORDER,
    D.*,
    L.HOSPITAL_LOS,
    L.HOSPITAL_LOS_ALIVE,
    DA90.DAOH_90,
    DA180.DAOH_180,
    C.CONT_HADM_ID,
    C.CONT_HADM_ADM_DATE,
    C.CONT_HADM_DSC_DATE,
    T.CONT_ICU_ID
FROM ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_TIME_HIERARCHY T
LEFT JOIN DESCRIPTIVE_PAR P ON T.HADM_ID = P.HADM_ID
LEFT JOIN DESCRIPTIVE_SIR D ON T.VtfId_LopNr = D.VtfId_LopNr
LEFT JOIN DAOH_180 DA180 ON T.VtfId_LopNr = DA180.VtfId_LopNr
LEFT JOIN DAOH_90 DA90 ON T.VtfId_LopNr = DA90.VtfId_LopNr
LEFT JOIN H_LOS L ON T.VtfId_LopNr = L.VtfId_LopNr
LEFT JOIN PAR_HADM_CONT_DATES C ON C.HADM_ID = T.HADM_ID
WHERE DX_ORDER = 1
),

----------------------------------------------------------------
-- Extra CTE's for using dx-hierarchy before time and vice-versa
----------------------------------------------------------------

-- Time has priority over dx-hierarchy

SUMMARY_TABLE_TIME_HIERARCHY AS (
SELECT
    T.LopNr,
    D.VtfId_LopNr,
    P.HADM_ID,
    P.par_tertiary_center,
    T.INDATUM AS par_adm_date,
    T.UTDATUM AS par_dsc_date,
    P.sex_female,
    P.age,
    T.DX_GROUP,
    T.DX_ORDER,
    D.*,
    L.HOSPITAL_LOS,
    L.HOSPITAL_LOS_ALIVE,
    DA90.DAOH_90,
    DA180.DAOH_180,
    C.CONT_HADM_ID,
    C.CONT_HADM_ADM_DATE,
    C.CONT_HADM_DSC_DATE,
    T.CONT_ICU_ID
FROM ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_TIME_HIERARCHY T
LEFT JOIN DESCRIPTIVE_PAR P ON T.HADM_ID = P.HADM_ID
LEFT JOIN DESCRIPTIVE_SIR D ON T.VtfId_LopNr = D.VtfId_LopNr
LEFT JOIN DAOH_180 DA180 ON T.VtfId_LopNr = DA180.VtfId_LopNr
LEFT JOIN DAOH_90 DA90 ON T.VtfId_LopNr = DA90.VtfId_LopNr
LEFT JOIN H_LOS L ON T.VtfId_LopNr = L.VtfId_LopNr
LEFT JOIN PAR_HADM_CONT_DATES C ON C.HADM_ID = T.HADM_ID
WHERE DX_ORDER = 1
),

-- Dx-hierarchy has priority over time

SUMMARY_TABLE_HIERARCHY_TIME AS (
SELECT
    T.LopNr,
    D.VtfId_LopNr,
    P.HADM_ID,
    P.par_tertiary_center,
    T.INDATUM AS par_adm_date,
    T.UTDATUM AS par_dsc_date,
    P.sex_female,
    P.age,
    T.DX_GROUP,
    T.DX_ORDER,
    D.*,
    L.HOSPITAL_LOS,
    L.HOSPITAL_LOS_ALIVE,
    DA90.DAOH_90,
    DA180.DAOH_180,
    C.CONT_HADM_ID,
    C.CONT_HADM_ADM_DATE,
    C.CONT_HADM_DSC_DATE,
    T.CONT_ICU_ID,
    T.T_CONT_ICU_ID
FROM ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME T
LEFT JOIN DESCRIPTIVE_PAR P ON T.HADM_ID = P.HADM_ID
LEFT JOIN DESCRIPTIVE_SIR D ON T.VtfId_LopNr = D.VtfId_LopNr
LEFT JOIN DAOH_180 DA180 ON T.VtfId_LopNr = DA180.VtfId_LopNr
LEFT JOIN DAOH_90 DA90 ON T.VtfId_LopNr = DA90.VtfId_LopNr
LEFT JOIN H_LOS L ON T.VtfId_LopNr = L.VtfId_LopNr
LEFT JOIN PAR_HADM_CONT_DATES C ON C.HADM_ID = T.HADM_ID
WHERE DX_ORDER = 1
),

-- SUMMARY_TABLE_FIRST_ADM is an identical CTE to SUMMARY_TABLE with one importande difference:
-- Here ONLY the first admission to a tertiary centre is retained (i.e. only MIN(sir_adm_time) is used).
-- This will result in a dataset in which patients are unable to be repeatedly admitted for anything if
-- they have been previously treated in an NSICU and also means only the initial ICU-stay is retained (which
-- is usually shorter if the patient has been admitted to a general ICU for a couple of hours and then
-- subsequently is admitted to a NSICU for several days.)
SUMMARY_TABLE_FIRST_ADM_TERTIARY AS (
SELECT
    T.LopNr,
    D.VtfId_LopNr,
    P.HADM_ID,
    P.par_tertiary_center,
    T.INDATUM AS par_adm_date,
    T.UTDATUM AS par_dsc_date,
    P.sex_female,
    P.age,
    T.DX_GROUP,
    T.DX_ORDER,
    D.*,
    L.HOSPITAL_LOS,
    L.HOSPITAL_LOS_ALIVE,
    DA90.DAOH_90,
    DA180.DAOH_180
FROM T_ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_TIME_HIERARCHY T
LEFT JOIN DESCRIPTIVE_PAR P ON T.HADM_ID = P.HADM_ID
LEFT JOIN DESCRIPTIVE_SIR D ON T.VtfId_LopNr = D.VtfId_LopNr
LEFT JOIN DAOH_180 DA180 ON T.VtfId_LopNr = DA180.VtfId_LopNr
LEFT JOIN DAOH_90 DA90 ON T.VtfId_LopNr = DA90.VtfId_LopNr
LEFT JOIN H_LOS L ON T.VtfId_LopNr = L.VtfId_LopNr
WHERE DX_ORDER = 1
GROUP BY T.LopNr HAVING MIN(sir_adm_time)
)